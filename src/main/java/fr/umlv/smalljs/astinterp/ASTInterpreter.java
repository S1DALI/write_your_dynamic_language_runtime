package fr.umlv.smalljs.astinterp;

import fr.umlv.smalljs.ast.Expr;
import fr.umlv.smalljs.ast.Expr.Block;
import fr.umlv.smalljs.ast.Expr.Call;
import fr.umlv.smalljs.ast.Expr.FieldAccess;
import fr.umlv.smalljs.ast.Expr.FieldAssignment;
import fr.umlv.smalljs.ast.Expr.Fun;
import fr.umlv.smalljs.ast.Expr.Identifier;
import fr.umlv.smalljs.ast.Expr.If;
import fr.umlv.smalljs.ast.Expr.Literal;
import fr.umlv.smalljs.ast.Expr.MethodCall;
import fr.umlv.smalljs.ast.Expr.ObjectLiteral;
import fr.umlv.smalljs.ast.Expr.Return;
import fr.umlv.smalljs.ast.Expr.VarAssignment;
import fr.umlv.smalljs.ast.Script;
import fr.umlv.smalljs.rt.Failure;
import fr.umlv.smalljs.rt.JSObject;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static fr.umlv.smalljs.rt.JSObject.UNDEFINED;
import static java.util.stream.Collectors.joining;

public final class ASTInterpreter {
  private static JSObject asJSObject(Object value, int lineNumber) {
    if (!(value instanceof JSObject jsObject)) {
      throw new Failure("at line " + lineNumber + ", type error " + value + " is not a JSObject");
    }
    return jsObject;
  }

  private static Object execute(Expr.Block body, JSObject env) {
    // initialize declared variables to UNDEFINED
    visitVariable(body, env);
    // interpret the AST
    return visit(body, env);
  }

  private static void visitVariable(Expr expression, JSObject env) {
    switch (expression) {
      case Block(List<Expr> exprs, _) -> {
        for (var expr : exprs) {
          visitVariable(expr, env);
        }
      }
      case VarAssignment(String name, _, boolean declaration, _) -> {
        if (declaration) {
          env.register(name, UNDEFINED);
        }
      }
      case If(_, Block trueBlock, Block falseBlock, _) -> {
        visitVariable(trueBlock, env);
        visitVariable(falseBlock, env);
      }
      case Literal _, Call _, Identifier _, Fun _, Return _, ObjectLiteral _, FieldAccess _,
           FieldAssignment _, MethodCall _ -> {
        // do nothing
      }
    };
  }

  static Object visit(Expr expression, JSObject env) {
    return switch (expression) {
      case Block(List<Expr> exprs, int lineNumber) -> {
          for (var v : exprs) {
              visit(v, env);
          }
          // TODO loop over all instructions
        yield UNDEFINED;
      }
      case Literal(Object value, int lineNumber) -> {
        yield value;
      }

      case Call(Expr qualifier, List<Expr> args, int lineNumber) -> {
          var isFunction = visit(qualifier, env);
          if(!(isFunction instanceof JSObject function)){
            throw new Failure("Not a function : " + isFunction + " at line " + lineNumber);
          }
          var arguments = args.stream().map(x -> visit(x, env)).toArray();

          yield function.invoke(UNDEFINED, arguments);
      }

      case Identifier(String name, int lineNumber) -> {
          var value = env.lookupOrDefault(name, null);

          if(value == null){
              throw new Failure("Underfined variable " + name + " at line " + lineNumber);
          }
          yield value;
      }

      case VarAssignment(String name, Expr expr, boolean declaration, int lineNumber) -> {
          var result = env.lookupOrDefault(name, null);

          if(!declaration && result == null){
              throw new Failure("Undefined variable " + name + " at line " + lineNumber);
          }

          var visited = visit(expr, env);
          env.register(name, visited);
          yield visited;
      }

      case Fun(String name, List<String> parameters, boolean toplevel, Block body, int lineNumber) -> {
          JSObject.Invoker invoker = new JSObject.Invoker() {
              @Override
              public Object invoke(Object receiver, Object... args) {
                if(parameters.size() != args.length){
                    throw new Failure("Wrong number of arguments for " + name);
                }
                var newEnv = JSObject.newEnv(env);
                newEnv.register("this", receiver);
                for(int i = 0; i < parameters.size(); i++){
                    newEnv.register(parameters.get(i), args[i]);
                }
                try{
                    execute(body, newEnv);
                }catch(ReturnError e){
                    return e.getValue();
                }
                return UNDEFINED;
              }
          };

          // create the JS function with the invoker
          var fun = JSObject.newFunction(name, invoker);
          // register it into the global env if it's a toplevel
          if(toplevel){
            env.register(name, fun);
          }// yield the function
          yield fun;
      }

      case Return(Expr expr, int lineNumber) -> {
          var value = visit(expr, env);
          throw new ReturnError(value);
      }

      case If(Expr condition, Block trueBlock, Block falseBlock, int lineNumber) -> {
        var value = visit(condition, env);

        if (value == null || value == UNDEFINED){
            visit(falseBlock, env);
        }
        else if (value instanceof Integer integer){
            if(integer == 1){
                visit(trueBlock, env);
            }else{ // 0
                visit(falseBlock, env);
            }
        }
        yield UNDEFINED; // if ne renvoie rieeeeeeeeeeeeeeen
      }

      case ObjectLiteral(Map<String, Expr> initMap, int lineNumber) -> {
        var obj = JSObject.newObject(null);
        for (var entry : initMap.entrySet()) {
            var key = entry.getKey();
            var value = visit(entry.getValue(), env);
            obj.register(key, value);
        }
        yield obj;
      }

      case FieldAccess(Expr receiver, String name, int lineNumber) -> {
        var value = visit(receiver, env);

        if (!(value instanceof JSObject object)) {
            throw new Failure("at line " + lineNumber + ", " + value + " is not an object");
        }

        yield object.lookupOrDefault(name, UNDEFINED);
      }

      case FieldAssignment(Expr receiver, String name, Expr expr, int lineNumber) -> {
        var value = visit(receiver, env);
        if (!(value instanceof JSObject jsObj)) {
          throw new Failure("at line " + lineNumber + ", " + value + " is not an object");
        }
        var result = visit(expr, env);
        jsObj.register(name, result);
        yield result;
      }
      case MethodCall(Expr receiver, String name, List<Expr> args, int lineNumber) -> {
          var value = visit(receiver, env);

          if (!(value instanceof JSObject jsObj)) {
              throw new Failure("at line " + lineNumber + ", " + value + " is not an object");
          }

          var result = jsObj.lookupOrDefault(name, UNDEFINED);

          if (!(result instanceof JSObject func)) {
              throw new Failure("at line " + lineNumber + ", " + name + " is not a function");
          }

          var arguments = args.stream().map(arg -> visit(arg, env)).toArray();

          yield func.invoke(jsObj, arguments);
      }
    };
  }


  @SuppressWarnings("unchecked")
  private static JSObject createGlobalEnv(PrintStream outStream) {
    var globalEnv = JSObject.newEnv(null);
    globalEnv.register("globalThis", globalEnv);
    globalEnv.register("print", JSObject.newFunction("print", (_, args) -> {
      System.err.println("print called with " + Arrays.toString(args));
      outStream.println(Arrays.stream(args).map(Object::toString).collect(Collectors.joining(" ")));
      return UNDEFINED;
    }));
    globalEnv.register("+", JSObject.newFunction("+", (_, args) -> (Integer) args[0] + (Integer) args[1]));
    globalEnv.register("-", JSObject.newFunction("-", (_, args) -> (Integer) args[0] - (Integer) args[1]));
    globalEnv.register("/", JSObject.newFunction("/", (_, args) -> (Integer) args[0] / (Integer) args[1]));
    globalEnv.register("*", JSObject.newFunction("*", (_, args) -> (Integer) args[0] * (Integer) args[1]));
    globalEnv.register("%", JSObject.newFunction("%", (_, args) -> (Integer) args[0] % (Integer) args[1]));
    globalEnv.register("==", JSObject.newFunction("==", (_, args) -> args[0].equals(args[1]) ? 1 : 0));
    globalEnv.register("!=", JSObject.newFunction("!=", (_, args) -> !args[0].equals(args[1]) ? 1 : 0));
    globalEnv.register("<", JSObject.newFunction("<", (_, args) -> (((Comparable<Object>) args[0]).compareTo(args[1]) < 0) ? 1 : 0));
    globalEnv.register("<=", JSObject.newFunction("<=", (_, args) -> (((Comparable<Object>) args[0]).compareTo(args[1]) <= 0) ? 1 : 0));
    globalEnv.register(">", JSObject.newFunction(">", (_, args) -> (((Comparable<Object>) args[0]).compareTo(args[1]) > 0) ? 1 : 0));
    globalEnv.register(">=", JSObject.newFunction(">=", (_, args) -> (((Comparable<Object>) args[0]).compareTo(args[1]) >= 0) ? 1 : 0));
    return globalEnv;
  }

  public static void interpret(Script script, PrintStream outStream) {
    var globalEnv = createGlobalEnv(outStream);
    var body = script.body();
    execute(body, globalEnv);
  }
}

