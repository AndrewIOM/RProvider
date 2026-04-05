namespace RProvider.Runtime

open RBridge.Extensions
open RProvider.Internal
open RProvider.Internal.RInit

/// Determines symbol names to apply for new bindings
/// in R, and handles execution using the RProvider singletons.
module internal Evaluate =

    let eval (env: REnvironment.REnvironment) (expr: string) =
        Logging.logWithOutput
            Singletons.characterDevice
            (fun () ->
                Logging.logf "eval(%s)" expr
                Evaluate.eval expr env Singletons.engine.Value)

    /// Evaluate an expression, setting the result as the specified symbol name.
    let evalTo env (expr: string) (symbol: string) device engine =
        Logging.logWithOutput
            Singletons.characterDevice
            (fun () ->
                Logging.logf "evalto(%s, %s)" expr symbol
                Symbol.setSymbol symbol (Evaluate.eval expr env engine)
            )

    let exec (env:REnvironment.REnvironment) (expr: string) : unit =
        Logging.logWithOutput
            Singletons.characterDevice
            (fun () ->
                Logging.logf "exec(%s)" expr
                // TODO below used use instead of let. IDisposable?
                let res = eval env expr
                ())


module internal Call =

    open RBridge
    open System.Collections.Generic
    open RProvider.Serialise

    /// Given an R environment scope, call a function given the
    /// named and unnamed arguments. 
    let callFunc
        convertToR
        (rEnv: REnvironment.REnvironment) 
        (fn: SymbolicExpression)
        (argsByName: seq<KeyValuePair<string, obj>>)
        (varArgs: obj [])
        : SymbolicExpression =

        let namedArgs =
            argsByName
            |> Seq.map (fun kvp -> kvp.Key, convertToR Singletons.engine.Value kvp.Value)
            |> Seq.toList

        let unnamedArgsOrdered =
            if isNull varArgs then
                []
            else
                varArgs
                |> Array.toList
                |> List.map (fun v -> "", convertToR Singletons.engine.Value v)

        let allArgs = namedArgs @ unnamedArgsOrdered
        let argsPairlist = PairList.build Singletons.engine.Value allArgs
        Evaluate.call rEnv fn argsPairlist Singletons.engine.Value

    /// Call an R function by name given a function name.
    let callFuncByName
        convertToR
        (rEnv: REnvironment.REnvironment)
        (packageName: string)
        (funcName: string)
        (namedArgs: seq<KeyValuePair<string,obj>>)
        (varArgs: obj array)
        : SymbolicExpression =

        let nsEnv = REnvironment.ofNamespace Singletons.engine.Value packageName
        let fn =
            REnvironment.tryGetValue Singletons.engine.Value nsEnv funcName
            |> Option.defaultWith (fun () ->
                failwithf "Function %s not found in namespace %s" funcName packageName)

        callFunc convertToR rEnv fn namedArgs varArgs

    /// Call an R function given arguments, using serialised values (i.e.
    /// from the type provider itself over a socket).
    /// Always uses the global environment.
    let call
        convertToR
        (packageName: string)
        (funcName: string)
        (serializedRVal: string)
        (namedArgs: obj [])
        (varArgs: obj [])
        : SymbolicExpression =

        match deserializeRValue serializedRVal with
        | RValue.Function (paramsR, hasVarArg) ->

            if namedArgs.Length <> paramsR.Length then
                failwithf "Function %s expects %d named arguments and you supplied %d" funcName paramsR.Length namedArgs.Length

            let argsByName =
                Seq.zip paramsR namedArgs
                |> Seq.map (fun (n, v) -> KeyValuePair(n, v))

            let gEnv = REnvironment.globalEnv Singletons.engine.Value
            callFuncByName convertToR gEnv packageName funcName argsByName varArgs

        | RValue.Value ->
            let nsEnv = REnvironment.ofNamespace Singletons.engine.Value packageName
            REnvironment.tryGetValue Singletons.engine.Value nsEnv funcName
            |> Option.defaultWith (fun () -> failwithf "Value %s not found" funcName)
