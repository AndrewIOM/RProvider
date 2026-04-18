namespace RProvider.Runtime

open RBridge.Extensions
open RProvider.Common

/// Determines symbol names to apply for new bindings
/// in R, and handles execution using the RProvider singletons.
module internal Evaluate =

    let eval (env: REnvironment) (expr: string) =
        LogFile.logf "eval(%s)" expr
        Evaluate.tryEval expr env Singletons.engine.Value

    /// Evaluate an expression, setting the result as the specified symbol name.
    let evalTo env (expr: string) (symbol: string) engine =
        LogFile.logf "evalto(%s, %s)" expr symbol
        Evaluate.tryEval expr env engine |> Result.map (fun v -> Symbol.setSymbol symbol v env engine)

    let exec (env: REnvironment) (expr: string) : Result<unit, string> =
        LogFile.logf "exec(%s)" expr
        eval env expr |> Result.map (fun _ -> ())


module internal Call =

    open RBridge
    open System.Collections.Generic
    open RProvider.Common.Serialisation

    /// Given an R environment scope, call a function given the
    /// named and unnamed arguments.
    let callFunc
        convertToR
        (rEnv: REnvironment)
        (fn: SymbolicExpression)
        (argsByName: seq<KeyValuePair<string, obj>>)
        (varArgs: obj [])
        : Result<SymbolicExpression, string> =

        let namedArgs =
            argsByName |> Seq.map (fun kvp -> Some kvp.Key, convertToR Singletons.engine.Value kvp.Value) |> Seq.toList

        let unnamedArgsOrdered =
            if isNull varArgs then
                []
            else
                varArgs |> Array.toList |> List.map (fun v -> None, convertToR Singletons.engine.Value v)

        let allArgs = namedArgs @ unnamedArgsOrdered
        Evaluate.tryCall rEnv fn allArgs Singletons.engine.Value

    /// Call an R function by name given a function name.
    let callFuncByName
        convertToR
        (rEnv: REnvironment)
        (packageName: string)
        (funcName: string)
        (namedArgs: seq<KeyValuePair<string, obj>>)
        (varArgs: obj array)
        : Result<SymbolicExpression, string> =

        let nsEnv = REnvironment.ofNamespace Singletons.engine.Value packageName

        let fn =
            REnvironment.tryGetValue Singletons.engine.Value nsEnv funcName
            |> Option.defaultWith (fun () -> failwithf "Function %s not found in namespace %s" funcName packageName)
            |> Promise.force Singletons.engine.Value

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
        : Result<SymbolicExpression, string> =

        match deserializeRValue serializedRVal with
        | RValue.Function (paramsR, hasVarArg) ->

            if namedArgs.Length <> paramsR.Length then
                failwithf
                    "Function %s expects %d named arguments and you supplied %d"
                    funcName
                    paramsR.Length
                    namedArgs.Length

            let argsByName = Seq.zip paramsR namedArgs |> Seq.map (fun (n, v) -> KeyValuePair(n, v))

            let gEnv = REnvironment.globalEnv Singletons.engine.Value
            callFuncByName convertToR gEnv packageName funcName argsByName varArgs

        | RValue.Value ->
            let nsEnv = REnvironment.ofNamespace Singletons.engine.Value packageName

            match REnvironment.tryGetValue Singletons.engine.Value nsEnv funcName with
            | None -> Error(sprintf "Value %s not found" funcName)
            | Some v -> v |> Promise.force Singletons.engine.Value |> Ok
