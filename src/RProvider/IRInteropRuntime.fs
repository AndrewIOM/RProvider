namespace RProvider.Runtime

open System
open RProvider.Abstractions
open RProvider.Common

/// The binding layer between the design-time quotations
/// and the runtime implementations.
type IRInteropRuntime =

    /// Call an R function by name with named and varargs.
    static member callFuncByName
        (env: RExpr)
        (package: string)
        (name: string)
        (namedArgs: (string * obj) [])
        (varArgs: obj [])
        : RExpr =
        LogFile.logf "callFuncByName"
        // Runtime implementation:
        let rEnvExpr = RExprWrapper.toRBridge env

        let rEnv =
            match RBridge.Extensions.REnvironment.ofSExp Singletons.engine.Value rEnvExpr with
            | Some e -> e
            | o -> invalidOp $"Expected REnvironment in env RExpr, got {o.GetType().FullName}"

        let named = namedArgs |> Seq.map (fun (k, v) -> Collections.Generic.KeyValuePair(k, v))

        let result = RProvider.Runtime.RInterop.callFuncByName rEnv package name named varArgs

        result |> RExprWrapper.toRProvider

    static member call
        (env: RExpr)
        (package: string)
        (name: string)
        (serialized: string)
        (namedArgs: (string * obj) [])
        (varArgs: obj [])
        : RExpr =
        LogFile.logf "call"
        // Legacy call ignores env and always uses global env.
        let named = namedArgs |> Seq.map snd |> Seq.toArray
        let result = RProvider.Runtime.RInterop.call package name serialized named varArgs
        result |> RExprWrapper.toRProvider

    static member globalEnvironment() : RExpr =
        LogFile.logf "globalEnvironment"
        let env = RProvider.Runtime.RInterop.globalEnvironment ()
        let sexp: RBridge.SymbolicExpression = { ptr = env.Pointer }
        sexp |> RExprWrapper.toRProvider

    static member loadRDataFile(fileName: string) : RProvider.Abstractions.RData =
        LogFile.logf "loadRDataFile"
        let sexp = REnv.loadRDataFile fileName
        { ptr = sexp.Pointer } |> RExpr.wrap |> RData.wrap

    static member getRDataSymbol (env: RData) (name: string) : RExpr =
        LogFile.logf "getRDataSymbol"
        let rEnv = env |> RData.unwrap |> RExprWrapper.toRBridge |> RBridge.Extensions.REnvironment.ofSExp Singletons.engine.Value |> Option.get
        let sexp = RInterop.tryGetValue Singletons.engine.Value rEnv name |> Option.get
        sexp |> RExprWrapper.toRProvider

    static member getRDataSymbolTyped<'T> (env: RData) (name: string) : 'T =
        LogFile.logf "getRDataSymbolTyped"
        let rEnv = env |> RData.unwrap |> RExprWrapper.toRBridge |> RBridge.Extensions.REnvironment.ofSExp Singletons.engine.Value |> Option.get
        let sexp = RInterop.tryGetValue Singletons.engine.Value rEnv name |> Option.get
        Convert.tryFromRStructural Singletons.engine.Value sexp
        |> Option.defaultWith(fun _ -> failwithf "Could not get typed version of symbol [%s]." typeof<'T>.Name)
