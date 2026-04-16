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
        (namedArgs: RExpr)
        (varArgs: RExpr)
        : RExpr =
        LogFile.logf "callFuncByName"
        // Runtime implementation:
        let rEnv =
            match unwrap env with
            | :? RBridge.Extensions.REnvironment as e -> e
            | o -> invalidOp $"Expected REnvironment in env RExpr, got {o.GetType().FullName}"

        let named = unwrapNamedArgs namedArgs
        let vargs = unwrapVarArgs varArgs

        let result =
            RProvider.Runtime.RInterop.callFuncByName rEnv package name named vargs

        wrap (result :> obj)

    static member call
        (env: RExpr)
        (package: string)
        (name: string)
        (serialized: string)
        (namedArgs: RExpr)
        (varArgs: RExpr)
        : RExpr =
        LogFile.logf "call"
        // Legacy call ignores env and always uses global env.
        let namedArr =
            unwrapNamedArgs namedArgs
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.toArray

        let vargs = unwrapVarArgs varArgs

        let result =
            RProvider.Runtime.RInterop.call package name serialized namedArr vargs

        wrap (result :> obj)

    static member globalEnvironment () : RExpr =
        LogFile.logf "globalEnvironment"
        let env = RProvider.Runtime.RInterop.globalEnvironment ()
        wrap (env :> obj)

    static member loadRDataFile (fileName: string) : RExpr =
        LogFile.logf "loadRDataFile"
        raise (NotImplementedException())

    static member getRDataSymbol (envObj: RExpr) (name: string) : RExpr =
        LogFile.logf "getRDataSymbol"
        let env =
            match unwrap envObj with
            | :? RBridge.Extensions.REnvironment as e -> e
            | o -> invalidOp $"Expected REnvironment, got {o.GetType().FullName}"

        failwith "not implemented"
        // let value = RProvider.Runtime.RInterop.getRDataSymbol env name
        // wrap (value :> obj)

    static member getRDataSymbolTyped (envObj: RExpr) (name: string) : obj =
        LogFile.logf "getRDataSymbolTyped"
        let env =
            match unwrap envObj with
            | :? RBridge.Extensions.REnvironment as e -> e
            | o -> invalidOp $"Expected REnvironment, got {o.GetType().FullName}"
        failwith "not implemented"
        // RProvider.Runtime.RInterop.getRDataSymbolTyped env name
