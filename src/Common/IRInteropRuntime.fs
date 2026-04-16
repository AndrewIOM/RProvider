namespace RProvider.Common

open RProvider
open System

open System
open System.Collections.Generic
open RProvider

/// Helpers to bridge between RExpr and the runtime.
[<AutoOpen>]
module RExprInterop =

    /// Unwrap an RExpr to its underlying object.
    let unwrap (RExpr o) = o

    /// Wrap an object back into RExpr.
    let wrap (o: obj) = RExpr o

    let buildNamedArgsFromDict (vals: IDictionary<string,obj>) : RExpr =
        let dict = Dictionary<string,RExpr>()
        for kv in vals do
            dict.Add(kv.Key, RExpr.wrap kv.Value)
        RExpr.wrap dict

    let buildNamedArgsFromList (vals: (string * obj) list) : RExpr =
        let dict = Dictionary<string,RExpr>()
        for (k,v) in vals do
            dict.Add(k, RExpr.wrap v)
        RExpr.wrap dict

    let buildVarArgs (vals: obj[]) : RExpr =
        vals |> Array.map RExpr.wrap |> RExpr.wrap

    let emptyVarArgs : RExpr =
        RExpr.wrap ([||] : RExpr[])

    /// Expect an RExpr containing a Dictionary<string, RExpr> and convert to seq<KVP<string,obj>>.
    let unwrapNamedArgs (RExpr o) =
        match o with
        | :? IDictionary<string, RExpr> as dict ->
            dict
            |> Seq.map (fun kv -> KeyValuePair(kv.Key, unwrap kv.Value))
        | _ ->
            invalidOp "Expected namedArgs to be an RExpr-wrapped IDictionary<string, RExpr>"

    /// Expect an RExpr containing RExpr[] and convert to obj[].
    let unwrapVarArgs (RExpr o) =
        match o with
        | :? array<RExpr> as arr ->
            arr |> Array.map unwrap
        | _ ->
            invalidOp "Expected varArgs to be an RExpr-wrapped RExpr[]"


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
#if IS_DESIGNTIME
        raise (NotImplementedException("IRInteropRuntime.callFuncByName is design-time only here."))
#else
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
#endif

    static member call
        (env: RExpr)
        (package: string)
        (name: string)
        (serialized: string)
        (namedArgs: RExpr)
        (varArgs: RExpr)
        : RExpr =
        LogFile.logf "call"
#if IS_DESIGNTIME
        raise (NotImplementedException("Design-time stub"))
#else
        // Legacy call ignores env and always uses global env.
        let namedArr =
            unwrapNamedArgs namedArgs
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.toArray

        let vargs = unwrapVarArgs varArgs

        let result =
            RProvider.Runtime.RInterop.call package name serialized namedArr vargs

        wrap (result :> obj)
#endif

    static member globalEnvironment () : RExpr =
        LogFile.logf "globalEnvironment"
#if IS_DESIGNTIME
        raise (NotImplementedException("Design-time stub"))
#else
        let env = RProvider.Runtime.RInterop.globalEnvironment ()
        wrap (env :> obj)
#endif

    static member loadRDataFile (fileName: string) : RExpr =
        LogFile.logf "loadRDataFile"
        raise (NotImplementedException())

    static member getRDataSymbol (envObj: RExpr) (name: string) : RExpr =
        LogFile.logf "getRDataSymbol"
#if IS_DESIGNTIME
        raise (NotImplementedException("Design-time stub"))
#else
        let env =
            match unwrap envObj with
            | :? RBridge.Extensions.REnvironment as e -> e
            | o -> invalidOp $"Expected REnvironment, got {o.GetType().FullName}"

        failwith "not implemented"
        // let value = RProvider.Runtime.RInterop.getRDataSymbol env name
        // wrap (value :> obj)
#endif

    static member getRDataSymbolTyped (envObj: RExpr) (name: string) : obj =
        LogFile.logf "getRDataSymbolTyped"
#if IS_DESIGNTIME
        raise (NotImplementedException("Design-time stub"))
#else
        let env =
            match unwrap envObj with
            | :? RBridge.Extensions.REnvironment as e -> e
            | o -> invalidOp $"Expected REnvironment, got {o.GetType().FullName}"
        failwith "not implemented"
        // RProvider.Runtime.RInterop.getRDataSymbolTyped env name
#endif