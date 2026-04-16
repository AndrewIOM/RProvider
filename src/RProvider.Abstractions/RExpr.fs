namespace RProvider.Abstractions

open System.Collections.Generic

/// The root erased type of R expressions within
/// RProvider.
type RExpr = RExpr of obj

module RExpr =
    let wrap (x: obj) = RExpr x
    let unwrap (RExpr x) = x

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

