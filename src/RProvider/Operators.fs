namespace RProvider

open RProvider.Runtime
open RBridge.Extensions
open RProvider.Abstractions

/// Custom operators that make composing and working with
/// R symbolic expressions easier.
module Operators =

    /// Opens a dynamic property of an R expression.
    /// Supports named lists, S4 objects, and dataframes.
    /// If a dataframe, the column is extracted by name.
    let op_Dynamic (expr: RExpr) (mem: string) =
        try
            let unwrapped = RExprWrapper.toRBridge expr
            let forced = RInterop.callFuncByName (RInterop.globalEnvironment()) "base" "eval" [] [| expr |]
            match forced with
            | ActivePatterns.S4Object Singletons.engine.Value s4Class -> SymbolicExpression.trySlot mem unwrapped |> Option.get
            | ActivePatterns.DataFrame Singletons.engine.Value df -> SymbolicExpression.column mem df
            | expr -> expr.Member mem
            |> RExprWrapper.toRProvider
        with
        | :? System.ArgumentOutOfRangeException ->
            RExprWrapper.toRProvider { ptr = Singletons.engine.Value.invoke(fun e -> e.Api.nilValue) }

    /// When calling an R function, use the => operator in a list
    /// to set a parameter: [ "someparam" => 2 ]
    let (=>) (key: string) (value: 'a) = key, box value
