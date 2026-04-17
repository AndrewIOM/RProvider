namespace RProvider.Abstractions

open System.Collections.Generic

/// An expression (function, value etc.) in R.
/// It is the root erased type of R expressions within
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
