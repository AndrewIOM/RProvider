namespace RProvider.Abstractions

/// An raw R expression represented by a pointer.
type RSymbolicExpr = { ptr: nativeint }

/// An expression (function, value etc.) in R.
/// It is the root erased type of R expressions within
/// RProvider.
type RExpr = RExpr of RSymbolicExpr

/// Helpers to bridge between RExpr and the runtime.
module RExpr =

    /// Unwrap an RExpr to its underlying object.
    let unwrap (RExpr o) = o

    /// Wrap an object back into RExpr.
    let wrap (o: RSymbolicExpr) = RExpr o
