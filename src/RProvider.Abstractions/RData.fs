namespace RProvider.Abstractions

/// An R data file or store.
type RData = RData of RExpr

/// Helpers to bridge between RExpr and the runtime.
module RData =

    /// Unwrap an RData to its underlying object.
    let unwrap (RData o) = o

    /// Wrap an object back into RData.
    let wrap (o: RExpr) = RData o
