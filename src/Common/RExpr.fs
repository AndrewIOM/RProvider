namespace RProvider.Common

/// The root erased type of R expressions within
/// RProvider.
type RExpr = private RExpr of obj

module RExpr =
    let wrap (x: obj) = RExpr x
    let unwrap (RExpr x) = x
