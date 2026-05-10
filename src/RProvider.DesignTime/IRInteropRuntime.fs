namespace RProvider.Runtime

open System
open RProvider.Abstractions

/// A stub layer of runtime functions for use in
/// design-time quotations.
type IRInteropRuntime =
    
    /// Call an R function by name with named and varargs.
    static member callFuncByName
        (env: RExpr)
        (package: string)
        (name: string)
        (namedArgs: (string * obj)[])
        (varArgs: obj[])
        : RExpr =
        raise (NotImplementedException "Design-time stub")

    static member call
        (env: RExpr)
        (package: string)
        (name: string)
        (serialized: string)
        (namedArgs: (string * obj)[])
        (varArgs: obj[])
        : RExpr =
        raise (NotImplementedException "Design-time stub")

    static member getValue (package: string) (name: string) : RExpr =
        raise (NotImplementedException "Design-time stub")

    static member globalEnvironment () : RExpr =
        raise (NotImplementedException "Design-time stub")

    static member loadRDataFile (fileName: string) : RData =
        raise (NotImplementedException())

    static member getRDataSymbol (env: RData) (name: string) : RExpr =
        raise (NotImplementedException "Design-time stub")

    static member getRDataSymbolTyped<'T> (env: RData) (name: string) : 'T =
        raise (NotImplementedException "Design-time stub")
