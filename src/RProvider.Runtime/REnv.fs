namespace RProvider

open RBridge.Extensions
open RProvider.Internal.RInit

/// The object represents an R environment loaded from RData file.
/// This type is typically used through an `RData` type provider. To
/// get a statically typed R environment for a given file, use
/// `RData<"some/path/myfile.rdata">`.
type REnv(fileName: string) =
    let env = REnvironment.createEmpty Singletons.engine.Value
    do RInterop.callFuncByName env "base" "load" (namedParams [ "file", box fileName; "envir", box env ]) [||] |> ignore

    /// Returns the underlying R environment, represented as `REnvironment`
    member _.Environment = env

    /// Get a value from the R environment as `SymbolicExpression`
    /// (This is equivalent to calling `R.get` function)
    member _.Get(name: string) = RInterop.callFuncByName env "base" "get" (namedParams [ "x", box name; "envir", box env ]) [||]

    /// Returns the keys of all values available in the environment
    /// (This is equivalent to calling `R.ls` function)
    member _.Keys : string [] =
        let ls = RInterop.callFuncByName env "base" "ls" (namedParams [ "envir", box env ]) [||]
        ls.FromR<string[]>()
