namespace RProvider.Runtime

open RBridge.Extensions
open RProvider.Abstractions
open System.Collections.Generic
open RProvider.Common
open RProvider

/// The module represents an R environment loaded from RData file.
/// These functions are typically used through an `RData` type provider. To
/// get a statically typed R environment for a given file, use
/// `RData<"some/path/myfile.rdata">`.
module REnv =

    /// Loads an R data file into a new environment and returns
    /// the environment.
    let loadRDataFile (fileName: string) =
        let env = REnvironment.createEmpty Singletons.engine.Value
        let envSexp: RBridge.SymbolicExpression = { ptr = env.Pointer }
        let argsByName =
            [ "file" , box fileName
              "envir", box envSexp ]
        RInterop.callFuncByName
            env
            "base"
            "load"
            (argsByName |> Seq.map (fun (k,v) -> KeyValuePair(k,v)))
            [||] |> ignore
        env

    /// Get the data symbols and infer their plausable
    /// .NET types using RProvider built-in converters.
    let getDataSymbols env =
        LogFile.logf "TYpe of env in getdatasymbols = %A" (env.GetType())
        let keysExpr =
            RInterop.callFuncByName
                env
                "base"
                "ls"
                (namedParams [ "envir", box env ])
                [||]
        let keys = keysExpr |> Extract.extractStringArray Singletons.engine.Value
        [|
            for k in keys do
                LogFile.logf "GetRDataSymbols: key={%O}" k

                let v =
                    RInterop.callFuncByName
                        env
                        "base"
                        "get"
                        (namedParams [ "x", box k; "envir", box env ])
                        [||]

                let typ = v.TryFromR() |> Option.map(fun b -> b.GetType())
                k, typ
        |]
