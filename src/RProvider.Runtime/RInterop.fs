namespace RProvider

open System.Collections.Generic
open RBridge
open RBridge.Extensions
open RBridge.Extensions.ActivePatterns
open RProvider.Internal
open RProvider.Internal.RInit
open RProvider.Serialise
open RProvider.Runtime

/// [omit]
/// The layer that the type provider accesses to
/// interop with R.
module RInterop =

    /// Replace dots in R names with underscores, making
    /// them safe for use as .NET type names.
    let makeSafeName (name: string) = name.Replace("_", "__").Replace(".", "_")

    /// List packages available in the loaded R instance.
    let getPackages () : string [] =
        Logging.logf "Communicating with R to get packages"
        let globEnv = REnvironment.globalEnv Singletons.engine.Value
        let res =
            match Evaluate.eval globEnv ".packages(all.available=T)" with
            | CharacterVector Singletons.engine.Value v -> v |> Extract.extractStringArray Singletons.engine.Value
            | _ -> failwith "Unexpected result getting packages"
        Logging.logf "Result: %O" res
        res

    /// Get the description for a particular package from R.
    let getPackageDescription packageName : string =
        let globEnv = REnvironment.globalEnv Singletons.engine.Value
        Evaluate.eval globEnv ("packageDescription(\"" + packageName + "\")$Description")
        |> SymbolicExpression.getValue

    /// Read a package's metadata to extract descriptions
    /// of each user-facing function.
    let getFunctionDescriptions packageName =
        let globEnv = REnvironment.globalEnv Singletons.engine.Value
        Evaluate.exec globEnv <| sprintf """rds = readRDS(system.file("Meta", "Rd.rds", package = "%s"))""" packageName
        Array.zip ((Evaluate.eval globEnv "rds$Name").FromR<string []>()) ((Evaluate.eval globEnv "rds$Title").FromR<string []>())

    let loadPackage packageName : unit =
        if not (Singletons.loadedPackages.Contains packageName) then
            let globalEnv = REnvironment.globalEnv Singletons.engine.Value
            let result = Evaluate.eval globalEnv ("require(" + packageName + ")")
            match result.FromR<bool option>() with
            | Some res ->
                if not res then
                    failwithf "Package %s not installed" packageName
            | None -> failwithf "Loading package %s failed" packageName
            Singletons.loadedPackages.Add packageName |> ignore

    /// Determines whether an expression is a value or a function.
    /// If a function, determines which arguments are available.
    let internal bindingInfo sexp : RValue =

        match sexp with
        | Closure Singletons.engine.Value clos ->

            let formals = Function.getFormals Singletons.engine.Value clos
            let names =
                Attributes.tryNames Singletons.engine.Value formals
                |> Option.defaultValue [||]
                |> Array.toList

            let hasVarArgs = names |> List.contains "..."
            let args = names |> List.filter ((<>) "...")
            RValue.Function(args, hasVarArgs)

        | ActivePatterns.BuiltIn Singletons.engine.Value _ 
        | ActivePatterns.Special Singletons.engine.Value _ ->
            // Don't know how to reflect on builtin or special args so just do as varargs
            RValue.Function([], true)

        | RealVector Singletons.engine.Value _
        | CharacterVector Singletons.engine.Value _
        | LogicalVector Singletons.engine.Value _
        | IntegerVector Singletons.engine.Value _
        | ComplexVector Singletons.engine.Value _
        | RawVector Singletons.engine.Value _
        | List Singletons.engine.Value _ ->
            RValue.Value

        | _ ->
            Logging.logf "Ignoring name of unknown SEXP type"
            RValue.Value

    /// Get bindings representing 
    let getBindings (packageName: string) =

        // In R, a namespace is an environment
        let nsEnv = REnvironment.ofNamespace Singletons.engine.Value packageName

        let names =
            Evaluate.eval nsEnv "ls(all.names=TRUE)"
            |> Extract.extractStringArray Singletons.engine.Value

        names
        |> Array.choose (fun name ->
            match REnvironment.tryGetValue Singletons.engine.Value nsEnv name with
            | None -> None
            | Some sexp ->
                let info = bindingInfo sexp
                Some (name, Serialise.serializeRValue info)
        )

    let globalEnvironment () =
        REnvironment.globalEnv Singletons.engine.Value

    /// Given an R environment scope, call a function given the
    /// named and unnamed arguments. 
    let callFunc
        (rEnv: REnvironment) 
        (fn: SymbolicExpression)
        (argsByName: seq<KeyValuePair<string, obj>>)
        (varArgs: obj [])
        : SymbolicExpression =

        Call.callFunc Converters.Convert.toR rEnv fn argsByName varArgs

    /// Call an R function by name given a function name.
    let callFuncByName
        (rEnv: REnvironment)
        (packageName: string)
        (funcName: string)
        (namedArgs: seq<KeyValuePair<string,obj>>)
        (varArgs: obj array)
        : SymbolicExpression =
        
        Call.callFuncByName Converters.Convert.toR rEnv packageName funcName namedArgs varArgs

    /// Call an R function given arguments, using serialised values (i.e.
    /// from the type provider itself over a socket).
    /// Always uses the global environment.
    let call
        (packageName: string)
        (funcName: string)
        (serializedRVal: string)
        (namedArgs: obj [])
        (varArgs: obj [])
        : SymbolicExpression =

        Call.call Converters.Convert.toR packageName funcName serializedRVal namedArgs varArgs


/// [omit]
/// Adds extensions to R symbolic expressions to support
/// printing using R's internal print() function.
[<AutoOpen>]
module SymbolicExpressionPrintExtensions =

    // Print by capturing the output in a registered character device
    let internal printUsingDevice print =
        Singletons.characterDevice <- Devices.Intercept.beginCapture Singletons.characterDevice
        print ()
        let result = Devices.Intercept.endCapture Singletons.characterDevice
        Singletons.characterDevice <- snd result
        fst result

    /// Print by redirecting the output to a temp file (on Mono/Mac,
    /// using character device hangs the R provider for some reason).
    /// TODO Can this be removed now on .NET 10+?
    let internal printUsingTempFile print =
        let temp = System.IO.Path.GetTempFileName()

        try
            let rvalStr = Function([ "file" ], true) |> serializeRValue
            RInterop.call "base" "sink" rvalStr [| temp |] [||] |> ignore
            print ()
            RInterop.call "base" "sink" rvalStr [||] [||] |> ignore
            System.IO.File.ReadAllText temp
        finally
            System.IO.File.Delete temp


    type SymbolicExpression with
        /// Call the R print function and return output as a string
        member this.Print() : string =

            let capturer = if Configuration.isUnixOrMac () then printUsingTempFile else printUsingDevice

            capturer
                (fun () ->
                    let rvalStr = RValue.Function([ "x" ], true) |> Serialise.serializeRValue
                    RInterop.call "base" "print" rvalStr [| this |] [||] |> ignore)


/// Contains helper functions for calling the functions generated by the R provider,
/// such as the `namedParams` function for specifying named parameters.
/// The module is automatically opened when you open the `RProvider` namespace.
[<AutoOpen>]
module Helpers =

    /// Construct a dictionary of named params to pass to an R function.
    ///
    /// ## Example
    /// For example, if you want to call the `R.plot` function with named parameters
    /// specifying `x`, `type`, `col` and `ylim`, you can use the following:
    ///
    ///     [ "x", box widgets;
    ///       "type", box "o";
    ///       "col", box "blue";
    ///       "ylim", box [0; 25] ]
    ///     |> namedParams |> R.plot
    ///
    let namedParams (s: seq<string * _>) = dict <| Seq.map (fun (n, v) -> n, box v) s


/// Custom operators that make composing and working with
/// R symbolic expressions easier.
module Operators =

    /// Opens a dynamic property of an R symbolic expression.
    /// Supports named lists, S4 objects, and dataframes.
    /// If a dataframe, the column is extracted by name.
    let op_Dynamic (expr:SymbolicExpression, mem:string) =
        try
            match expr with
            | ActivePatterns.S4Object Singletons.engine.Value s4 ->
                SymbolicExpression.trySlot mem s4 |> Option.get
            | ActivePatterns.DataFrame Singletons.engine.Value _ ->
                SymbolicExpression.column mem expr
            | _ -> expr.Member mem
        with 
        | :? System.ArgumentOutOfRangeException -> { ptr = Singletons.engine.Value.Api.nilValue }

    /// When calling an R function, use the => operator in a list
    /// to set a parameter: [ "someparam" => 2 ]
    let (=>) (key:string) (value:'a) = (key, box value)
