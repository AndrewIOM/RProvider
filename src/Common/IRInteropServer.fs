namespace RProvider.Common

/// Interface that is used for communication between the R provider server
/// (RProvider.Server.exe) which communicates with R and the client that runs
/// in the host IDE process (Visual Studio, Xamarin Studio etc.)
///
/// NOTE: In order to support standalone compilation of `RProvider.Server.exe` (which
/// inlines the F# Core library), the interface does not expose any F# Core types.
type IRInteropServer =

    /// If there was an initialization error when loading R provider, this
    /// string returns the error. Otherwise, the value is `null`.
    abstract member InitializationErrorMessage: unit -> string

    /// Returns an array with the names of all installed packages (e.g. "base", "graphics" etc.)
    abstract member GetPackages: unit -> string []
    /// Loads the package (using R's `require`). This should be called before `GetBindings`.
    abstract member LoadPackage: string -> unit
    /// Returns an array with binding information. The first string is the name of the
    /// function. The second string is serialized `RValue` with information about the
    /// kind of the binding and function parameters (use `deserializeRValue`).
    abstract member GetBindings: string -> (string * string) []

    /// Returns an array with pairs consisting of function name and its description
    abstract member GetFunctionDescriptions: string -> (string * string) []
    /// Returns the description (documentation) for a given package
    abstract member GetPackageDescription: string -> string

    /// Given an `.rdata` file, returns the names of the symbols in the file, together
    /// with an F# type that it can be converted to (this is done by getting the type
    /// of `symExpr.Value` using currently installed convertors). If the type is not
    /// available, this returns `null`.
    abstract member GetRDataSymbols: string -> (string * option<System.Type>) []

module InteropServer =

    open System.IO

    type ServerRequest =
        | InitializationErrorMessage
        | GetPackages
        | LoadPackage of string
        | GetBindings of string
        | GetFunctionDescriptions of string
        | GetPackageDescription of string
        | GetRDataSymbols of string

    type ServerResponse =
        | InitializationErrorMessageResult of string
        | Packages of string[]
        | UnitResult
        | Bindings of (string * string)[]
        | FunctionDescriptions of (string * string)[]
        | PackageDescription of string
        | RDataSymbols of (string * option<System.Type>)[]
        | ServerError of string

    module Request =

        let write (w: BinaryWriter) (req: ServerRequest) =
            match req with
            | InitializationErrorMessage ->
                w.Write "InitializationErrorMessage"

            | GetPackages ->
                w.Write "GetPackages"

            | LoadPackage pkg ->
                w.Write "LoadPackage"
                w.Write pkg

            | GetBindings pkg ->
                w.Write "GetBindings"
                w.Write pkg

            | GetFunctionDescriptions pkg ->
                w.Write "GetFunctionDescriptions"
                w.Write pkg

            | GetPackageDescription pkg ->
                w.Write "GetPackageDescription"
                w.Write pkg

            | GetRDataSymbols path ->
                w.Write "GetRDataSymbols"
                w.Write path

        let read (r: BinaryReader) : ServerRequest =
            match r.ReadString() with
            | "InitializationErrorMessage" -> InitializationErrorMessage
            | "GetPackages" -> GetPackages
            | "LoadPackage" -> LoadPackage (r.ReadString())
            | "GetBindings" -> GetBindings (r.ReadString())
            | "GetFunctionDescriptions" -> GetFunctionDescriptions (r.ReadString())
            | "GetPackageDescription" -> GetPackageDescription (r.ReadString())
            | "GetRDataSymbols" -> GetRDataSymbols (r.ReadString())
            | other -> failwithf "Unknown ServerRequest case: %s" other

    module Response =

        let write (w: BinaryWriter) (resp: ServerResponse) =
            match resp with

            | InitializationErrorMessageResult s ->
                w.Write "InitializationErrorMessageResult"
                w.Write s

            | Packages pkgs ->
                w.Write "Packages"
                w.Write pkgs.Length
                for p in pkgs do
                    w.Write p

            | UnitResult ->
                w.Write "UnitResult"

            | Bindings arr ->
                w.Write "Bindings"
                w.Write arr.Length
                for (a, b) in arr do
                    w.Write a
                    w.Write b

            | FunctionDescriptions arr ->
                w.Write "FunctionDescriptions"
                w.Write arr.Length
                for (a, b) in arr do
                    w.Write a
                    w.Write b

            | PackageDescription d ->
                w.Write "PackageDescription"
                w.Write d

            | RDataSymbols syms ->
                w.Write "RDataSymbols"
                w.Write syms.Length
                for (name, typOpt) in syms do
                    w.Write name
                    match typOpt with
                    | Some typ ->
                        w.Write true
                        w.Write typ.AssemblyQualifiedName
                    | None ->
                        w.Write false

            | ServerError msg ->
                w.Write "ServerError"
                w.Write msg


        let read (r: BinaryReader) : ServerResponse =
            match r.ReadString() with

            | "InitializationErrorMessageResult" ->
                InitializationErrorMessageResult (r.ReadString())

            | "Packages" ->
                let n = r.ReadInt32()
                let arr = Array.init n (fun _ -> r.ReadString())
                Packages arr

            | "UnitResult" ->
                UnitResult

            | "Bindings" ->
                let n = r.ReadInt32()
                let arr =
                    Array.init n (fun _ ->
                        let a = r.ReadString()
                        let b = r.ReadString()
                        a, b)
                Bindings arr

            | "FunctionDescriptions" ->
                let n = r.ReadInt32()
                let arr =
                    Array.init n (fun _ ->
                        let a = r.ReadString()
                        let b = r.ReadString()
                        a, b)
                FunctionDescriptions arr

            | "PackageDescription" ->
                PackageDescription (r.ReadString())

            | "RDataSymbols" ->
                let n = r.ReadInt32()
                let arr =
                    Array.init n (fun _ ->
                        let name = r.ReadString()
                        let hasType = r.ReadBoolean()
                        let typOpt =
                            if hasType then
                                let aqn = r.ReadString()
                                let t = System.Type.GetType(aqn, throwOnError = false)
                                Some t
                            else
                                None
                        name, typOpt)
                RDataSymbols arr

            | "ServerError" ->
                ServerError (r.ReadString())

            | other ->
                failwithf "Unknown ServerResponse case: %s" other

