namespace RProvider.Internal

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

    /// Set an additional 'pathdir' within R to resolve packages
    abstract member AddPackagePath: string -> unit
    /// Install an R package from a CRAN mirror using 'install.packages' R function.
    /// Parameters are (1) package name, (2) version (empty string for latest), and
    /// (3) repository to use.
    /// Returns false if the package failed to install correctly.
    abstract member InstallPackage: (string * string * string)  -> bool

    /// Returns an array with the names of all installed packages (e.g. "base", "graphics" etc.).
    abstract member GetPackages: unit -> string []
    /// Returns an array of user installed packages only, with their name and version.
    abstract member GetPackageVersions: unit -> (string * string) []
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
    abstract member GetRDataSymbols: string -> (string * System.Type) []