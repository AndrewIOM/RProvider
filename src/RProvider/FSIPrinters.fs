namespace RProvider

open RProvider.Runtime

/// Print functions that may be used in
/// F# interactive to 'pretty-print' R types to the
/// console window. Use in your scripts by
/// passing to `fsi.AddPrinter`.
module FSIPrinters =

    /// Print any `SymbolicExpression` using R's built-in
    /// `print` function.
    let rValue (expr: Abstractions.RExpr) =
        let header = sprintf "R semantic type <%A>" expr.Type
        let body = expr |> RExprWrapper.toRBridge |> Printing.printUsingTempFile
        header + "\n\n" + body