namespace RProvider.Runtime

open RBridge
open RBridge.Extensions
open RBridge.Extensions.ActivePatterns
open RProvider.Runtime.RTypes
open RProvider.Common

/// Convert between user-facing RExpr and the internal
/// RBridge symbolic expression type.
module internal RExprWrapper =

    open RProvider.Abstractions

    let toRBridge (ex: RExpr) : RBridge.SymbolicExpression = { ptr = (RExpr.unwrap ex).ptr }

    let toRProvider (ex: RBridge.SymbolicExpression) : RExpr = RExpr.wrap { ptr = ex.ptr }


/// Contains conversion functions to convert from .NET types
/// to values in R, and from R to .NET types. Functions allow
/// extraction of R data to .NET memory, or expression of R objects
/// in .NET semantic wrappers.
module Convert =

    /// Default conversions from R to .NET primitive types or
    /// simple .NET representations provided by RBridge (for example,
    /// for RComplex). The values are extracted into .NET memory space.
    let tryFromRStructural<'outType> (engine: NativeApi.RunningEngine) (sexp: SymbolicExpression) : 'outType option =

        let retype (x: 'b) : Option<'outType> = x |> box |> unbox<'outType> |> Some
        let at = typeof<'outType>

        match sexp with

        // To array:
        | IntegerVector engine xs when at = typeof<int []> -> xs |> Extract.extractIntArray engine |> retype |> unbox
        | RealVector engine xs when at = typeof<float []> -> xs |> Extract.extractFloatArray engine |> retype |> unbox
        | LogicalVector engine xs when at = typeof<bool option []> ->
            xs |> Extract.extractLogicalArray engine |> retype |> unbox
        | CharacterVector engine xs when at = typeof<string []> ->
            xs |> Extract.extractStringArray engine |> retype |> unbox
        | ComplexVector engine xs when at = typeof<RComplex []> ->
            xs |> Extract.extractComplexArray engine |> retype |> unbox

        // To list:
        | IntegerVector engine xs when at = typeof<int list> ->
            xs |> Extract.extractIntArray engine |> Array.toList |> retype |> unbox
        | RealVector engine xs when at = typeof<float list> ->
            xs |> Extract.extractFloatArray engine |> Array.toList |> retype |> unbox
        | LogicalVector engine xs when at = typeof<bool option list> ->
            xs |> Extract.extractLogicalArray engine |> Array.toList |> retype |> unbox
        | CharacterVector engine xs when at = typeof<string list> ->
            xs |> Extract.extractStringArray engine |> Array.toList |> retype |> unbox
        | ComplexVector engine xs when at = typeof<RComplex list> ->
            xs |> Extract.extractComplexArray engine |> Array.toList |> retype |> unbox

        // To atomic:
        | IntegerVector engine xs when at = typeof<int> ->
            xs |> Extract.extractIntArray engine |> Array.head |> retype |> unbox
        | RealVector engine xs when at = typeof<float> ->
            xs |> Extract.extractFloatArray engine |> Array.head |> retype |> unbox
        | LogicalVector engine xs when at = typeof<bool option> ->
            xs |> Extract.extractLogicalArray engine |> Array.head |> retype |> unbox
        | CharacterVector engine xs when at = typeof<string> ->
            xs |> Extract.extractStringArray engine |> Array.head |> retype |> unbox
        | ComplexVector engine xs when at = typeof<RComplex> ->
            xs |> Extract.extractComplexArray engine |> Array.head |> retype |> unbox
        // | NumericVector (v) -> wrap <| v.ToArray()

        // To matrix:
        | CharacterMatrix engine v when at = typeof<string [,]> ->
            v |> Extract.extractStringMatrix engine |> retype |> unbox
        | IntegerMatrix engine v when at = typeof<int [,]> -> v |> Extract.extractIntMatrix engine |> retype |> unbox
        | LogicalMatrix engine v when at = typeof<bool option [,]> ->
            v |> Extract.extractLogicalMatrix engine |> retype |> unbox

        // Empty vectors in R are represented as null
        | Null engine _ when at = typeof<string list> -> retype <| List.empty<string>
        | Null engine _ when at = typeof<string []> -> retype <| Array.empty<string>
        | Null engine _ when at = typeof<RComplex list> -> retype <| List.empty<RComplex>
        | Null engine _ when at = typeof<RComplex []> -> retype <| Array.empty<RComplex>
        | Null engine _ when at = typeof<int list> -> retype <| List.empty<int>
        | Null engine _ when at = typeof<int []> -> retype <| Array.empty<int>
        | Null engine _ when at = typeof<bool option list> -> retype <| List.empty<bool>
        | Null engine _ when at = typeof<bool option []> -> retype <| Array.empty<bool>
        | Null engine _ when at = typeof<double list> -> retype <| List.empty<double>
        | Null engine _ when at = typeof<double []> -> retype <| Array.empty<double>

        // Dates and date-times
        // TODO Implement date/time conversions:
        // | RealVector engine xs when Dates.isDate xs && at = typeof<RDate[]> ->
        //     xs |> Array.map (fun days -> RDateTime.fromDays days)
        // | RealVector engine xs when Dates.isPosixDateTime xs && at = typeof<RDateTime[]> ->
        //     xs |> Array.map (fun seconds -> Create.dateTimeVectorFromSeconds seconds)
        // | RealVector engine xs when Dates.isDate xs && at = typeof<RDate list> ->
        //     xs |> Array.map (fun days -> Create.dateVectorFromDays days)
        // | RealVector engine xs when Dates.isPosixDateTime xs && at = typeof<RDateTime list> ->
        //     xs |> Array.map (fun seconds -> Create.dateTimeVectorFromSeconds seconds)
        // | RealVector engine xs when Dates.isDate xs && at = typeof<RDate> ->
        //     xs |> Array.map (fun days -> Create.dateVectorFromDays days)
        // | RealVector engine xs when Dates.isPosixDateTime xs && at = typeof<RDateTime> ->
        //     xs |> Array.map (fun seconds -> Create.dateTimeVectorFromSeconds seconds)

        | _ ->
            LogFile.logf
                "Cannot convert SexpType %A to %s"
                (SymbolicExpression.getType engine sexp)
                typeof<'outType>.FullName

            None

    /// Convert a value from a symbolic expression into a wrapped
    /// R type included in RProvider. The types are shaped so as to
    /// be usable directly by statistics libraries that support them,
    /// without the need for a plugin converter system.
    /// Conversion between basic numeric types and containers (e.g.
    /// data frame) should be done using explicit conversion functions.
    let tryAsRTyped engine sexp : RTypes.RSemantic<'u> option =
        LogFile.logf "Classified as %A" (classify engine sexp)
        match classify engine sexp with
        | FactorType -> Factor.tryOfExpr sexp |> Option.map FactorInR
        | DataFrameType -> DataFrame.tryOfExpr sexp |> Option.map DataFrameInR
        | VectorType -> GenericVector.tryCreate sexp |> Option.map VectorInR        
        | ScalarType
        | ListType
        | MatrixType
        | ArrayType
        | FunctionType
        | EnvironmentType
        | S3ObjectType
        | S4ObjectType
        | R6ObjectType -> None

    let toR (eng: NativeApi.RunningEngine) (value: obj) : SymbolicExpression =
        match value with

        // Pass-through of basic R expression values:
        | :? SymbolicExpression as s -> s
        | :? RProvider.Abstractions.RExpr as s -> RExprWrapper.toRBridge s

        // .NET primitives:
        | null -> { ptr = eng.Api.nilValue }
        | :? string as s -> Create.stringVector eng [| s |]
        | :? array<string> as xs -> Create.stringVector eng xs
        | :? list<string> as xs -> Create.stringVector eng xs
        | :? int as i -> Create.intVector eng [| i |]
        | :? array<int> as xs -> Create.intVector eng xs
        | :? list<int> as xs -> Create.intVector eng xs
        | :? float as f -> Create.realVector eng [| f |]
        | :? array<float> as xs -> Create.realVector eng xs
        | :? list<float> as xs -> Create.realVector eng xs
        | :? bool as b -> Create.logicalVector eng [| b |]
        | :? array<bool> as xs -> Create.logicalVector eng xs
        | :? list<bool> as xs -> Create.logicalVector eng xs
        | :? RComplex as c -> Create.complexVector eng [| c |]
        | :? array<RComplex> as cs -> Create.complexVector eng cs
        | :? list<RComplex> as cs -> Create.complexVector eng cs
        // TODO RDate and RDateTime.

        // Pass-through of R semantic types:
        | :? Factor.RFactor as f -> f.Sexp
        | :? Real.Vector.RRealVector<_> as v -> v.Inner.Sexp
        // | :? RVector.ComplexV v -> v.Inner.Sexp
        // | :? RVector.IntegerV v -> v.Inner.Sexp
        // | :? RVector.LogicalV v -> v.Inner.Sexp
        // | :? RVector.NumericV v -> v.Inner.Sexp
        // | :? RVector.RawV v -> v.Inner.Sexp
        | :? Real.Scalar.RRealScalar<_> as s -> s.RExpr
        | :? DataFrame.RFrame as df -> df.RExp

        | _ ->
            failwithf
                "The value you passed cannot be converted to an R object.\n\
                Type: %s\n\
                Value: %A\n\
                Hint: RProvider only supports built-in conversion of primitive types, arrays, and lists.\n\
                Tuples of length > 2 are not automatically converted. Consider converting it manually. Note: F# lists are seperated with semi-colons, not commas."
                (value.GetType().Name)
                value
