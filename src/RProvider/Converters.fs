namespace RProvider.Runtime

open RBridge
open RBridge.Extensions
open RBridge.Extensions.ActivePatterns
open RProvider.Runtime.RTypes

/// Contains conversion functions to convert from .NET types
/// to values in R, and from R to .NET types. Functions allow
/// extraction of R data to .NET memory, or expression of R objects
/// in .NET semantic wrappers.
module Convert =

    /// Default conversions from R to .NET primitive types or
    /// simple .NET representations provided by RBridge (for example,
    /// for RComplex). The values are extracted into .NET memory space.
    let tryFromRStructural<'outType> (engine : NativeApi.RunningEngine) (sexp : SymbolicExpression) : 'outType option =
        
        let retype (x: 'b) : Option<'outType> = x |> box |> unbox<'outType> |> Some
        let at = typeof<'outType>
        
        match sexp with

        // To array:
        | IntegerVector engine xs when at = typeof<int[]> ->
            xs |> Extract.extractIntArray engine |> retype |> unbox
        | RealVector engine xs when at = typeof<float[]> ->
            xs |> Extract.extractFloatArray engine |> retype |> unbox
        | LogicalVector engine xs when at = typeof<bool option[]> ->
            xs |> Extract.extractLogicalArray engine |> retype |> unbox
        | CharacterVector engine xs when at = typeof<string[]> ->
            xs |> Extract.extractStringArray engine |> retype |> unbox
        | ComplexVector engine xs when at = typeof<RComplex[]> ->
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
        | IntegerMatrix engine v when at = typeof<int [,]> ->
            v |> Extract.extractIntMatrix engine |> retype |> unbox
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
            Logging.logf "Cannot convert SexpType %A to %s"
                (SymbolicExpression.getType engine sexp)
                typeof<'outType>.FullName
            None


    /// Convert a value from a symbolic expression into a wrapped
    /// R type included in RProvider. The types are shaped so as to
    /// be usable directly by statistics libraries that support them,
    /// without the need for a plugin converter system.
    /// Conversion between basic numeric types and containers (e.g.
    /// data frame) should be done using explicit conversion functions.
    let tryAsRTyped engine sexp : RProvider.Runtime.RTypes.RSemantic<'u> option =

        match sexp with
        | Factor engine f -> Factor.tryFromExpr f |> Option.map FactorInR
        | DataFrame engine df -> DataFrame.tryAsFrame df |> Option.map DataFrameInR

        | RealVector engine s
        | ComplexVector engine s
        | IntegerVector engine s
        | LogicalVector engine s
        | CharacterVector engine s
        | RawVector engine s ->
            GenericVector.tryCreate s
            |> Option.map VectorInR

        // TODO Assess if full types are covered, e.g. Matrix?
        // - Date and DateTime?

        | _ ->
            Logging.logf "No typed conversion was possible for sexp: %A"
                (SymbolicExpression.getType engine sexp)
            None

    let toR (eng : NativeApi.RunningEngine) (value : obj) : SymbolicExpression =
        match value with

        // .NET primitives:
        | :? string as s -> Create.stringVector eng [| s |]
        | :? array<string> as xs -> Create.stringVector eng xs
        | :? int as i -> Create.intVector eng [| i |]
        | :? array<int> as xs -> Create.intVector eng xs
        | :? float as f -> Create.realVector eng [| f |]
        | :? array<float> as xs -> Create.realVector eng xs
        | :? bool as b -> Create.logicalVector eng [| b |]
        | :? array<bool> as xs -> Create.logicalVector eng xs
        | :? RComplex as c -> Create.complexVector eng [| c.Real, c.Imag |]
        | :? array<RComplex> as cs -> Create.complexVector eng (cs |> Array.map (fun c -> c.Real, c.Imag))
        // TODO RDate and RDateTime.

        // Pass-through of values already in R:
        | :? SymbolicExpression as s -> s
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
            failwithf "Cannot convert .NET value of type %s to R"
                (value.GetType().FullName)


// /// This type can be used for providing new converters that can convert
// /// custom .NET data types to R values. The converter is used whenever the
// /// user calls an R function (such as `R.foo(...)`) with an arguments that
// /// is of type `TInType`.
// type ToRConverter =
//     { Convert : NativeApi.RunningEngine -> obj -> SymbolicExpression }

// /// This type can be used for providing new converters that can convert
// /// R values to .NET types. The converter is used whenever the users calls the
// /// `se.GetValue<'TOutType>()` on a `SymbolicExpression` value returned from R
// /// provider.
// type FromRConverter<'TOutType> =
//     { ConvertFrom: SymbolicExpression -> Option<'TOutType> }

// /// Represents a function that converts a specific R shape into
// /// a .NET object of an appropriate type.
// type RShapeToDotNet = NativeApi.RunningEngine -> SymbolicExpression -> obj option

// /// A central representation of possible type conversion to and
// /// from R values.
// type ConverterRegistry =
//     { ToR : IReadOnlyDictionary<Type, ToRConverter>
//       FromR: IReadOnlyDictionary<SymbolicExpression.SexpType, RShapeToDotNet> }

// /// Functions to create and manage a .NET <-> R conversion registry.
// module Registry =

//     /// Add a function that will convert from a specific type to a value in R to
//     /// an existing converter registry.
//     let registerToR<'T> conv (reg : ConverterRegistry) =
//         let dict = Dictionary reg.ToR
//         let wrapper =
//             { Convert = fun eng (o: obj) -> conv eng (unbox<'T> o) }
//         let t = typeof<'T>

//         dict.[t] <- wrapper
//         { reg with ToR = dict }

//     /// Add a function that will convert from a specific R value or class to a .NET
//     /// type into an existing container registry.
//     let registerFromR sexpType conv registry =
//         let dict = Dictionary registry.FromR
//         dict.[sexpType] <- conv
//         { registry with FromR = dict }

//     /// Convert an R expression into 'T. Fails if no converter function
//     /// for 'T in the registry.
//     let convertFromR<'T> registry engine sexp : 'T =
//         let sexpType = SymbolicExpression.getType engine sexp
//         match registry.FromR.TryGetValue sexpType with
//         | true, conv ->
//             match conv engine sexp with
//             | Some o ->
//                 match o with
//                 | :? 'T as t -> t
//                 | _ -> failwithf "Conversion registry returned wrong type for %s" typeof<'T>.FullName
//             | None -> failwithf "Conversion registry could not convert R type %A to %s" sexpType typeof<'T>.FullName
//         | _ -> failwithf "No converter registered for R type %A" sexpType

//     let convertFromRToObj registry eng sexp =
//         let sexpType = SymbolicExpression.getType eng sexp
//         match registry.FromR.TryGetValue sexpType with
//         | true, conv -> conv eng sexp
//         | _ -> None

//     let defaultConvertFromR converters =
//         converters
//         |> registerFromR SymbolicExpression.IntegerVector factorVectorConverter
//         |> registerFromR<string>   factorScalarConverter

//     /// A container of basic converters of .NET -> R values.
//     let internal defaultConvertToR registry =
//         registry
//         |> registerToR<SymbolicExpression> (fun _ v -> v)
//         |> registerToR<string> (fun eng v -> Create.createStringVector eng [| v |])
//         |> registerToR<int> (fun eng v -> Create.createIntVector eng [| v |])
//         |> registerToR<double> (fun eng v -> Create.createDoubleVector eng [| v |])
//         |> registerToR<DateTime> (fun eng v -> Create.createDateVector eng [ v ])
//         |> registerToR<string seq> (fun eng v -> Create.createStringVector eng (Seq.toArray v))
//         |> registerToR<double seq> (fun eng v -> Create.createDoubleVector eng (Seq.toArray v))
//         |> registerToR<DateTime seq> (fun eng v -> Create.createDateVector eng v)
//         |> registerToR<double[,]> (fun eng v -> Create.createDoubleMatrix eng v)
// //         registerToR<Complex> (fun engine v -> upcast engine.CreateComplexVector [| v |])
// //         registerToR<bool> (fun engine v -> upcast engine.CreateLogicalVector [| v |])
// //         registerToR<byte> (fun engine v -> upcast engine.CreateRawVector [| v |])
// //         registerToR<Complex seq> (fun engine v -> upcast engine.CreateComplexVector v)
// //         registerToR<int seq> (fun engine v -> upcast engine.CreateIntegerVector v)
// //         registerToR<bool seq> (fun engine v -> upcast engine.CreateLogicalVector v)
// //         registerToR<byte seq> (fun engine v -> upcast engine.CreateRawVector v)
// //         registerToR<string [,]> (fun engine v -> upcast engine.CreateCharacterMatrix v)
// //         registerToR<Complex [,]> (fun engine v -> upcast engine.CreateComplexMatrix v)
// //         registerToR<int [,]> (fun engine v -> upcast engine.CreateIntegerMatrix v)
// //         registerToR<bool [,]> (fun engine v -> upcast engine.CreateLogicalMatrix v)
// //         registerToR<byte [,]> (fun engine v -> upcast engine.CreateRawMatrix v)

//     let defaultRegistry =
//         { FromR = Dictionary<Type, obj>(); ToR = Dictionary<Type, ToRConverter>() }
//         |> defaultConvertFromR
//         |> defaultConvertToR



//     // let internal defaultConvertFromRBuiltins engine (sexp: SymbolicExpression) : Option<obj> =
//     //     let wrap x = box x |> Some

//     //     match sexp with
//     //     | CharacterVector engine v -> wrap <| v
//     //     // | ComplexVector (v) -> wrap <| v.InnerToArray()
//     //     // | IntegerVector (v) -> wrap <| v.ToArray()
//     //     // | LogicalVector (v) -> wrap <| v.ToArray()
//     //     // | NumericVector (v) ->
//     //     //     match v.GetAttribute("class") with
//     //     //     | CharacterVector (cv) when cv.ToArray() = [| "Date" |] ->
//     //     //         wrap <| [| for n in v -> DateTime.FromOADate(n + RDateOffset) |]
//     //     //     | _ -> wrap <| v.ToArray()
//     //     // | CharacterMatrix (v) -> wrap <| v.ToArray()
//     //     // | ComplexMatrix (v) -> wrap <| v.ToArray()
//     //     // | IntegerMatrix (v) -> wrap <| v.ToArray()
//     //     // | LogicalMatrix (v) -> wrap <| v.ToArray()
//     //     // | NumericMatrix (v) -> wrap <| v.ToArray()
//     //     // | List (v) -> wrap <| v
//     //     // | Pairlist (pl) -> wrap <| (pl |> Seq.map (fun sym -> sym.PrintName, sym.AsSymbol().Value))
//     //     // | Null () -> wrap <| null
//     //     // | Symbol (s) -> wrap <| (s.PrintName, s.Value)
//     //     | _ -> None


// module BuiltIn =

//     /// Contains higher-level converters
//     /// [omit]
//     module Factor =

//         let getLevels sexp =
//             let rvalStr = RInterop.serializeRValue (RValue.Function([ "x" ], false))
//             let symexpr = RInterop.call "base" "levels" rvalStr [| sexp |] [||]
//             symexpr.AsCharacter().ToArray()

//         let factorFromR : RShapeToDotNet =
//             fun eng sexp ->
//                 if Extract.isFactor eng sexp then
//                     let levels = Factor.getLevels eng sexp
//                     let ints = Extract.extractIntArray eng sexp
//                     Some (Array.map (fun i -> levels.[i - 1]) ints :> obj)
//                 else None

//     module DataFrame =

//         // [<Export(typeof<IConvertFromR<string>>)>]
//         // type DataFrameConverter() =
//         //     interface IConvertFromR<string> with
//         //         member this.Convert(sexp: SymbolicExpression) =
//         //             match sexp with
//         //             | IntegerVector (nv) when sexp.Class = [| "factor" |] && nv.Length = 1 ->
//         //                 Some <| getLevels(sexp).[nv.[0]]
//         //             | _ -> None

//         let frameFromR engine = function
//             | ActivePatterns.DataFrame engine df ->
//                 Some df
//             | _ -> None


//     // /// Convert an R value to a .NET value of the specified outType.
//     // /// Does not include complex R types, such as closures, but only
//     // /// basic numeric and character types.
//     // let internal convertFromRBuiltins<'outType> engine (sexp: SymbolicExpression) : Option<'outType> =
//     //     let retype (x: 'b) : Option<'a> = x |> box |> unbox |> Some
//     //     let at = typeof<'outType>

//     //     match sexp with
//     //     | CharacterVector engine v when at = typeof<string list> -> retype <| List.ofSeq v
//     //     | CharacterVector engine v when at = typeof<string []> -> retype <| v
//     //     | CharacterVector engine v when at = typeof<string> -> retype <| Array.head v
//     //     | RealVector engine v when at = typeof<float list> -> retype <| List.ofSeq v
//     //     | RealVector engine v when at = typeof<float []> -> retype <| v
//     //     | RealVector engine v when at = typeof<float> -> retype <| Array.head v
//     //     | IntegerVector engine v when at = typeof<int list> -> retype <| List.ofSeq v
//     //     | IntegerVector engine v when at = typeof<int []> -> retype <| v
//     //     | IntegerVector engine v when at = typeof<int> -> retype <| Array.head v
//     //     | LogicalVector engine v when at = typeof<bool list> -> retype <| List.ofSeq v
//     //     | LogicalVector engine v when at = typeof<bool []> -> retype <| v
//     //     | LogicalVector engine v when at = typeof<bool> -> retype <| Array.head v
//     //     | CharacterMatrix engine v when at = typeof<string [,]> -> retype <| v.ToArray()
//     //     | IntegerMatrix engine v when at = typeof<int [,]> -> retype <| v.ToArray()
//     //     | LogicalMatrix engine v when at = typeof<int [,]> -> retype <| v.ToArray()
//     //     // Empty vectors in R are represented as null
//     //     | Null () when at = typeof<string list> -> retype <| List.empty<string>
//     //     | Null () when at = typeof<string []> -> retype <| Array.empty<string>
//     //     | Null () when at = typeof<RComplex list> -> retype <| List.empty<RComplex>
//     //     | Null () when at = typeof<RComplex []> -> retype <| Array.empty<RComplex>
//     //     | Null () when at = typeof<int list> -> retype <| List.empty<int>
//     //     | Null () when at = typeof<int []> -> retype <| Array.empty<int>
//     //     | Null () when at = typeof<bool list> -> retype <| List.empty<bool>
//     //     | Null () when at = typeof<bool []> -> retype <| Array.empty<bool>
//     //     | Null () when at = typeof<double list> -> retype <| List.empty<double>
//     //     | Null () when at = typeof<double []> -> retype <| Array.empty<double>
//     //     | Null () when at = typeof<DateTime list> -> retype <| List.empty<DateTime>
//     //     | Null () when at = typeof<DateTime []> -> retype <| Array.empty<DateTime>

//     //     | _ -> None

//     // let internal convertFromR<'outType> engine (sexp: SymbolicExpression) : 'outType =
//     //     let concreteType = typeof<'outType>
//     //     match convertFromRBuiltins<'outType> engine sexp with
//     //     | Some res -> res
//     //     | _ ->
//     //         failwithf
//     //             "No converter registered to convert from R %A to .NET type %s"
//     //             (SymbolicExpression.getType engine sexp)
//     //             concreteType.FullName

//     // let internal defaultConvertFromR engine (sexp: SymbolicExpression) : obj =
//     //     Logging.logf "Converting value from R..."
//     //     let converters = mefContainer.Value.GetExports<IDefaultConvertFromR>()

//     //     match converters |> Seq.tryPick (fun conv -> conv.Value.Convert sexp) with
//     //     | Some res -> res
//     //     | None ->
//     //         match defaultConvertFromRBuiltins engine sexp with
//     //         | Some res -> res
//     //         | _ -> failwithf "No default converter registered from R %A." (SymbolicExpression.getType engine sexp)


