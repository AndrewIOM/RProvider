module ConverterTests

open RProvider
open System
open Expecto
open System.Text
open System.Globalization
open RProvider.Common
open RProvider.SymbolicExpressionExtensions

// Generic function to test that a value round-trips
// when SEXP is asked for the value by-type
let testRoundTrip (x: 'a) (typeof: RBridge.SymbolicExpression.SexpType) (clsName: Option<string>) =
    let sexp = SymbolicExpression.ofObj x
    Expect.equal x (sexp.FromR<'a>()) ""
    Expect.equal (SymbolicExpression.getExprType sexp) typeof ""
    Expect.equal sexp.Class (Option.toArray clsName) ""

// Generic function to test that a value round-trips
// when SEXP is asked for the value by-type, and
// as the default .NET representation
let testRoundTripAndDefault (x: 'a) (typeof: RBridge.SymbolicExpression.SexpType) (clsName: Option<string>) =
    testRoundTrip x typeof clsName
    let sexp = SymbolicExpression.ofObj x
    Expect.equal x (unbox<'a> <| sexp.FromR<obj>()) ""

let testVector (xs: 'scalarType []) (typeof: RBridge.SymbolicExpression.SexpType) (clsName: Option<string>) =
    // Test arrays and lists round-trip
    testRoundTrip (Array.toList xs) typeof clsName
    // Array is the default return type from .Value
    testRoundTripAndDefault xs typeof clsName
    // Can only round-trip a vector as a scalar if it is of length 1
    if xs.Length <> 1 then
        ignore <| Expect.throwsT<InvalidOperationException>(fun () ->
            let s = SymbolicExpression.ofObj xs
            s.FromR<'scalarType>() |> ignore)

let testScalar (x: 'scalarType) (typeof: RBridge.SymbolicExpression.SexpType) (clsName: Option<string>) =
    // Scalars round-trip to scalar when scalar-type is requested explicitly
    testRoundTrip x typeof clsName

    // Scalars round-trip as vectors
    let sexp = SymbolicExpression.ofObj x
    Expect.equal[| x |] (unbox<'scalarType []> <| sexp.FromR()) ""
    Expect.equal[| x |] (sexp.FromR<'scalarType []>()) ""

[<Tests>]
let roundTrips =
    testList "Round-trip tests" [

        // testProperty "Date vector round-trip tests" <| fun (xs: DateTime []) ->
        //     testVector xs RBridge.SymbolicExpression.SexpType.RealVector (Some "Date")

        // testProperty "Date scalar round-trip tests" <| fun (xs: DateTime) ->
        //     testScalar xs RBridge.SymbolicExpression.SexpType.RealVector (Some "Date")

        testProperty "Int vector round-trip tests" <| fun (xs: int []) ->
            testVector xs RBridge.SymbolicExpression.SexpType.IntegerVector None

        testProperty "Int scalar round-trip tests" <| fun (x: int) ->
            testScalar x RBridge.SymbolicExpression.SexpType.IntegerVector None

        testProperty "Double vector round-trip tests" <| fun (x: float []) ->
            testVector x RBridge.SymbolicExpression.SexpType.RealVector None

        testProperty "Double scalar round-trip tests" <| fun (x: float) ->
            testScalar x RBridge.SymbolicExpression.SexpType.RealVector None

        // testProperty "Bool vector round-trip tests" <| fun (x: bool option []) ->
        //     testVector x RBridge.SymbolicExpression.SexpType.LogicalVector None

        // testProperty "Bool scalar round-trip tests" <| fun (x: bool option) ->
        //     testScalar x RBridge.SymbolicExpression.SexpType.LogicalVector None

        testProperty "Complex vector round-trip tests" <| fun (x: (float * float) []) ->
            let xs =
                [| for (r, i) in x do
                    if not (Double.IsNaN(r) || Double.IsNaN(i)) then yield RBridge.Extensions.RComplex.Create (r, i) |]
            testVector xs RBridge.SymbolicExpression.SexpType.ComplexVector None

        testProperty "Complex scalar round-trip tests" <| fun (r: float) (i:float) ->
            if not (Double.IsNaN(r) || Double.IsNaN(i)) then
                let x = RBridge.Extensions.RComplex.Create(r, i)
                testScalar x RBridge.SymbolicExpression.SexpType.ComplexVector None

        testProperty "String arrays round-trip" <| fun (strings: string []) ->
            let ascii = ASCIIEncoding()
            if Array.forall (fun (s: string) -> s = (s |> ascii.GetBytes |> ascii.GetString)) strings then
                let sexp = SymbolicExpression.ofObj strings
                Expect.equal strings (unbox<string[]> <| sexp.FromR<obj>()) ""
                Expect.equal strings (sexp.FromR<string []>()) ""



    ]

// TODO Serialisation is now a server / client concern, not runtime.
// let serialisation =
//     testList "Serialisation" [

//         testProperty "Serialization of R values works" <| fun (isValue: bool) (args: string []) (hasVar: bool) ->
        
//             let args = List.ofSeq args

//             if args |> Seq.forall (fun a -> not (isNull a) && not (a.Contains(";"))) then
//                 let rvalue = if isValue then RValue.Value else RValue.Function(args, hasVar)
//                 let actual = deserializeRValue (serializeRValue (rvalue))
//                 Expect.equal rvalue actual ""



//     ]
