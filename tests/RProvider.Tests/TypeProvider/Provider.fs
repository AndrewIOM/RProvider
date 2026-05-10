module ProviderTests

open System
open System.Globalization
open RProvider
open RProvider.datasets
open RProvider.Runtime.Helpers

open Expecto

[<Tests>]
let printing =
    testList "Printing" [

        testCase "Printing of data frame returns string with frame data" <| fun _ ->
            let df = namedParams [ "Test", box [| 1; 42; 2 |] ] |> R.data_frame
            Expect.stringContains (RExpr.printToString df) "42" ""

    ]

[<Tests>]
let genTypes =
    testList "Generated types" [

        testCase "Can access mtcars (dataset property)" <| fun _ ->
            R.mtcars |> ignore
    ]

// module RoundTrip =

//     let roundTripAsFactor (value: string []) =
//         let sexp = R.as_factor(x = value)
//         Expect.equal(value, sexp.FromR<string []>())

//     let roundTripAsDataframe (value: string []) =
//         let df =
//             R.data_frame(namedParams [ "Column", value ]).AsDataFrame
//             |> fun df -> Expect.wantSome df "Was not inferred as a data frame"
//         // let col = Runtime.RTypes.DataFrame.getColumn "Column"
//         Expect.equal(value, df.[0].FromR<string []>())

//     [<Tests>]
//     let roundTrips =
//         testList "Round trip of non-primitive types" [

        // ]


let systemLocale = Threading.Thread.CurrentThread.CurrentCulture

[<Tests>]
let locales =
    testList "When working in different locales" [

        testCase "Passing numbers works for regions with a dot decimal seperator" <| fun _ ->
            Threading.Thread.CurrentThread.CurrentCulture <- CultureInfo("en-GB", false)
            let x1 : float = R.sin(1).FromR()
            let x2 : float = R.sin(1.0).FromR()
            Expect.floatClose Accuracy.high x1 0.8414709848 ""
            Expect.floatClose Accuracy.high x2 0.8414709848 ""
            Threading.Thread.CurrentThread.CurrentCulture <- systemLocale

        testCase "Passing numbers works for regions with a comma decimal seperator" <| fun _ ->
            Threading.Thread.CurrentThread.CurrentCulture <- CultureInfo("nn-NO", false)
            let x1 : float = R.sin(1).FromR()
            let x2 : float = R.sin(1.0).FromR()
            Expect.floatClose Accuracy.high x1 0.8414709848 ""
            Expect.floatClose Accuracy.high x2 0.8414709848 ""
            Threading.Thread.CurrentThread.CurrentCulture <- systemLocale

    ]


[<EntryPoint>]
let main argv =
    Tests.runTestsInAssemblyWithCLIArgs [ Sequenced ] argv
