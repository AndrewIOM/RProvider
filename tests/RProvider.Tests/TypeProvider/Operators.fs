module OperatorTests

open RProvider
open RProvider.datasets
open RProvider.Runtime.Helpers

open Expecto

[<Tests>]
let operatorTests =
    testList "Operators" [

        // [<Fact>]
        // let ``Cannot pass two parameters of same name to R function`` () =
        //     Assert.Throws<System.Exception>(fun () -> 
        //         R.data_frame [ "Test" => [ 1; 42; 2 ]; "Test" => seq { 1 .. 10 } ] |> ignore)

        // [<Fact>]
        // let ``Can make a dataframe using list arguments from arrow operator`` () =
        //     let df = R.data_frame [ "Test" => [ 1; 42; 2 ] ]
        //     Assert.Contains("42", df.Print())

        // testCase "Can get member of an S3 object when it exists" <| fun _ ->
        //     let col = R.mtcars.Member "mpg"
        //     Expect.isTrue col.IsVector "Expected mpg column to be a vector"

        // testCase "Can access S3 member using dynamic operator" <| fun _ ->
        //     let col = R.mtcars?mpg
        //     Expect.isTrue col.IsVector "Expected mpg column to be a vector"

        // testCase "Throws when S3 member does not exist" <| fun _ ->
        //     Expect.throws
        //         (fun () -> R.mtcars?somerandomvectorname |> ignore)
        //         "Expected accessing a missing S3 member to throw"

        // testCase "Can get slot of an S4 object using dynamic operator" <| fun _ ->
        //     // Define an S4 class
        //     R.setClass <| namedParams [
        //         "Class", box "Person"
        //         "slots", box (R.c (namedParams [
        //             "name", "string"
        //             "age",  "numeric"
        //         ]))
        //     ]

        //     // Instantiate it
        //     let person = R.``new`` <| namedParams ["Class", "Person"]

        //     // Access slot
        //     let age = person?age.AsNumeric()

        //     Expect.equal age.Length 0 "Expected age slot to be a numeric vector of length 0"
    ]

