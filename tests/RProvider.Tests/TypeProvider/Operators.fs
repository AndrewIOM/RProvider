module OperatorTests

open RProvider
open RProvider.Operators
open RProvider.Runtime

open RProvider.datasets
open RProvider.methods

open Expecto

[<Tests>]
let operatorTests =
    testList "Operators" [

        testCase "Cannot pass two parameters of same name to R function" <| fun _ ->
            Expect.throwsT<System.Exception>(fun () -> 
                R.data_frame [ "Test" => [ 1; 42; 2 ]; "Test" => seq { 1 .. 10 } ] |> ignore)
                ""

        testCase "Can make a dataframe using list arguments from arrow operator" <| fun _ ->
                let df = R.data_frame [ "Test" => [ 1; 42; 2 ] ]
                Expect.stringContains (df.Print()) "42"""

        testCase "Can get member of an S3 object when it exists" <| fun _ ->
            let col = R.mtcars.Member "mpg"
            let t = col.TryAsTyped
            Expect.isSome col.TryAsVector "Expected mpg column to be a vector"

        testCase "Can access S3 member using dynamic operator" <| fun _ ->
            let col = R.mtcars?mpg
            Expect.isSome col.TryAsVector "Expected mpg column to be a vector"

        testCase "Throws when S3 member does not exist" <| fun _ ->
            Expect.throws
                (fun () -> R.mtcars?somerandomvectorname |> ignore)
                "Expected accessing a missing S3 member to throw"

        testCase "Can get slot of an S4 object using dynamic operator" <| fun _ ->
            // Define an S4 class
            R.setClass(namedParams [
                "Class", box "Person"
                "slots", box (R.c (namedParams [
                    "name"  =>  "string"
                    "age"   =>  "numeric"
                ]))
            ]) |> ignore

            // Instantiate it
            let person =
                R.``new`` (namedParams ["Class" => "Person"])

            // Access slot
            let age = Expect.wantSome person?age.TryAsVector "Age was not a numeric vector"

            let len: int =
                match age with
                | RTypes.RVector.NumericV n -> n.Length.RExpr.FromR ()
                | _ -> failwith "wrong type"

            Expect.equal len 0 "Expected age slot to be a numeric vector of length 0"
    ]

