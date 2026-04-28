module RDataTests

open System
open RProvider
open Expecto

type Sample = RData<"/Users/andrewmartin/Documents/GitHub Projects/RProvider/tests/RProvider.Tests/data/sample.rdata">

let removeNAs v = v |> Array.choose id

[<Tests>]
let rDataTests =
    testList "RData provider" [

        testCase "Can read sample RData file" <| fun _ ->
            let sample = Sample()
            let sum = sample.volcanoList |> removeNAs |> Array.sum
            Expect.equal sum 690907.0 ""
            Expect.equal (int sample.volcanoMean.[0].Value) 130 ""

        testCase "Can save RData file and read it from a temp path" <| fun _ ->
            let volcanoList = [| 3.0; 1.0 |]
            let volcanoMean = [| 2.0 |]

            let temp = IO.Path.GetTempFileName() + ".rdata"
            R.assign ("volcanoList", volcanoList) |> ignore
            R.assign ("volcanoMean", volcanoMean) |> ignore
            R.save (list = [ "volcanoList"; "volcanoMean" ], file = temp) |> ignore

            let sample = Sample temp
            Expect.equal (sample.volcanoList |> removeNAs |> Array.sum) 4.0 ""
            Expect.equal (int sample.volcanoMean.[0].Value) 2 ""


    ]
