(** 
---
category: Documentation
categoryindex: 1
index: 4
---
*)

(*** condition: prepare ***)
#nowarn "211"
#r "nuget: RProvider, 0.0.1-local"
(*** condition: fsx ***)
#if FSX
#r "nuget: RProvider,{{package-version}}"
#endif // FSX
(*** condition: ipynb ***)
#if IPYNB
#r "nuget: RProvider,{{package-version}}"
#endif // IPYNB

(**
Working with R expressions
===============

RProvider represents R objects and values through an `RExpr` type. RProvider includes an `RExpr` module that allows you to work with R expressions in a more idiomatic way using forward pipes (`|>`). First, open RProvider, its custom operators and any packages you need:
*)

open RProvider
open RProvider.Operators

open RProvider.``base``
open RProvider.datasets
open RProvider.stats

(**

# S4 classes

For this example, let's set up an S4 class and object from scratch:
*)

R.eval "setClass('testclass', representation(foo='character', bar='integer'))"
let s4 = R.eval "new('testclass', foo='s4', bar=1:4)"

(**
You can find out if there are slots using the `slots` and `trySlots` functions:
*)

s4 |> RExpr.slots
s4 |> RExpr.trySlots
R.mtcars |> RExpr.trySlots

(**
You can access slot values similarly with the `slot` and `trySlot` functions:
*)

s4 |> RExpr.slot "foo"
s4 |> RExpr.trySlot "foo"
s4 |> RExpr.trySlot "doesntexist"
