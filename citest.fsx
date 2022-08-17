#r "nuget:RProvider,2.0.2"

open RProvider
open RProvider.``base``

open RDotNet

R.c (1., 2., 3)

let installed = R.c().Engine.Evaluate("as.data.frame(installed.packages()[,c(1,3:4)])").AsList()
installed.["Package"].GetValue<string[]>()
|> Array.zip (installed.["Version"].GetValue<string[]>())

R.print(installed)

installed.GetValue<string*string>()