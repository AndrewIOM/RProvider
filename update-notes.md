## Changes made as part of moving to RBridge from R.NET

* Replaced R.NET with RBridge.
* bindingInfo in R interop switched to use SEXP types rather than R typeof(get(..)) execution.
* Introduced RTypes in Runtime, for wrapping of semantic R types.
* Removed old plugin system. As an alternative, implemented FsSci shapes on RTypes, so that R numeric types are interoperable with F# types. This doesn't allow implicit conversion between R frames and Deedle for example, but does allow use of R data frames in F# stats functions that support the common numerics shapes.
* getBindings actually properly uses environments / namespaces; this was not implemented before (in RInterop).
* Refactored 'call' within Runtime to use the namespace of the package to retrieve the symbol for the required function, then call it directly. Drops support for tuple-based named arguments.