module Tests

open System
open Xunit

[<Fact>]
let ``Can start maxima process`` () =
    let m = new Maxi
