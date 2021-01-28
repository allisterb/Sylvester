namespace Sylvester

open System
open System.Text
open System.Text.RegularExpressions
open ExpectNet
open Expect

module E =
    let mutable cmd = "eprover"

type E(inp) =
    inherit Runtime()

