// Based on: https://paulor.land/writing/algebra-beyond-numbers/index.html#modular-arithmetic

namespace Sylvester

open System
open System.Collections

open Sylvester.Arithmetic.N10
open Sylvester.Collections

[<RequireQualifiedAccess>]
module Mod = 
    let modulo n x = (x % n + n) % n
    
    let (+) n x y = modulo n (x + y)
    let (-) n x y = modulo n (x - y)
    let (*) n x y = modulo n (x * y)

[<AutoOpen>]
module Numbers = 
    let Z5 = Ring(Z, Mod.(+) 5, (*), 0, 1, (~+))
    
