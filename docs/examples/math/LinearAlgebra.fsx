#load "Include.fsx"

open Sylvester
open Dimension
open Vector

3. * vec ``3`` [-2; 0; 1]

let p, q = scalar_var<real> "p", scalar_var<real> "q"

let P = vec ``3`` [p.[0]; p.[1]; p.[2]]

let Q = vec ``3`` [q.[0]; q.[1]; q.[2]]

P * Q