#load "../math/Include.fsx"

open Sylvester
open Dimension
open Matrix

module E3 = 
    let A = mat ``2`` ``3`` [1; 2; -1; -2; 3; 5]
    let B = mat ``2`` ``3`` [1; 0; 3; -4; 2; -1]
    let C = A + 3 * B