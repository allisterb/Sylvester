namespace Sylvester

type Peano = Zero | Succ of Peano
    with
        static member op_Explicit(source: Peano) : int =
            let rec desugar = 
                function
                | Zero   -> 0
                | Succ x -> 1 + desugar x
            desugar source
        override this.ToString() =       
            sprintf "%d" (int this)

module Peano =
    
    let rec add a b =
        match a, b with
        | Zero, b   -> b
        | Succ x, b -> Succ (add x b)

    let rec mult a b =
        match a, b with
        | a, Zero   -> Zero
        | a, Succ x -> add a (mult a x)
