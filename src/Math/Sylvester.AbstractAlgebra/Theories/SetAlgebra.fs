namespace Sylvester

open Sylvester.Arithmetic

module SetAlgebra =
    open BooleanAlgebra    

    [<Formula; Symbol"\u22c3">]
    let union<'t when 't : equality> (bound:int) (range:bool) (body:Family<'t>) = sum Set.(|+|) "\u22c3" bound range body.Head 

    [<Formula; Symbol"\u22c3">]
    let intersect<'t when 't : equality> (bound:int) (range:bool) (body:Family<'t>) = product Set.(|*|) "\u22c3" bound range body.Head
 
    
    /// Print set algebra operator symbols
    let print_set_algebra_operators (s:string) = 
        s.Replace("|+|", "\u222A")
         .Replace("|*|", "\u2229")
         .Replace("Empty", "\u2205")
         .Replace("U", "\uD835")

    
    //let |Union|_|
    let set_algebra<'t when 't: equality> = BooleanAlgebraTheory("Set Algebra", <@ Set.(|+|) @>, <@ Set.(|*|) @>, <@ Set.Empty @>, <@ Set.U<'t> @>, <@ id @>)

    (* Admissible Rules *)
    let LeftAssoc = set_algebra.Rules.[0]

    let RightAssoc = set_algebra.Rules.[1]

    let Commute = set_algebra.Rules.[2]

    let ReduceIdemp = set_algebra.Rules.[3]

    let ReduceIdent = set_algebra.Rules.[4]

    let ReduceComp = set_algebra.Rules.[5]

    let Distrib = set_algebra.Rules.[6]