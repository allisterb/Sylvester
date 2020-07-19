namespace Sylvester

module SetAlgebra =
    open BooleanAlgebra    

    (* Symbols *)
    do 
        Symbols.BulitIn.Add("Empty", "\u2205")
        Symbols.BulitIn.Add("U", "\u1D54C")
    
    (* Formulas *)

    /// n-ary union of sets
    [<Formula>]
    let union<'t when 't : equality> (bound:int) (range:bool) (body:Set<'t>) = sum Set.(|+|) "\u22c3" bound range body

    /// n-ary intersection of sets
    [<Formula>]
    let intersect<'t when 't : equality> (bound:int) (range:bool) (body:Set<'t>) = product Set.(|*|) "\u22c3" bound range body
    

    (* Theory *)

    let set_algebra<'t when 't: equality> = BooleanAlgebraTheory("Set Algebra", <@ Set.(|+|) @>, <@ Set.(|*|) @>, <@ Set.Empty @>, <@ Set.U<'t> @>, <@ id @>)

    (* Admissible Rules *)
    
    let LeftAssoc = set_algebra.Rules.[0]

    let RightAssoc = set_algebra.Rules.[1]

    let Commute = set_algebra.Rules.[2]

    let ReduceIdemp = set_algebra.Rules.[3]

    let ReduceIdent = set_algebra.Rules.[4]

    let ReduceComp = set_algebra.Rules.[5]

    let Distrib = set_algebra.Rules.[6]