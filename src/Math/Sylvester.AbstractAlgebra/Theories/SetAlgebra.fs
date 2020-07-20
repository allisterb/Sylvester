namespace Sylvester

open FSharp.Quotations

open BooleanAlgebra

module SetAlgebra =
        
    (* Symbols *)
    do 
        Symbols.BulitIn.Add(src <@ Empty@>, "\u2205")
        Symbols.BulitIn.Add(src <@ U @>, "\U0001D54C")
    
    (* Formulas *)

    /// n-ary union of sets
    [<Formula>]
    let union<'t when 't : equality> (bound:int) (range:bool) (body:Set<'t>) = sum Set.(|+|) "\u22c3" bound range body

    /// n-ary intersection of sets
    [<Formula>]
    let intersect<'t when 't : equality> (bound:int) (range:bool) (body:Set<'t>) = product Set.(|*|) "\u22c3" bound range body
    
    (* Theory *)

    let set_algebra<'t when 't: equality> = BooleanAlgebraTheory("Set Algebra", <@ Set.(|+|) @>, <@ Set.(|*|) @>, <@ Set.Empty @>, <@ Set.U @>, <@ Set.(~-) @>)

    (* Admissible Rules *)
    
    let left_assoc = set_algebra.Rules.[0]

    let right_assoc = set_algebra.Rules.[1]

    let commute = set_algebra.Rules.[2]

    let idemp = set_algebra.Rules.[3]

    let ident_set = set_algebra.Rules.[4]

    let comp = set_algebra.Rules.[5]

    let distrib = set_algebra.Rules.[6]