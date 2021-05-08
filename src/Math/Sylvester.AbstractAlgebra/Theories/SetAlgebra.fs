namespace Sylvester

open FSharp.Quotations

open BooleanAlgebra

/// Theory of set algebra based on a generic Boolean algebra.
module SetAlgebra =
        
    (* Symbols *)
    do 
        Symbols.BulitIn.Add(src <@ Empty @>, "\u2205")
        Symbols.BulitIn.Add(src <@ Set.U @>, "\U0001D54C")
    
    (* Formulas *)

    /// n-ary union of sets
    [<Formula>]
    let union<'t when 't : equality> (bound:int) (range:bool) (body:Set<'t>) = formula<Set<'t>>

    /// n-ary intersection of sets
    [<Formula>]
    let intersect<'t when 't : equality> (bound:int) (range:bool) (body:Set<'t>) = product Set.(|*|) "\u22c2" bound range body
    
    (* Theory *)

    type SetAlgebra<'t when 't: equality>(?axioms:Axioms, ?rules:Rules) = 
        inherit BooleanAlgebra<Set<'t>>("Set Algebra", <@ Set.(|+|) @>, <@ Set.(|*|) @>, <@ Set.Empty @>, <@ Set.U @>, <@ Set.(~-) @>, 
            defaultArg axioms (fun _-> None), defaultArg rules [])
    
    let set_algebra<'t when 't: equality> = SetAlgebra<'t>()

    (* Admissible Rules *)
    
    let left_assoc<'t when 't : equality> = set_algebra<'t>.Rules.[0]

    let right_assoc<'t when 't : equality> = set_algebra<'t>.Rules.[1]

    let commute<'t when 't : equality> = set_algebra<'t>.Rules.[2]

    let idemp<'t when 't : equality> = set_algebra<'t>.Rules.[3]

    let ident_set<'t when 't : equality> = set_algebra<'t>.Rules.[4]

    let comp<'t when 't : equality> = set_algebra<'t>.Rules.[5]

    let distrib<'t when 't : equality> = set_algebra<'t>.Rules.[6]