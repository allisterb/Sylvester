namespace Sylvester

open System

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

open FormulaPatterns

type Axioms = (Expr * Expr->Expr option)

type Rules = (Expr * Expr ->Expr * Expr) list 

type ProofCalculus =  ProofCalculus of Axioms * Rules with
    member x.Axioms = let (ProofCalculus(a, _)) = x in a
    member x.Rules = let (ProofCalculus(_, r)) = x in r
    member x.AxiomaticEquality a b = match x.Axioms (a, b) with | Some _ -> true | None -> false
    static member (|-) ((c:ProofCalculus), (a, b)) = c.AxiomaticEquality a b 

type ProofStep = Expr list -> Expr list

type Proof = ProofCalculus * Expr list * ProofStep list

type Theorem<'t, 'u>(lhs:Formula<'t, 'u>, rhs:Formula<'t, 'u>, proof:Proof) = class end
   
module Proof =   
    let axiomatic_equality (c:ProofCalculus) (a:Formula<'t, 'u>) (b:Formula<'t, 'u>) = c.AxiomaticEquality a.Expr b.Expr
    
    let thm (lhs:Formula<_, _>) (rhs:Formula<_, _>) (proof) = Theorem(lhs, rhs, proof)