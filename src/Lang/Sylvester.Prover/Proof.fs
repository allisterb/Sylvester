namespace Sylvester

open Microsoft.FSharp.Quotations

type Axioms = (Expr * Expr -> bool)

type Rules = (Expr * Expr -> Expr * Expr) list 

type ProofSystem(axioms: Axioms, rules: Rules) =
    let axiomStep (a,b) = match axioms (a, b) with | true -> (a, b) |  false -> (CONT.Expr, CONT.Expr)
    member x.AxiomStep(a, b) = axiomStep a,b
    member val Axioms = axioms
    member val Rules = axiomStep :: rules
    member x.AxiomaticallyEquivalent a b = x.Axioms (a, b)     
    static member (|-) ((c:ProofSystem), (a, b)) = c.AxiomaticallyEquivalent a b 

type ProofStep = (Expr * Expr) -> (Expr * Expr)

type Proof(system: ProofSystem, a:Expr,  b:Expr,  steps: ProofStep list) =
    let ruleNames = system.Rules |> List.map (fun r -> r.ToString())
    let stepNames = steps |> List.map (fun r -> r.ToString())
    do if stepNames |> Seq.forall(fun step -> List.contains step ruleNames) then
        do stepNames |> Seq.iteri (fun i p -> if ruleNames |> List.contains p |> not then printf "%i" i)

type Theorem<'t, 'u>(lhs:Formula<'t, 'u>, rhs:Formula<'t, 'u>, proof:Proof) = class end
   
module Proof =   
    let thm (lhs:Formula<_, _>) (rhs:Formula<_, _>) (proof) = Theorem(lhs, rhs, proof)