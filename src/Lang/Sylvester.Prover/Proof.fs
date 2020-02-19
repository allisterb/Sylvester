namespace Sylvester

open Microsoft.FSharp.Quotations

type Axioms = (Expr * Expr -> bool)

type Rule = Rule of string * (Expr * Expr -> Expr * Expr) with
    member x.Name = let (Rule(n, _)) = x in n
    member x.Apply = let (Rule(_, r)) = x in r
    
type Rules = Rule list 

type ProofSystem(axioms: Axioms, rules: Rules) =
    member val Axioms = axioms
    member val Rules = rules
    member x.AxiomaticallyEquivalent a b = x.Axioms (a, b)     
    static member (|-) ((c:ProofSystem), (a, b)) = c.AxiomaticallyEquivalent a b 

type Proof(system: ProofSystem, a:Expr,  b:Expr,  steps: Rule list) =
    let ruleNames = system.Rules |> List.map (fun r -> r.Name)
    let stepNames = steps |> List.map (fun r -> r.Name)
    let mutable astate, bstate = (a, b)
    let mutable stepCount = 0
    do printfn "Proof"
    do while stepCount < steps.Length do
        if not(List.contains stepNames.[stepCount] ruleNames) then
            printfn "Step %i (%s) is not part of the rules of the proof system." stepCount stepNames.[stepCount]
            stepCount <- steps.Length
        else
            let (_a, _b) = steps.[stepCount].Apply (astate, bstate)
            printfn "(%s, %s) -> (%s, %s)" (decompile astate) (decompile bstate) (decompile _a) (decompile _b)
            astate <- _a
            bstate <- _b
            stepCount <- stepCount + 1
type Theorem<'t, 'u>(lhs:Formula<'t, 'u>, rhs:Formula<'t, 'u>, proof:Proof) = class end
   
module Proof =   
    let thm (lhs:Formula<_, _>) (rhs:Formula<_, _>) (proof) = Theorem(lhs, rhs, proof)