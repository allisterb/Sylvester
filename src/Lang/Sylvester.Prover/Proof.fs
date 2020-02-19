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
    do printfn "Proof of %s <=> %s." (decompile a) (decompile b)
    let mutable astate, bstate = (a, b)
    let mutable stepCount = 0
    do while stepCount < steps.Length do
        if not(List.contains stepNames.[stepCount] ruleNames) then
            failwithf "Step %i (%s) is not part of the rules of the proof system." stepCount stepNames.[stepCount]
        else
            let (_a, _b) = steps.[stepCount].Apply (astate, bstate)
            printfn "%i. %s: (%s, %s) -> (%s, %s)" stepCount stepNames.[stepCount] (decompile astate) (decompile bstate) (decompile _a) (decompile _b)
            astate <- _a
            bstate <- _b
            if system |- (astate, bstate) then 
                printfn "Proof complete." 
                stepCount <- steps.Length
            else
                printfn "Proof incomplete."
                stepCount <- stepCount + 1
    
    member val Complete = system |- (astate, bstate)
    member val A = a
    member val B = b
    member val System = system
    member val Steps = steps
    static member (|-) ((proof:Proof), (a, b)) = proof.A = a && proof.B = b && proof.Complete

type Theorem<'t, 'u>(a:Formula<'t, 'u>, b:Formula<'t, 'u>, proof:Proof) = 
    do if not (proof.A = a.Expr && proof.B = b.Expr) then failwithf "The provided proof is not a proof of %s<=>%s" (a.Src) (b.Src)
    do if not (proof.Complete) then failwithf "The provided proof of %s<=>%s is not complete." (a.Src) (b.Src)
    member val A = a
    member val B = b
    member val Proof = proof
   
module Proof =   
    let thm (lhs:Formula<_, _>) (rhs:Formula<_, _>) (proof) = Theorem(lhs, rhs, proof)