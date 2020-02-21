namespace Sylvester

open Microsoft.FSharp.Quotations

type Proof(a:Expr,  b:Expr, system: ProofSystem, steps: Rule list) =
    let ruleNames = system.Rules |> List.map (fun (r:Rule) -> r.Name)
    let stepNames = steps |> List.map (fun r -> r.Name)
    do printfn "Proof of A: %s <=> B: %s:" (src a) (src b)
    do if system |- (a, b) then
        printfn "|- %s <=> %s" (src a) (src b)
        printfn "Proof complete."
    let mutable astate, bstate = (a, b)
    let mutable stepCount = 0
    do while stepCount < steps.Length do
        let step = steps.[stepCount]
        do 
            match step with
            | (Rule(n, _)) -> if not(ruleNames |> List.contains n) then failwithf "Rule at step %i (%s) is not part of the rules of the current proof system." stepCount n
            | (Subst(n, p, _)) -> if not(p.System = system) then failwithf "Substitution rule at step %i (%s) does not use the rules of the current proof system." stepCount n
        let (_a, _b) = step.Apply (astate, bstate)
        if (not ((sequal _a astate)) && (not (sequal _b bstate))) then
            printfn "%i. %s: (%s, %s) <=> (%s, %s)" (stepCount + 1) stepNames.[stepCount] (src astate) (src bstate) (src _a) (src _b)
        else if not (sequal _a astate) then
            printfn "%i. %s: %s <=> %s" (stepCount + 1) stepNames.[stepCount] (src astate) (src _a)
        else if not (sequal _b bstate) then
            printfn "%i. %s: %s <=> %s" (stepCount + 1) stepNames.[stepCount] (src bstate) (src _b)
        else
            printfn "%i. %s: No change." (stepCount + 1) stepNames.[stepCount] 
        astate <- _a
        bstate <- _b
        if system |- (astate, bstate) then 
            printfn "Proof complete." 
            stepCount <- steps.Length
        else
            printfn "Proof incomplete."
            stepCount <- stepCount + 1
    
    member val A = a
    member val B = b
    member val System = system
    member val Steps = steps
    member val Complete = system |- (astate, bstate)
    static member (|-) ((proof:Proof), (a, b)) = proof.A = a && proof.B = b && proof.Complete
    static member (|-) ((proof:Proof), (A:Formula<'t,'u>, B:Formula<'v,'w>)) = proof.A = A.Expr && proof.B = B.Expr && proof.Complete

and Axioms = (Expr * Expr -> bool)

and Rule = 
    | Rule of string * (Expr * Expr -> Expr * Expr) 
    | Subst of string * Proof * (Proof -> (Expr * Expr -> Expr * Expr))
with
    member x.Name = 
        match x with 
        | (Rule(n, _)) -> n
        | (Subst(n, _, _)) -> n
    member x.Apply = 
        match x with
        | (Rule(_, r)) -> r
        | (Subst(_, p, r)) -> r p
       
and Rules = Rule list 

and ProofSystem(axioms: Axioms, rules: Rules) =
    member val Axioms = axioms
    member val Rules = rules
    member x.AxiomaticallyEquivalent a b = x.Axioms (a, b)     
    static member (|-) ((c:ProofSystem), (a, b)) = c.AxiomaticallyEquivalent a b

type Theorem<'t, 'u>(stms:TheoremStmt<'t, 'u>, proof:Proof) = 
    let (a, b) = stms
    do if not ((sequal proof.A a.Expr) && (sequal proof.B b.Expr)) then failwithf "The provided proof is not a proof of %s<=>%s" (a.Src) (b.Src)
    do if not (proof.Complete) then failwithf "The provided proof of %s<=>%s is not complete." (a.Src) (b.Src)
    member val A = a
    member val B = b
    member val Proof = proof
 
 and TheoremStmt<'t, 'u> = Formula<'t, 'u> * Formula<'t, 'u>

[<AutoOpen>]
module Proof =   
    let rec subst (p:Proof) = 
        function 
        | A when (sequal (p.A) (A)) && p.Complete -> p.B  
        | expr -> traverse expr (subst p)
    /// Substitute A with X when A <=> X.
    let subst_a (p:Proof) = Subst(sprintf "Substitute %s in A with %s" (src p.A) (src p.B), p, fun proof (a,b) -> (subst proof a), b) 
    /// Substitute B with X when B <=> X.
    let subst_b (p:Proof) = Subst(sprintf "Substitute %s in B with %s" (src p.A) (src p.B), p, fun proof (a, b) -> (a, subst proof b))

    let proof_system axioms rules = ProofSystem(axioms, rules)
    let proof' a b system steps = Proof(a, b, system, steps)
    let proof (a:Formula<_,_>, b:Formula<_,_>) (system: ProofSystem) (steps: Rule list) = proof' a.Expr b.Expr system steps
    let axiomatic (a,b) system  = proof (a,b) system []
    let axiomatic' a b system   = proof' a b system []
    let theorem (stmt:TheoremStmt<_,_>) (proof) = Theorem(stmt, proof)