namespace Sylph

open Microsoft.FSharp.Quotations

open Sylvester

type Proof(a:Expr,  b:Expr, system: ProofSystem, steps: Rule list, ?quiet:bool) =
    let logBuilder = System.Text.StringBuilder()
    let ruleNames = system.Rules |> List.map (fun (r:Rule) -> r.Name)
    let stepNames = steps |> List.map (fun r -> r.Name)
    let q = defaultArg quiet false
    let prooflog (x:string) = 
        do 
            logBuilder.Append(x) |> ignore
            if not q then printfn "%s" x
    do sprintf "Proof of A: %s <=> B: %s:" (src a) (src b) |> prooflog
    do if system |- (a, b) then
        sprintf "|- %s <=> %s" (src a) (src b) |> prooflog
        sprintf "Proof complete." |> prooflog
    let mutable astate, bstate = (a, b)
    let mutable state:(Expr * Expr * string) list = [] 
    let mutable stepCount = 0
    do while stepCount < steps.Length do
        let step = steps.[stepCount]
        let stepId = stepCount + 1
        do 
            match step with
            | (Rule(n, _)) -> if not(ruleNames |> List.contains n) then failwithf "Rule at step %i (%s) is not part of the rules of the current proof system." stepId n
            | (Subst(n, p, _)) -> if not(p.System = system) then failwithf "Substitution rule at step %i (%s) does not use the rules of the current proof system." stepId n
        let (_a, _b) = step.Apply (astate, bstate)
        let msg =
            if (not ((sequal _a astate)) && (not (sequal _b bstate))) then
                sprintf "%i. %s: (%s, %s) <=> (%s, %s)" (stepId) stepNames.[stepCount] (src astate) (src bstate) (src _a) (src _b)
            else if not (sequal _a astate) then
                sprintf "%i. %s: %s <=> %s" (stepId) stepNames.[stepCount] (src astate) (src _a)
            else if not (sequal _b bstate) then
                sprintf "%i. %s: %s <=> %s" (stepId) stepNames.[stepCount] (src bstate) (src _b)
            else
                sprintf "%i. %s: No change." (stepId) stepNames.[stepCount] 
        prooflog msg
        astate <- _a
        bstate <- _b
        state <- state @ [(astate, bstate, msg)]
        if system |- (astate, bstate) then 
            sprintf "Proof complete." |> prooflog 
            stepCount <- steps.Length
        else
            sprintf "Proof incomplete." |> prooflog
            stepCount <- stepCount + 1
    do if stepCount = 0 && not(system |- (a, b)) then
        sprintf "Proof incomplete." |> prooflog
    member val A = a
    member val B = b
    member val System = system
    member val Steps = steps
    member val Complete = system |- (astate, bstate)
    member val State = state
    member val Log = logBuilder
    static member (|-) ((proof:Proof), (a, b)) = proof.A = a && proof.B = b && proof.Complete
    static member (|-) ((proof:Proof), (A:Formula<'t,'u>, B:Formula<'v,'w>)) = proof.A = A.Expr && proof.B = B.Expr && proof.Complete

    static member (+) (l:Proof, r:Rule) = if l.Complete then failwith "Cannot add a rule to a completed proof." else Proof(l.A, l.B, l.System, l.Steps @ [r])

    static member (+) (l:Proof, r:Proof) = 
        let rec subst (p:Proof) = 
            function 
            | A when (sequal (p.A) (A)) && p.Complete -> p.B  
            | expr -> traverse expr (subst p)
        let (last_l_astate, _, _) = l.State |> Seq.last
        if l.System = r.System then 
            if sequal last_l_astate r.A  then
                let l2 = Proof(l.A, last_l_astate, l.System, l.Steps, true) in
                let s = Subst(sprintf "Joining proof of %s <=> %s to proof of %s <=> %s" (src l.A) (src last_l_astate) (src r.A) (src r.B), l2, fun proof (a,b) -> (subst proof a), b) in
                
                Proof(l.A, r.B, l.System, s::r.Steps)  
            else failwith "Cannot create proof from these proofs. The RHS of the first proof is not the LHS of the 2nd proof."
        else
            failwith "Cannot create proof from these proofs because they use different proof systems."
        
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
    static member (|-) ((c:ProofSystem), (a, b):Formula<_,_> * Formula<_,_>) = c.AxiomaticallyEquivalent a.Expr b.Expr

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