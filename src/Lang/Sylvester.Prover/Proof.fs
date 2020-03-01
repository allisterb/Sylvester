namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester
open Operators
open EquationalLogic

type Proof(a:Expr,  b:Expr, theory: Theory, steps: RuleApplication list, ?quiet:bool) =
    let S:Theory = Theory.S
    let ruleNames = List.concat [
            (S.Rules: Rule list) |> List.map (fun (r:Rule) -> r.Name) 
            theory.Rules |> List.map (fun (r:Rule) -> r.Name)
        ]
    let stepNames: string list = steps |> List.map (fun r -> r.RuleName)
    
    let logBuilder = System.Text.StringBuilder()
    let q = defaultArg quiet false
    let prooflog (x:string) = 
        do 
            logBuilder.Append(x) |> ignore
            if not q then printfn "%s" x
    
    let mutable astate, bstate = (a, b)
    let mutable state:(Expr * Expr * string) list = [] 
    let mutable stepCount = 0

    do sprintf "Proof of A: %s == B: %s:" (src a) (src b) |> prooflog
    do 
        if S |- (a, b) || theory |- (a, b)  then
           sprintf "|- %s == %s" (src a) (src b) |> prooflog
           sprintf "Proof complete." |> prooflog
           stepCount <- steps.Length
    
    do while stepCount < steps.Length do
        let step = steps.[stepCount]
        let stepId = stepCount + 1
        let stepName = stepNames.[stepCount]
        do 
            match step.Rule with
            | (Rule(n, _)) -> if (not (ruleNames |> List.contains n)) then failwithf "Rule at step %i (%s) is not a logical inference rule or part of the rules of the current theory." stepId n
            | (Subst(n, p, _)) -> if not (p.System = S || (p.System = theory)) then failwithf "Substitution rule at step %i (%s) does not use the rules of S or the current theory." stepId n
        let (_a, _b) = step.Apply (astate, bstate)
        let msg =
            if (not ((sequal _a astate)) && (not (sequal _b bstate))) then
                sprintf "%i. %s: (%s, %s) == (%s, %s)." (stepId) (stepName.Replace("(expression)", "A and B")) (src astate) (src bstate) (src _a) (src _b)
            else if not (sequal _a astate) then
                sprintf "%i. %s: %s == %s." (stepId) (stepName.Replace("(expression)", "A")) (src astate) (src _a)
            else if not (sequal _b bstate) then
                sprintf "%i. %s: %s == %s." (stepId) (stepName.Replace("(expression)", "B")) (src bstate) (src _b)
            else
                sprintf "%i. %s: No change." (stepId) (stepName.Replace("(expression)", "A or B")) 
        do prooflog msg
        astate <- _a
        bstate <- _b
        state <- state @ [(astate, bstate, msg)]
        if theory |- (astate, bstate) then 
            sprintf "Proof complete." |> prooflog 
            stepCount <- steps.Length
        else
            sprintf "Proof incomplete. Current state: %s == %s." (src astate) (src bstate) |> prooflog
            stepCount <- stepCount + 1

    do if stepCount = 0 && not(theory |- (a, b)) then
        sprintf "Proof incomplete. Current state: %s == %s." (src astate) (src bstate) |> prooflog
    member val A = a
    member val B = b
    member val System = theory
    member val Steps = steps
    member val Complete = theory |- (astate, bstate)
    member val State = state
    member val AState = astate
    member val BState = bstate
    member val Subst = steps |> List.map (fun s  -> s.Apply) |> List.fold(fun e r -> e >> r) id
    member val Log = logBuilder
    static member (|-) ((proof:Proof), (a, b)) = proof.A = a && proof.B = b && proof.Complete
    static member (|-) ((proof:Proof), (A:Formula<'t>, B:Formula<'v>)) = proof.A = A.Expr && proof.B = B.Expr && proof.Complete
    
    static member (+) (l:Proof, r:RuleApplication) = if l.Complete then failwith "Cannot add a step to a completed proof." else Proof(l.A, l.B, l.System, l.Steps @ [r])
    static member (+) (l:Proof, r:Proof) = 
        let rec subst (p:Proof) = 
            function 
            | A when (sequal (p.A) (A)) && p.Complete -> p.B  
            | expr -> traverse expr (subst p)
        let (last_l_astate, _, _) = l.State |> Seq.last
        if l.System = r.System then 
            if sequal last_l_astate r.A  then
                let l2 = Proof(l.A, last_l_astate, l.System, l.Steps, true) in
                let s = Subst(sprintf "Joining proof of %s == %s to proof of %s == %s" (src l.A) (src last_l_astate) (src r.A) (src r.B), l2, fun proof e -> subst proof e) |> EntireA in
                Proof(l.A, r.B, l.System, s::r.Steps)  
            else failwith "Cannot join these proofs. The RHS of the first proof is not the LHS of the 2nd proof."
        else
            failwith "Cannot join these proofs because they use different theories."
        
and Axioms = (Expr * Expr -> bool)

and AxiomDescription = AxiomDescription of string * int * string

and Rule = 
    | Rule of string * (Expr -> Expr ) 
    | Subst of string * Proof * (Proof -> Expr -> Expr)
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

and RuleApplication =
    | EntireA of Rule
    | EntireB of Rule
    | LeftA of Rule
    | LeftB of Rule
    | RightA of Rule
    | RightB of Rule
with
    member x.Rule = 
        match x with
        | EntireA rule -> rule
        | EntireB rule -> rule
        | LeftA rule -> rule
        | LeftB rule -> rule
        | RightA rule -> rule
        | RightB rule -> rule
    member x.RuleName = x.Rule.Name
    member x.Apply(a:Expr, b:Expr) =       
        match x with
        | EntireA rule -> rule.Apply a, b
        | EntireB rule -> a, rule.Apply b
        | LeftA rule -> 
                match a with
                | Patterns.Call(o, m, l::r::[]) -> let s = rule.Apply l in binary_call(o, m, s, r), b
                | _ -> failwith "A is not a binary operation."
        | LeftB rule -> 
                match b with
                | Patterns.Call(o, m, l::r::[]) -> let s = rule.Apply l in a, binary_call(o, m, s, r)
                | _ -> failwith "B is not a binary operation."

        | RightA rule -> 
                match a with
                | Patterns.Call(o, m, l::r::[]) -> let s = rule.Apply r in binary_call(o, m, l, s), b
                | _ -> failwithf "A is not a binary operation: %s." (src a)
        | RightB rule -> 
                match b with
                | Patterns.Call(o, m, l::r::[]) -> let s = rule.Apply r in a, binary_call(o, m, l, s)
                | _ -> failwithf "B is not a binary operation: %s." (src a)
 
and Theory(axioms: Axioms, rules: Rules) =
    member val Axioms = axioms
    member val Rules = rules
    member x.AxiomaticallyEquivalent a b = x.Axioms (a, b)
    static member (|-) ((c:Theory), (a, b)) = c.AxiomaticallyEquivalent a b
    static member (|-) ((c:Theory), (a, b):Formula<_> * Formula<_>) = c.AxiomaticallyEquivalent a.Expr b.Expr

    /// The theory of equational logic used by Sylph.
    static member val S =    
        /// Reduce logical constants in expression. 
        let S_Reduce = Rule("Reduce logical constants in (expression)", reduce_constants)

        /// Logical operators in expression are left-associative.
        let S_LeftAssoc = Rule("Logical operators in (expression) are left-associative", left_assoc)
        
        /// Logical operators in expression are right associative.
        let S_RightAssoc = Rule("Logical operators in (expression) are right-associative", right_assoc)
          
        /// Logical operators in expression commute.
        let S_Commute = Rule("Logical operators in (expression) are commutative", commute)

        /// distribute lgical tems in over addition in expression.
        let S_Distrib = Rule("Distribute logical terms in (expression)", distrib)
        
        /// Collect distributed terms in expression.
        let S_Collect = Rule("Collect distributed logical terms in (expression)", collect)

        /// Substitute identical terms in expression.
        let S_Ident = Rule("Substitute identical logical terms in (expression)", ident)

        /// Substitue idempotent terms in expression.
        let S_Idemp = Rule("Substitute idempotent logical terms in (expression)", idemp)

        /// Replace identical terms in expression.
        let S_ExcludedMiddle = Rule("Logical terms in (expression) satisfy the law of excluded middle", excluded_middle)

        /// Replace identical terms in expression.
        let S_GoldenRule = Rule("Logical terms in (expression) satisfy the golden rule", golden_rule)

        Theory(logical_axioms, [
            S_Reduce
            S_LeftAssoc
            S_RightAssoc
            S_Commute
            S_Distrib
            S_Collect
            S_Ident
            S_Idemp
            S_ExcludedMiddle
            S_GoldenRule
        ])

type Theorem<'t>(stmt:TheoremStmt<'t>, proof:Proof) = 
    let (a, b) = stmt.Members
    do if not ((sequal proof.A a) && (sequal proof.B b)) then failwithf "The provided proof is not a proof of %s==%s" (src a) (src b)
    do if not (proof.Complete) then failwithf "The provided proof of %s==%s is not complete." (src a) (src b)
    member val A = a
    member val B = b
    member val System = proof.System
    member val Proof = proof
    
 and TheoremStmt<'t> = 
    | Equivalence of Formula<'t> * Formula<'t>
    | Tautology of Formula<'t> 
    | Contradiction of Formula<'t>
with
    member x.Members =
        match x with
        | Equivalence (a, b) -> a.Expr, b.Expr
        | Tautology t -> t.Expr, Prop.T.Expr
        | Contradiction f -> f.Expr, Prop.F.Expr

[<AutoOpen>]
module LogicalRules = 
    /// The theory of equational logic that defines the logical axioms and inference rules for proofs.
    let S = Theory.S

    let rec subst (p:Proof) = 
        function
        | A when (sequal (p.A) (A)) && p.Complete -> p.B  
        | expr -> traverse expr (subst p)
    
    /// Leibniz's rule : A behaves equivalently in a formula if we substitute a part of A: a with x when x == a.
    let Subst (p:Proof) = 
        if not p.Complete then 
            failwithf "The proof of %A == %A is not complete" (src p.A) (src p.B) 
        else Subst(sprintf "Substitute %s in A with %s" (src p.A) (src p.B), p, fun proof e -> subst proof e) 

    /// Substitute a formula with the form a = b that is true in a theory.
    let Lemma (a:Expr<'t>) theory steps  = 
        match a |> body with 
        | Equiv(l, r) -> Proof(l, r, theory, steps) |> Subst 
        | _ -> failwithf "The expression %A is not recognized as a valid expression for substitution." (src a)  

    /// /// Substitute a logical formula with the form a = b that is axiomatically true in s.
    let Lemma' (a:Expr<'t>) steps  = Lemma a S steps 

    /// Substitute a formula of the form a = b that is axiomatically true in a theory.
    let Axiom (a:Expr<'t>) = 
        match (a |> body) with
        | Equiv(l, r) -> let p = Proof(l, r, S, [], true) in Subst p
        | _ -> failwithf "The expression %A is not recognized as a valid expression for substitution." (src a)

    /// Substitute a logical formula of the form a = b that is axiomatically true.
    let Axiom' (a:Expr<'t>) = 
        match (a |> body) with
        | Equiv(l, r) -> let p = Proof(l, r, S, [], true) in Subst p
        | _ -> failwithf "The expression %A is not recognized as a valid logical expression for substitution." (src a)
        
    /// Reduce logical constants in expression. 
    let Reduce' = S.Rules.[0]

    /// Logical expression is left associative.
    let LeftAssoc' = S.Rules.[1]
    
    /// Logical expression is right associative.
    let RightAssoc' = S.Rules.[2]
      
    /// Logical expression is commutative.
    let Commute' = S.Rules.[3]

    /// Distribute logical terms in expression.
    let Distrib' = S.Rules.[4]
    
    /// Collect distributed logical terms in expression.
    let Collect' = S.Rules.[5]

    /// Substitute identical logical terms in expression.
    let Ident' = S.Rules.[6]

    /// Substitute idempotent logical terms in expression.
    let Idemp' = S.Rules.[7]

    /// Logical expression satisfies law of excluded middle.
    let ExcludedMiddle = S.Rules.[8]

    /// Logical expression satisfies golden rule.
    let GoldenRule = S.Rules.[9]

[<AutoOpen>]
module Proof =     
    let True = Prop.T
    let False = Prop.F
    let inline taut f = f == True
    let inline contra f = f == False

    let theory axioms rules = Theory(axioms, rules)
    let proof' a b theory steps = Proof(a, b, theory, steps)
    let proof (a:Formula<_>, b:Formula<_>) (theory: Theory) (steps: RuleApplication list) = proof' a.Expr b.Expr theory steps
    let axiom (a, b) theory  = proof (a,b) theory []
    let axiom' expr  = 
        match expr |> body with 
        | Equiv(l, r) -> proof' l r S [] 
        | _ -> failwithf "The expression %A is not recognized as a valid logical expression for substitution." (src expr)  
    let lemma expr theory steps  = 
        match expr |> body with 
        | Equiv(l, r) -> proof' l r theory steps 
        | _ -> failwithf "The expression %A is not recognized as a valid expression for substitution." (src expr)  
    let lemma' expr steps  = lemma expr S steps
    let theorem (stmt:TheoremStmt<_>) (proof) = Theorem(stmt, proof)