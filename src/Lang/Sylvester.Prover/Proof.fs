namespace Sylph

open FSharp.Quotations

open Sylvester
open FormulaPatterns
open EquationalLogic

type Theory(axioms: Axioms, rules: Rules, ?formulaPrinter:string->string) =
    member val Axioms = axioms
    member val Rules = rules
    member val FormulaPrinter = defaultArg formulaPrinter id
    member x.AxiomaticallyEqual a b = x.Axioms (a, b) |> Option.isSome
    static member (|-) ((c:Theory), (a, b)) = c.AxiomaticallyEqual a b
   
    /// The default logical theory used in Sylph proofs.
    static member val S =     
        let S_Reduce = Rule("Reduce logical constants in (expression)", reduce_constants)

        let S_LeftAssoc = Rule("Logical operators in (expression) are left-associative", left_assoc)
        
        let S_RightAssoc = Rule("Logical operators in (expression) are right-associative", right_assoc)
          
        let S_Commute = Rule("Logical operators in (expression) are commutative", commute)

        let S_Distrib = Rule("Distribute logical terms in (expression)", distrib)
        
        let S_Collect = Rule("Collect distributed logical terms in (expression)", collect)

        let S_Idemp = Rule("Substitute idempotent logical terms in (expression)", idemp)

        let S_ExcludedMiddle = Rule("Logical terms in (expression) satisfy the law of excluded middle", excluded_middle)

        let S_GoldenRule = Rule("Logical terms in (expression) satisfy the golden rule", golden_rule)

        Theory(logical_axioms, [
            S_Reduce
            S_LeftAssoc
            S_RightAssoc
            S_Commute
            S_Distrib
            S_Collect
            S_Idemp
            S_ExcludedMiddle
            S_GoldenRule
        ], print_S_Operators)

    static member val internal Trivial = 
        Theory((fun (_:Expr,_:Expr) ->  Some(axiom_desc "Assumption" <@fun x y -> x = y @>)), [])

and Axioms = (Expr * Expr -> AxiomDescription option)

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

and Proof internal(a:Expr,  b:Expr, theory: Theory, steps: RuleApplication list, ?quiet:bool) =
    /// The logical theory used for the proof.
    static let mutable L:Theory = Theory.S
    let ruleNames = List.concat [
            (L.Rules: Rule list) |> List.map (fun (r:Rule) -> r.Name) 
            theory.Rules |> List.map (fun (r:Rule) -> r.Name)
        ]
    let stepNames: string list = steps |> List.map (fun r -> r.RuleName)
    
    let logBuilder = System.Text.StringBuilder()
    let q = defaultArg quiet false
    let output (s:string) = 
        match defaultDisplay with
            | Text -> printfn "%s" (theory.FormulaPrinter s)
            | _ -> printfn "%s" (theory.FormulaPrinter s)

    let prooflog (x:string) = 
        do 
            logBuilder.Append(x) |> ignore
            if not q then output x
    
    let mutable astate, bstate = (a, b)
    let mutable state:(Expr * Expr * string) list = [] 
    let mutable stepCount = 0
    do 
        if theory.GetType() = typeof<Assumption> then 
            do sprintf "Assume A: %s == B: %s:" (src a) (src b) |> prooflog
            stepCount <- steps.Length
        else
            do sprintf "Proof of A: %s == B: %s:" (src a) (src b) |> prooflog
            do
                if L |- (a, b)  then
                   let axeq = L.Axioms (a, b)
                   sprintf "|- %s == %s. [Logical Axiom of %s]" (src a) (src b) axeq.Value.Name |> prooflog
                   sprintf "Proof complete." |> prooflog
                   stepCount <- steps.Length               
                if theory |- (a, b)  then
                    let axeq = theory.Axioms (a, b)
                    sprintf "|- %s == %s. [Axiom of %s]" (src a) (src b) axeq.Value.Name |> prooflog
                    sprintf "Proof complete." |> prooflog
                    stepCount <- steps.Length
    do while stepCount < steps.Length do
        let step = steps.[stepCount]
        let stepId = stepCount + 1
        let stepName = stepNames.[stepCount]
        do 
            match step.Rule with
            | (Rule(n, d)) -> if (not (ruleNames |> List.contains (n))) then failwithf "Rule at step %i (%s) is not a logical inference rule or part of the rules of the current theory." stepId n
            | (Subst(n, p, _)) -> if not ((p.Theory = L) || (p.Theory = theory) || (p.Theory.GetType().IsAssignableFrom(theory.GetType()))) then failwithf "Substitution rule at step %i (%s) does not use the rules of L or the current theory." stepId n
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
        if L |- (astate, bstate) then 
            let axeq = L.Axioms (astate, bstate)
            sprintf "|- %s == %s. [Logical Axiom of %s]" (src astate) (src bstate) axeq.Value.Name |> prooflog
            sprintf "Proof complete." |> prooflog
            stepCount <- steps.Length
        else if theory |- (astate, bstate) then
            let axeq = theory.Axioms (astate, bstate)
            sprintf "|- %s == %s. [Axiom of %s]" (src astate) (src bstate) axeq.Value.Name |> prooflog
            sprintf "Proof complete." |> prooflog 
            stepCount <- steps.Length
        else
            sprintf "Proof incomplete. Current state: %s == %s." (src astate) (src bstate) |> prooflog
            stepCount <- stepCount + 1

    do if stepCount = 0 && not(theory |- (a, b) || L |- (a, b)) then
        sprintf "Proof incomplete. Current state: %s == %s." (src astate) (src bstate) |> prooflog
    member val A = a
    member val B = b
    member val Theory = theory
    member val Steps = steps
    abstract Complete:bool
    default val Complete = L |- (astate, bstate) || theory |- (astate, bstate)
    member val State = state
    member val AState = astate
    member val BState = bstate
    member val Subst = steps |> List.map (fun s  -> s.Apply) |> List.fold(fun e r -> e >> r) id
    member val Log = logBuilder

    /// The default logical theory used by proofs. Defaults to S but can be changed to something else.
    static member Logic with get() = L and set(v) = L <- v
    static member (|-) ((proof:Proof), (a, b)) = proof.A = a && proof.B = b && proof.Complete 
    
    static member (+) (l:Proof, r:RuleApplication) = if l.Complete then failwith "Cannot add a step to a completed proof." else Proof(l.A, l.B, l.Theory, l.Steps @ [r])
    static member (+) (l:Proof, r:Proof) = 
        let rec subst (p:Proof) = 
            function 
            | A when (sequal (p.A) (A)) && p.Complete -> p.B  
            | expr -> traverse expr (subst p)
        let (last_l_astate, _, _) = l.State |> Seq.last
        if l.Theory = r.Theory then 
            if sequal last_l_astate r.A  then
                let l2 = Proof(l.A, last_l_astate, l.Theory, l.Steps, true) in
                let s = Subst(sprintf "Joining proof of %s == %s to proof of %s == %s" (src l.A) (src last_l_astate) (src r.A) (src r.B), l2, fun proof e -> subst proof e) |> EntireA in
                Proof(l.A, r.B, l.Theory, s::r.Steps)  
            else failwith "Cannot join these proofs. The RHS of the first proof is not the LHS of the 2nd proof."
        else
            failwith "Cannot join these proofs because they use different theories."
    interface IDisplay with
        member x.Output(item:'t) = item.ToString()
        member x.Transform(str:string) = str

and Assumption internal(a:Expr,  b:Expr) = inherit Proof(a, b, Theory.Trivial, [], true)

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
 
type Theorem internal (stmt: TheoremStmt, proof:Proof) = 
    let (a, b) = stmt.Members
    do if not ((sequal proof.A a) && (sequal proof.B b)) then failwithf "The provided proof is not a proof of %s==%s" (src a) (src b)
    do if not proof.Complete then failwithf "The provided proof of %s==%s is not complete." (src a) (src b)
    member val A = a
    member val B = b
    member val Proof = proof
       
and TheoremStmt = 
    | Taut of Expr
    | Contr of Expr
    | Ident of Expr
    with
    member x.Members = 
        match x with
        | Taut a -> a, True.Raw
        | Contr a -> a, False.Raw
        | Ident a -> match a with | Equiv(l, r) -> l, r | _ -> failwithf "The formula %A is not an identity." (src a)

[<AutoOpen>]
module LogicalRules = 
    /// The default theory of equational logic that defines the logical axioms and inference rules for proofs.
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

    /// Substitute a theorem with a completed proof into another proof.
    let Lemma (lemma:Theorem) = lemma.Proof |> Subst 
       
    /// Substitute an assumption.
    let Assume (a,b) = Assumption(a, b) |> Subst 
       
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

    /// Substitute idempotent logical terms in expression.
    let Idemp' = S.Rules.[6]

    /// Logical expression satisfies law of excluded middle.
    let ExcludedMiddle = S.Rules.[7]

    /// Logical expression satisfies golden rule.
    let GoldenRule = S.Rules.[8]

[<AutoOpen>]
module Proof =        
    let proof (e:Expr<'t>) theory steps =         
        let f = e |> expand |> body
        do if not (range_type typeof<'t> = typeof<bool>) then failwithf "The formula %A does not have a truth value." (src f)
        match f with 
        | Implies(l, r) -> Proof(f, True, theory, steps)
        | Equiv(l, r) -> Proof(l, r, theory, steps)
        | _ -> failwithf "The expression %A is not recognized as a theorem statement." (src f)  
    let theorem theory steps (e:Expr<'t>) = 
        let f = e |> expand |> body
        do if not (range_type typeof<'t> = typeof<bool>) then failwithf "The formula %A does not have a truth value." (src f)
        Theorem(Taut(f), Proof (f, True, theory, steps))  
    let contr theory steps (e:Expr<'t>) = 
        let f = e |> expand |> body
        do if not (range_type typeof<'t> = typeof<bool>) then failwithf "The formula %A does not have a truth value." (src f)
        Theorem((Contr f), Proof (f, False, theory, steps))
    let ident theory steps  (e:Expr<_>) = 
        let f = e |> expand |> body
        match f with 
        | Equiv(l, r) -> Theorem((Ident f), Proof(l, r, theory, steps)) 
        | _ -> failwithf "The expression %A is not recognized as an identity." (src f)  
    let axiom theory e = theorem theory [] e
    let ident_axiom theory e = ident theory [] e

    let logical_contr steps f = contr Proof.Logic steps f
    let logical_theorem steps f = theorem Proof.Logic steps f
    let logical_axiom e = axiom Proof.Logic e
    let logical_ident steps f = ident Proof.Logic steps f
    let logical_ident_axiom e = logical_ident [] e