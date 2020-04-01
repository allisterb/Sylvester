namespace Sylvester

open FSharp.Quotations

open Descriptions
open Patterns
open EquationalLogic

type Theory(axioms: Axioms, rules: Rules, ?formulaPrinter:string->string) =
    member val Axioms = axioms
    member val Rules = rules
    member val FormulaPrinter = defaultArg formulaPrinter id
    member x.AxiomaticallyEquiv a  = x.Axioms a |> Option.isSome  
    static member (|-) ((c:Theory), a) = c.AxiomaticallyEquiv a

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

        Theory(equational_logic_axioms, [
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
        Theory((fun (_:Expr) ->  Some(axiom_desc "Assumption" id (pattern_desc "Assumption" <@fun x y -> x = y @>))), [])

and Axioms = (Expr -> AxiomDescription option)

and Rule = 
    | Rule of string * (Expr -> Expr) 
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

and Proof internal(a:Expr, theory: Theory, steps: RuleApplication list, ?lemma:bool) =
    /// The logical theory used for the proof.
    static let mutable ProofLogic:Theory = Theory.S
    let ruleNames = List.concat [
            (ProofLogic.Rules: Rule list) |> List.map (fun (r:Rule) -> r.Name) 
            theory.Rules |> List.map (fun (r:Rule) -> r.Name)
        ]
    let stepNames: string list = steps |> List.map (fun r -> r.RuleName)
    
    let logBuilder = System.Text.StringBuilder()
    let l = defaultArg lemma false
    let proof_sep = if l then System.Environment.NewLine else ""
    let output (s:string) = 
        match defaultDisplay with
            | Text -> printfn "%s" ((ProofLogic.FormulaPrinter >> theory.FormulaPrinter) s)
            | _ -> printfn "%s" ((ProofLogic.FormulaPrinter >> theory.FormulaPrinter) s)

    let prooflog (x:string) = 
        do 
            logBuilder.Append(x) |> ignore
            if not l then output x else output ("[Lemma] " + x)
    
    let mutable _state = a
    let mutable state:(Expr * string) list = [] 
    let mutable stepCount = 0
    do 
        if theory.GetType() = typeof<Assumption> then 
            do sprintf "Assume %s." (src a) |> prooflog
            stepCount <- steps.Length
        else
            do sprintf "Proof of %s:" (src a) |> prooflog
            do
                if theory |- a  then
                    let axeq = theory.Axioms a
                    sprintf "|- %s. [%s]" (src a) (if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Axiom of %s" axeq.Value.Name) |> prooflog
                    sprintf "Proof complete." + proof_sep |> prooflog
                    stepCount <- steps.Length
                else if ProofLogic |- a   then
                   let axeq = ProofLogic.Axioms a
                   sprintf "|- %s. [%s]" (src a) (if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Logical Axiom of %s" axeq.Value.Name) |> prooflog
                   sprintf "Proof complete." + proof_sep |> prooflog
                   stepCount <- steps.Length               
    do while stepCount < steps.Length do
        let step = steps.[stepCount]
        let stepId = stepCount + 1
        let stepName = stepNames.[stepCount]
        do 
            match step.Rule with
            | (Rule(n, d)) -> if (not (ruleNames |> List.contains (n))) then failwithf "Rule at step %i (%s) is not a logical inference rule or part of the rules of the current theory." stepId n
            | (Subst(n, p, _)) -> if not ((p.Theory = ProofLogic) || (p.Theory = theory) || (p.Theory.GetType().IsAssignableFrom(theory.GetType()))) then failwithf "Substitution rule at step %i (%s) does not use the rules of L or the current theory." stepId n
        let _a = step.Apply _state
        let msg =
            match step.Rule with
            | (Rule(_, _)) -> 
                if not ((sequal _a _state)) then
                    sprintf "%i. %s: %s \u2192 %s." (stepId) (stepName.Replace("(expression)", step.Pos)) (src _state) (src _a) 
                else
                    sprintf "%i. %s: No change." (stepId) (stepName.Replace("(expression)", "expression")) 
            | (Subst(_,_,_)) ->
                if not ((sequal _a _state))  then
                    sprintf "%i. %s." (stepId) (stepName.Replace("(expression)", step.Pos)) 
                else
                    sprintf "%i. %s: No change." (stepId) (stepName.Replace("(expression)", "expression"))
        do prooflog msg
        _state <- _a
        state <- state @ [(_state, msg)]
        if theory |- _state then
            let axeq = theory.Axioms _state
            sprintf "|- %s. [%s]" (src _state)(if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Axiom of %s" axeq.Value.Name)|> prooflog
            sprintf "Proof complete." + proof_sep |> prooflog 
            stepCount <- steps.Length
        else if ProofLogic |- _state then 
            let axeq = ProofLogic.Axioms _state
            sprintf "|- %s. [%s]" (src _state) (if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Logical Axiom of %s" axeq.Value.Name) |> prooflog
            sprintf "Proof complete." + proof_sep |> prooflog
            stepCount <- steps.Length
        else
            sprintf "Proof incomplete. Current state: %s." (src _state) |> prooflog
            stepCount <- stepCount + 1

    do if stepCount = 0 && not(theory |- a || ProofLogic |- a ) then
        sprintf "Proof incomplete. Current state: %s." (src _state) |> prooflog
    member val Expr = a
    member val LastState = _state
    member __.L = 
        match a with
        | Equals(l, _) -> l
        | _ -> failwith "This expression is not an identity."
    member __.R = 
        match a with
        | Equals(_, r) -> r
        | _ -> failwith "This expression is not an identity."
    member val Theory = theory
    member val Steps = steps
    abstract Complete:bool
    default val Complete = ProofLogic |- _state || theory |- _state
    member val State = state
    member val Subst = steps |> List.map (fun s  -> s.Apply) |> List.fold(fun e r -> e >> r) id
    member val Log = logBuilder
    member __.Msg msg = prooflog msg
    /// The default logical theory used by proofs. Defaults to S but can be changed to something else.
    static member Logic with get() = ProofLogic and set(v) = ProofLogic <- v
    static member (|-) (proof:Proof, expr:Expr) = sequal proof.Expr expr  && proof.Complete
    //static member (|-) (proof:Proof, e:Expr<'t>) =         
        //let f = e |> expand |> body
        //do if not (range_type typeof<'t> = typeof<bool>) then failwithf "The formula %A does not have a truth value." (src f)

    static member (+) (l:Proof, r:RuleApplication) = if l.Complete then failwith "Cannot add a step to a completed proof." else Proof(l.Expr, l.Theory, l.Steps @ [r])

    interface IDisplay with
        member x.Output(item:'t) = item.ToString()
        member x.Transform(str:string) = str

and Assumption internal(expr:Expr) = inherit Proof(expr, Theory.Trivial, [], true)

and RuleApplication =
    | L  of Rule
    | R of Rule
    | LR of Rule
with
    member x.Rule = 
        match x with
        | LR rule -> rule
        | L rule -> rule
        | R rule -> rule
    member x.RuleName = x.Rule.Name
    member x.Apply(expr:Expr) =       
        match x with
        | LR rule -> rule.Apply expr
        | L rule -> 
            match expr with
            | Patterns.Call(o, m, l::r::[]) -> let s = rule.Apply l in binary_call(o, m, s, r)
            | _ -> failwith "Expression is not a binary operation."
        | R rule -> 
            match expr with
            | Patterns.Call(o, m, l::r::[]) -> let s = rule.Apply r in binary_call(o, m, l, s)
            | _ -> failwith "Expression is not a binary operation."
    member x.Pos =
        match x with
        | LR _ -> "expression"
        | L _ -> "left of expression"
        | R _ -> "right of expression"
type Theorem internal (expr: Expr, proof:Proof) = 
    do if not (sequal expr proof.Expr) then failwithf "The provided proof is not a proof of %s." (src expr)
    do if not proof.Complete then failwithf "The provided proof of %s is not complete." (src expr)
    member val Expr = expr
    member val Proof = proof
       
[<AutoOpen>]
module LogicalRules = 
    /// The default theory of equational logic that defines the logical axioms and inference rules for proofs.
    let S = Theory.S

    let rec subst (p:Proof) = 
        function
        | l when sequal l p.L && p.Complete -> p.R
        | expr -> traverse expr (subst p) 
    
    /// Leibniz's rule : A behaves equivalently in a formula if we substitute a part of A: a with x when x = a.
    let Subst (p:Proof) = 
        if not p.Complete then 
            failwithf "The proof of %A is not complete" (src p.Expr)  
        else Subst(sprintf "Substitute %s \u2261 %s into (expression)" (src p.L) (src p.R), p, fun proof e -> subst proof e) 

    /// Substitute an identity with a completed proof into another proof.
    let Ident (ident:Theorem) = ident.Proof |> Subst 
       
    /// Substitute an assumption.
    let Assume expr = Assumption(expr) |> Subst 
       
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
    let proof theory (e:Expr<'t>) steps =         
        let f = e |> expand |> body
        do if not (range_type typeof<'t> = typeof<bool>) then failwithf "The formula %A does not have a truth value." (src f)
        Proof(f, theory, steps)
    let theorem theory (e:Expr<'t>) steps  = 
        let f = e |> expand |> body
        do if not (range_type typeof<'t> = typeof<bool>) then failwithf "The formula %A does not have a truth value." (src f)
        Theorem(f, Proof(e, theory, steps))
    let lemma theory (e:Expr<'t>) steps  = 
        let f = e |> expand |> body
        do if not (range_type typeof<'t> = typeof<bool>) then failwithf "The formula %A does not have a truth value." (src f)
        Theorem(f, Proof (f, theory, steps, true))
    
    (* Identities *)
    let ident theory (e:Expr<'t>) steps =
        let f = e |> expand |> body
        do if not (range_type typeof<'t> = typeof<bool>) then failwithf "The formula %A does not have a truth value." (src f)
        match f with
        | Equals(_, _) -> Theorem(f, Proof (f, theory, steps, true)) |> Ident
        | _ -> failwithf "The expression %s is not an identity" (src f)
    let ident' steps e = ident Proof.Logic e steps
    let id_ax theory e = ident theory e []
    let id_ax' e = id_ax Proof.Logic e

    (* proof step shortcuts *)
    let id_ax_lr theory expr = expr |> id_ax theory |> LR
    let id_ax_l theory expr = expr |> id_ax theory |> L
    let id_ax_r theory expr = expr |> id_ax theory |> R
    
    let id_lr theory proof expr = expr |> ident theory proof |> LR
    let id_l theory proof expr = expr |> ident theory proof |> L
    let id_r theory proof expr = expr |> ident theory proof |> R