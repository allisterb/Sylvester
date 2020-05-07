namespace Sylvester

open FSharp.Quotations

open Descriptions
open Patterns

type Theory(axioms: Axioms, rules: Rules, ?formulaPrinter:string->string) =
    member val Axioms = axioms
    member val Rules = rules
    member val FormulaPrinter = defaultArg formulaPrinter id
    member x.AxiomaticallyEquiv a  = a |> body |> expand |> x.Axioms |> Option.isSome  
    static member (|-) ((c:Theory), a) = c.AxiomaticallyEquiv a

    /// The default logical theory used in Sylph proofs.
    static member val S =     
        let S_Reduce = Admit("Reduce logical constants in (expression)", EquationalLogic.reduce_constants)

        let S_LeftAssoc = Admit("Logical operators in (expression) are left-associative", EquationalLogic.left_assoc)
        
        let S_RightAssoc = Admit("Logical operators in (expression) are right-associative", EquationalLogic.right_assoc)
          
        let S_Commute = Admit("Logical operators in (expression) are commutative", EquationalLogic.commute)

        let S_Distrib = Admit("Distribute logical terms in (expression)", EquationalLogic.distrib)
        
        let S_Collect = Admit("Collect distributed logical terms in (expression)", EquationalLogic.collect)

        let S_Idemp = Admit("Substitute idempotent logical terms in (expression)", EquationalLogic.idemp)

        let S_ExcludedMiddle = Admit("Logical terms in (expression) satisfy the law of excluded middle", EquationalLogic.excluded_middle)

        let S_GoldenRule = Admit("Logical terms in (expression) satisfy the golden rule", EquationalLogic.golden_rule)

        let S_Implication = Admit("Substitute definition of implication into (expression)", EquationalLogic.implication)

        Theory(EquationalLogic.equational_logic_axioms, [
            S_Reduce
            S_LeftAssoc
            S_RightAssoc
            S_Commute
            S_Distrib
            S_Collect
            S_Idemp
            S_ExcludedMiddle
            S_GoldenRule
            S_Implication
        ],EquationalLogic.print_S_Operators)

    static member val internal Trivial = 
        Theory((fun (_:Expr) ->  Some(axiom_desc "Assumption" id (pattern_desc "Assumption" <@fun x y -> x = y @>))), [])

and Axioms = (Expr -> AxiomDescription option)

and Rule = 
    | Admit of string * (Expr -> Expr) 
    | Derive of string * Proof * (Proof -> Expr -> Expr)
with
    member x.Name = 
        match x with 
        | Admit(n, _) -> n
        | Derive(n, _, _) -> n
    member x.Apply = 
        match x with
        | Admit(_, r) -> r
        | Derive(_, p, r) -> r p
       
and Rules = Rule list 

and Proof internal(a:Expr, theory: Theory, steps: RuleApplication list, ?lemma:bool) =
    static let mutable logic:Theory = Theory.S /// The logical theory used for the proof.
    let ruleNames = List.concat [
            (logic.Rules: Rule list) |> List.map (fun (r:Rule) -> r.Name) 
            theory.Rules |> List.map (fun (r:Rule) -> r.Name)
        ]
    let stepNames: string list = steps |> List.map (fun r -> r.RuleName)
    
    let logBuilder = System.Text.StringBuilder()
    let l = defaultArg lemma false
    let proof_sep = if l then System.Environment.NewLine else ""
    let output (s:string) = 
        match defaultDisplay with
            | Text -> printfn "%s" ((logic.FormulaPrinter >> theory.FormulaPrinter) s)
            | _ -> printfn "%s" ((logic.FormulaPrinter >> theory.FormulaPrinter) s)
    let prooflog (x:string) = 
        do 
            logBuilder.Append(x) |> ignore
            if not l || x.StartsWith "[Lemma]" then output x else output ("        " + x)
    
    let mutable _state = a
    let mutable state:(Expr * string) list = [] 
    let mutable stepCount = 0

    do 
        if theory.GetType() = typeof<Assumption> then 
            do sprintf "Assume %s." (src a) |> prooflog
            stepCount <- steps.Length
        else  
            if l then 
                sprintf "[Lemma] %s:" (src a) |> prooflog
            else
                sprintf "Proof of %s:" (src a) |> prooflog
        
            if theory |- a  then
                let axeq = theory.Axioms a
                sprintf "|- %s. [%s]" (src a) (if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Axiom of %s" axeq.Value.Name) |> prooflog
                sprintf "Proof complete." + proof_sep |> prooflog
                stepCount <- steps.Length
            
            else if logic |- a   then
                let axeq = logic.Axioms a
                sprintf "|- %s. [%s]" (src a) (if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Logical Axiom of %s" axeq.Value.Name) |> prooflog
                sprintf "Proof complete." + proof_sep |> prooflog
                stepCount <- steps.Length               
    
    do if steps.Length = 0 && not(theory |- a || logic |- a ) then
        sprintf "Proof incomplete. Current state: %s." (src _state) |> prooflog

    // Iterate through proof steps
    do while stepCount < steps.Length do
        let step = steps.[stepCount]
        let stepId = stepCount + 1
        let stepName = stepNames.[stepCount]
        do 
            match step.Rule with
            | Admit(n, _) -> if (not (ruleNames |> List.contains (n))) then 
                                failwithf "Rule at step %i (%s) is not a logical inference rule or part of the rules of the current theory." stepId n
            | Derive(n, p, _) -> 
                if not ((p.Theory = logic) || (p.Theory = theory) || (p.Theory.GetType().IsAssignableFrom(theory.GetType()))) then 
                    failwithf "Substitution rule at step %i (%s) does not use the rules of L or the current theory." stepId n                  
        let _a = 
            match step.Rule with
            | Admit(_,_) -> step.Apply _state
            | Derive(_,_,_) -> step.Apply _state
        let msg =
            match step.Rule with
            | Admit(_, _) -> 
                if not ((sequal _a _state)) then
                    sprintf "%i. %s: %s \u2192 %s." (stepId) (stepName.Replace("(expression)", step.Pos)) (src _state) (src _a) 
                else
                    sprintf "%i. %s: No change." (stepId) (stepName.Replace("(expression)", "expression")) 
            | Derive(_,_,_) ->
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
        else if logic |- _state then 
            let axeq = logic.Axioms _state
            sprintf "|- %s. [%s]" (src _state) (if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Logical Axiom of %s" axeq.Value.Name) |> prooflog
            sprintf "Proof complete." + proof_sep |> prooflog
            stepCount <- steps.Length
        else
            sprintf "Proof incomplete. Current state: %s." (src _state) |> prooflog
            stepCount <- stepCount + 1
    
    member val Stmt = a
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
    default val Complete = logic |- _state || theory |- _state
    member val State = state
    member val Subst = steps |> List.map (fun s  -> s.Apply) |> List.fold(fun e r -> e >> r) id
    member val Log = logBuilder
    member __.Msg msg = prooflog msg
    
    /// The default logical theory used by proofs. Defaults to S but can be changed to something else.
    static member Logic with get() = logic and set(v) = logic <- v
    static member (|-) (proof:Proof, expr:Expr) = sequal proof.Stmt expr  && proof.Complete
    static member (+) (l:Proof, r:RuleApplication) = if l.Complete then failwith "Cannot add a step to a completed proof." else Proof(l.Stmt, l.Theory, l.Steps @ [r])

    interface IDisplay with
        member x.Output(item:'t) = item.ToString()
        member x.Transform(str:string) = str

and Assumption internal(expr:Expr) = inherit Proof(expr, Theory.Trivial, [], true)

and RuleApplication =
    | L  of Rule
    | R of Rule
    | LR of Rule
    | L' of RuleApplication
    | R' of RuleApplication
with
    member x.Rule = 
        match x with
        | LR rule -> rule
        | L rule -> rule
        | R rule -> rule
        | L' ra -> ra.Rule
        | R' ra -> ra.Rule
            
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
        | L' ra ->
            match expr with
            | Patterns.Call(o, m, l::r::[]) -> 
                let s = ra.Apply l
                binary_call(o, m, s, r)
            | _ -> failwith "Expression is not a binary operation."
        | R' ra ->
             match expr with
             | Patterns.Call(o, m, l::r::[]) -> 
                 let s = ra.Apply r
                 binary_call(o, m, l, s)
             | _ -> failwith "Expression is not a binary operation."

    member x.Pos =
        match x with
        | LR _ -> "expression"
        | L _ -> "left of expression"
        | R _ -> "right of expression"
        | L' ra -> sprintf "left-%sof expression" ra.Pos
        | R' ra -> sprintf "right-%sof expression" ra.Pos
        
and Theorem internal (expr: Expr, proof:Proof) = 
    do if not (sequal expr proof.Stmt) then failwithf "The provided proof is not a proof of %s." (src expr)
    do if not proof.Complete then failwithf "The provided proof of %s is not complete." (src expr)
    member val Stmt = expr
    member val LastState = proof.LastState
    member val Proof = proof
    member val Theory = proof.Theory
    member val Name = expr |> src

[<AutoOpen>]
module LogicalRules =     
    /// Leibniz's rule : A behaves equivalently in a formula if we substitute a part of A: a with x when x = a.
    let Subst (p:Proof) = 
        let rec subst (p:Proof) = 
            function
            | l when sequal l p.L && p.Complete -> p.R
            | expr -> traverse expr (subst p) 

        if not p.Complete then 
            failwithf "The proof of %A is not complete" (src p.Stmt)  
        else Derive(sprintf "Substitute %s \u2261 %s into (expression)" (src p.L) (src p.R), p, fun proof e -> subst proof e)
        
    /// Substitute an identity with a completed proof into another proof.
    let Ident (ident:Theorem) = ident.Proof |> Subst 
       
    /// Substitute an assumption.
    let Assume expr = Assumption(expr) |> Subst 
       
[<AutoOpen>]
module Proof =        
    let proof theory (e:Expr<'t>) steps =         
        let f = e |> expand |> body
        do if not (range_type typeof<'t> = typeof<bool>) then failwithf "The formula %A does not have a truth value." (src f)
        Proof(f, theory, steps)
    let theorem theory (e:Expr<'t>) steps  = 
        let f = e |> expand |> body
        do if not (range_type typeof<'t> = typeof<bool>) then failwithf "The formula %A does not have a truth value." (src f)
        Theorem(f, Proof(f, theory, steps))
    let axiom theory (e:Expr<'t>)  = 
        let f = e |> expand |> body
        do if not (range_type typeof<'t> = typeof<bool>) then failwithf "The formula %A does not have a truth value." (src f)
        Theorem(f, Proof (f, theory, [], true))
    
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