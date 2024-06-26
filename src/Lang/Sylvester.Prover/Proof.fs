﻿namespace Sylvester

open FSharp.Quotations

open Descriptions
open Patterns

type Theory(axioms: Axioms, rules: Rules, ?formula_printer:Expr->string) =
    member val Axioms = axioms
    member val Rules = rules
    member val PrintFormula = defaultArg formula_printer Display.print_formula
    member x.AxEquiv a  = a |> expand |> x.Axioms |> Option.isSome  
    static member (|-) ((c:Theory), a) = c.AxEquiv a
    static member (|-) ((c:Theory), a:Term<_>) = c.AxEquiv a.Expr
    /// The default logical theory used in Sylph proofs.
    static member val S =     
        let reduce = Admit("Reduce logical constants in (expression)", EquationalLogic._reduce_constants)

        let left_assoc = Admit("Logical operators in (expression) are left-associative", EquationalLogic._left_assoc)
        
        let right_assoc = Admit("Logical operators in (expression) are right-associative", EquationalLogic._right_assoc)
          
        let commute = Admit("Logical operators in (expression) are commutative", EquationalLogic._commute)

        let distrib = Admit("Distribute logical terms in (expression)", EquationalLogic._distrib)
        
        let collect = Admit("Collect distributed logical terms in (expression)", EquationalLogic._collect)

        let idemp = Admit("Substitute idempotent logical terms in (expression)", EquationalLogic._idemp)

        let excluded_middle = Admit("Logical terms in (expression) satisfy the law of excluded middle", EquationalLogic._excluded_middle)

        let golden_rule = Admit("Logical terms in (expression) satisfy the golden rule", EquationalLogic._golden_rule)

        let def_implies = Admit("Substitute definition of implication into (expression)", EquationalLogic._def_implies)

        let shunt = Admit("Shunt implication in (expression)", EquationalLogic._shunt)

        let rshunt = Admit("Reverse shunt implication in (expression)", EquationalLogic._rshunt)

        let mutual_implication = Admit("The (expression) contains a mutual implication", EquationalLogic._mutual_implication)
        
        let subst_and = Admit("Substitute an equivalent subexpression in (expression)", EquationalLogic._subst_and)

        let subst_implies = Admit("Substitute an equivalent subexpression in (expression)", EquationalLogic._subst_implies)

        let subst_and_implies = Admit("Substitute an equivalent subexpression in (expression). ", EquationalLogic._subst_and_implies)

        let subst_true = Admit("Substitute an equivalent subexpression in (expression) with the constant true. ", EquationalLogic._subst_true)

        let subst_false = Admit("Substitute an equivalent subexpression in (expression) with the constant false. ", EquationalLogic._subst_false)

        let subst_or_and = Admit("Use the Shannon substitution in (expression)", EquationalLogic._subst_or_and)

        let distrib_implies = Admit("Distribute implication in (expression)", EquationalLogic._distrib_implies)

        let double_neg = Admit("Replace (expression) with its dual", EquationalLogic._double_neg)

        let empty_range = Admit("Substitute the quantifier's empty range in (expression)", EquationalLogic._empty_range)

        let trade_body = Admit("Move the quantifier's range into its body in (expression)", EquationalLogic._trade_body)

        let collect_forall_and = Admit("Collect \u2200 quantifer terms distributed over \u2227 in (expression)", EquationalLogic._collect_forall_and)

        let collect_exists_or = Admit("Collect \u2203 quantifer terms distributed over or in (expression)", EquationalLogic._collect_exists_or)

        let distrib_or_forall = Admit("\u2228 distributes over \u2200 in (expression)", EquationalLogic._distrib_or_forall)

        let split_range_forall = Admit("Split \u2200 quantifer range in (expression)", EquationalLogic._split_range_forall)
        
        let split_range_exists = Admit("Split \u2203 quantifer range in (expression)", EquationalLogic._split_range_exists)

        Theory(EquationalLogic.equational_logic_axioms, [
            reduce
            left_assoc
            right_assoc
            commute
            distrib
            collect
            idemp
            excluded_middle
            golden_rule
            def_implies
            shunt
            rshunt
            mutual_implication
            subst_and
            subst_implies
            subst_and_implies
            subst_true
            subst_false
            subst_or_and
            distrib_implies
            double_neg
            empty_range
            trade_body
            collect_forall_and
            collect_exists_or
            distrib_or_forall
            split_range_forall
            split_range_exists
        ])

and Axioms = (Expr -> AxiomDescription option)

and Rule = 
    | Admit of string * (Expr -> Expr) 
    | Derive of string * Proof * (Proof -> Expr -> Expr)
    | Deduce of string * Proof * (Proof -> Expr -> Expr) 
    | Define of string * (Expr -> Expr)
with
    member x.Name = 
        match x with 
        | Admit(n, _) -> n
        | Derive(n, _, _) -> n
        | Deduce(n,_,_) -> n
        | Define(n, _) -> n
    member x.Apply = 
        match x with
        | Admit(_, r) -> r
        | Derive(_, p, r) -> r p
        | Deduce(_, p, r) -> r p
        | Define(_, r) -> r
        
and Rules = Rule list 

and Proof(a:Expr, theory: Theory, steps: RuleApplication list, ?lemma:bool) =
    /// The logical theory used for the proof.
    static let mutable logic:Theory = Theory.S
    /// The proof log level.
    static let mutable logLevel:int = 1 

    let ruleNames = List.concat [
            let logicRules = (logic.Rules: Rule list) in logicRules |> List.map (fun (r:Rule) -> r.Name) 
            theory.Rules |> List.map (fun (r:Rule) -> r.Name)
        ]
    let stepNames: string list = steps |> List.map (fun r -> r.RuleName)
    let print_formula = theory.PrintFormula
    let logBuilder = System.Text.StringBuilder()
    let l = defaultArg lemma false
    let isAxiom = l && steps.Length = 0
    
    // Proof logging
    let proof_sep = ""
    let _prooflog (steps:RuleApplication list) (level:int) (isLemma:bool) (x:string) = 
        let output (s:string) = 
            logBuilder.Append(x) |> ignore
            printfn "%s" s 
        do 
            let isShortLemmaProof = isLemma && steps.Length <= 2
            if not isLemma then 
                output x
            else if isLemma && level > 2 then
                output ("        " + x)
            else if not isShortLemmaProof && level >= 1 then
                    output ("        " + x)         
    let prooflog = _prooflog steps logLevel l
    let alwayslog = _prooflog steps 3 false
    do if not l then 
        match logLevel with
        | 0 -> alwayslog <| sprintf "Proof log level is %i. Only necessary output will be printed."  logLevel
        | 1 -> alwayslog <| sprintf "Proof log level is %i. Short proofs of lemmas won't be printed."  logLevel
        | 2 -> alwayslog <| sprintf "Proof log level is %i. All proofs of lemmas will be printed."  logLevel
        | 3 -> alwayslog <| sprintf "Proof log level is %i. All proofs of axioms and lemmas will be printed."  logLevel
        | _ -> failwith "Unknown proof log level."
    do 
        if isAxiom && logLevel <= 2 then 
            sprintf "[Axiom] %s." (print_formula a) |> alwayslog
        else if isAxiom && logLevel > 2 then 
            sprintf "[Axiom] %s:" (print_formula a) |> alwayslog
        else if l && logLevel > 2 then 
            sprintf "[Lemma] %s:" (print_formula a) |> alwayslog
        else if l && logLevel < 2 && steps.Length <= 2 then 
            sprintf "[Lemma] %s." (print_formula a) |> alwayslog
        else if l && logLevel < 2 && steps.Length >= 2 then 
            sprintf "[Lemma] %s:" (print_formula a) |> alwayslog
        else if not l then
            sprintf "Proof of %s:" (print_formula a) |> alwayslog

    let mutable _state = a
    let mutable state:(Expr * string) list = [] 
    let mutable stepCount = 0
    do if theory |- a  then
            let axeq = theory.Axioms a
            sprintf "|- %s. [%s]" (print_formula a) (if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Axiom of %s" axeq.Value.Name) |> prooflog
            sprintf "Proof complete." + proof_sep |> prooflog
            stepCount <- steps.Length
       else if logic |- a   then
            let axeq = logic.Axioms a
            sprintf "|- %s. [%s]" (print_formula a) (if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Logical Axiom of %s" axeq.Value.Name) |> prooflog
            sprintf "Proof complete." + proof_sep |> prooflog
            stepCount <- steps.Length               
       else if steps.Length = 0 && not(theory |- a || logic |- a ) then
        sprintf "Proof incomplete. Current state: %s." (print_formula _state) |> prooflog

    // Iterate through proof steps
    do while stepCount < steps.Length do
        let current_ant, current_conseq, current_conjuncts = 
            match _state with 
            | Argument (ant, con, conj) -> (Some ant, Some con, Some conj)
            | _ -> None, None, None
        let step = steps.[stepCount]
        let stepId = stepCount + 1
        let stepName = stepNames.[stepCount]
        do 
            match step.Rule with
            | Admit(n,_) -> if (not (ruleNames |> List.contains (n))) then 
                                failwithf "Rule at step %i (%s) is not an admitted logical inference rule or part of the admitted rules of the current theory." stepId n
            | Derive(n, p, _) -> 
                if not ((p.Theory = logic) || (p.Theory = theory) || (p.Theory.GetType().IsAssignableFrom(theory.GetType()))) then 
                    failwithf "Substitution rule at step %i (%s) does not use the rules of the current proof logic or theory." stepId n
            | Deduce(n,p,_) -> 
                if current_ant.IsNone then failwithf "Deduction rule at step %i (%s) cannot be used since %s is not a logical implication with an antecedent and consequent." stepId n (print_formula a)
                if not ((p.Theory = logic) || (p.Theory = theory) || (p.Theory.GetType().IsAssignableFrom(theory.GetType()))) then 
                    failwithf "Deduction rule at step %i (%s) does not use the rules of the current proof logic or theory." stepId n
                let conjs = 
                    match p.Stmt with 
                    | Argument(_, _, cj) -> cj 
                    | _ -> failwithf "%s is not a logical implication with an antecedent and consequent." (print_formula p.Stmt)
                if conjs |> List.forall(fun v -> List.exists(fun v' -> sequal v v') current_conjuncts.Value) |> not then
                    failwithf "The conjunct %s in deduction rule at step %i (%s) is not in the antecedent of %s." 
                        (conjs |> List.find(fun v -> List.exists(fun v' -> not (sequal v v')) current_conjuncts.Value) |> print_formula) stepId n (print_formula a)
                if not step.RightApplication then failwith "A deduction rule can only be applied to the consequent of a logical implication."
            | _ -> ()
        let _a = step.Apply _state

        let msg =
            match step.Rule with
            | Admit(_, _) -> 
                if not ((sequal _a _state)) then
                    sprintf "%i. %s: %s \u2192 %s." (stepId) (stepName.Replace("(expression)", step.Pos)) (print_formula _state) (print_formula _a) 
                else
                    sprintf "%i. %s: No change." (stepId) (stepName.Replace("(expression)", step.Pos)) 
            | Derive(_,_,_) ->
                if not ((sequal _a _state))  then
                    sprintf "%i. %s." (stepId) (stepName.Replace("(expression)", step.Pos)) 
                else
                    sprintf "%i. %s: No change." (stepId) (stepName.Replace("(expression)", step.Pos))
            | Deduce(_,_,_) ->
                if not ((sequal _a _state))  then
                    sprintf "%i. %s." (stepId) (stepName.Replace("(expression)", step.Pos)) 
                else
                    sprintf "%i. %s: No change." (stepId) (stepName.Replace("(expression)", step.Pos))
            | Define(_, _) -> 
                if not ((sequal _a _state)) then
                    sprintf "%i. %s: %s \u2192 %s." (stepId) (stepName.Replace("(expression)", step.Pos)) (print_formula _state) (print_formula _a) 
                else
                    sprintf "%i. %s: No change." (stepId) (stepName.Replace("(expression)", step.Pos))
        do prooflog msg
        _state <- _a
        state <- state @ [(_state, msg)]
        if theory |- _state then
            let axeq = theory.Axioms _state
            sprintf "|- %s. [%s]" (print_formula _state)(if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Axiom of %s" axeq.Value.Name)|> prooflog
            sprintf "Proof complete." + proof_sep |> prooflog
            stepCount <- steps.Length
        else if logic |- _state then 
            let axeq = logic.Axioms _state
            sprintf "|- %s. [%s]" (print_formula _state) (if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Logical Axiom of %s" axeq.Value.Name) |> prooflog
            sprintf "Proof complete." + proof_sep |> prooflog
            stepCount <- steps.Length
        else
            sprintf "Proof incomplete. Current state: %s." (print_formula _state) |> prooflog
            stepCount <- stepCount + 1
    let last_ant, last_conseq, last_conjuncts = 
        match _state with 
        | Argument (ant, con, conj) -> (Some ant, Some con, Some conj)
        | _ -> None, None, None

    member val Stmt = a
    member val LastState = _state
    member val L = 
        match a with
        | Equals(l, _) -> l
        | _ -> a
    member val R = 
        match a with
        | Equals(_, r) -> r
        | _ -> a
    member val LastAntecedent = last_ant
    member val LastConseq = last_conseq
    member val LastConjuncts = last_conjuncts
    member val IsLemma = l
    member val Theory = theory
    member val Steps = steps
    abstract Complete:bool; default val Complete = logic |- _state || theory |- _state
    member val State = state
    member val Subst = steps |> List.map (fun s  -> s.Apply) |> List.fold(fun e r -> e >> r) id
    member val Log = logBuilder
    member val Msg = prooflog
    /// Proof log level.
    static member LogLevel with get() = logLevel and set(v) = logLevel <- v
    /// The default logical theory used by proofs. Defaults to S but can be changed to something else.
    static member Logic with get() = logic and set(v) = logic <- v
    static member (|-) (proof:Proof, expr:Expr) = sequal proof.Stmt expr && proof.Complete
    static member (+) (l:Proof, r:RuleApplication) = 
        if l.Complete then 
            failwith "Cannot add a step to a completed proof." 
        else Proof(l.Stmt, l.Theory, l.Steps @ [r])
    static member (+) (l:Proof, r:RuleApplication list) = 
        if l.Complete then 
            failwith "Cannot add a step to a completed proof." 
        else Proof(l.Stmt, l.Theory, l.Steps @ r)

and RuleApplication =
    | ApplyLeft  of Rule
    | R of Rule
    | LR of Rule
    | QR of Rule
    | QB of Rule
    | NextLeft of RuleApplication
    | R' of RuleApplication
    | LR' of RuleApplication
    | QR' of RuleApplication
    | QB' of RuleApplication
with
    member x.Rule = 
        match x with
        | LR rule
        | ApplyLeft rule
        | R rule
        | QR rule
        | QB rule -> rule
        | NextLeft ra 
        | R' ra 
        | LR' ra 
        | QR' ra
        | QB' ra -> ra.Rule
    member x.RuleName = x.Rule.Name
    member x.Apply(expr:Expr) =
        let print_formula = Proof.Logic.PrintFormula
        match x with
        | LR rule -> rule.Apply expr
        | ApplyLeft rule -> 
            match expr with
            | Patterns.Call(o, m, l::r::[]) -> let s = rule.Apply l in binary_call(o, m, s, r)
            | _ -> failwithf "%s is not a binary operation." (print_formula expr)
        | R rule -> 
            match expr with
            | Patterns.Call(o, m, l::r::[]) -> let s = rule.Apply r in binary_call(o, m, l, s)
            | _ -> failwithf "%s is not a binary operation." (print_formula expr)
        | QR rule ->
            match expr with
            | Quantifier(op, x, range, body) -> let s = rule.Apply range in let v = vars_to_tuple x in call op (v::s::body::[])
            | _ -> failwithf "%s is not a binary operation." (print_formula expr)
        | QB rule ->
            match expr with
            | Quantifier(op, x, range, body) -> let s = rule.Apply body in let v = vars_to_tuple x in call op (v::range::s::[])
            | _ -> failwithf "%s is not a binary operation." (print_formula expr)
        | NextLeft ra ->
            match expr with
            | Patterns.Call(o, m, l::r::[]) -> let s = ra.Apply l in binary_call(o, m, s, r)
            | _ -> failwithf "%s is not a binary operation." (print_formula expr)
        | R' ra ->
            match expr with
            | Patterns.Call(o, m, l::r::[]) -> let s = ra.Apply r in binary_call(o, m, l, s)
            | _ -> failwithf "%s is not a binary operation." (print_formula expr)
        | LR' ra ->
            match expr with
            | Patterns.Call(o, m, l::[]) -> let s = ra.Apply l in unary_call(o, m, s)
            | _ -> failwithf "%s is not a binary operation." (print_formula expr)
        | QR' ra ->
            match expr with
            | Quantifier(op, x, range, body) -> let s = ra.Apply range in let v = vars_to_tuple x in call op (v::s::body::[])
            | _ -> failwithf "%s is not a binary operation." (print_formula expr)
        | QB' ra ->
            match expr with
            | Quantifier(op, x, range, body) -> let s = ra.Apply body in let v = vars_to_tuple x in call op (v::range::s::[])
            | _ -> failwithf "%s is not a binary operation." (print_formula expr)
    member x.Pos =
        match x with
        | LR _ -> "expression"
        | ApplyLeft _ -> "left of expression"
        | R _ -> "right of expression"
        | QR _ -> "quantifier range"
        | QB _ -> "quantifier body"
        | NextLeft ra -> sprintf "left>%s of expression" (ra.Pos.Replace(" of expression", ""))
        | R' ra -> sprintf "right>%s of expression" (ra.Pos.Replace(" of expression", ""))
        | LR' ra -> sprintf "left-right>%s of expression" (ra.Pos.Replace(" of expression", ""))
        | QR' ra -> sprintf "quantifier-range>%s of expression" (ra.Pos.Replace(" of expression", ""))
        | QB' ra -> sprintf "quantifier-body>%s of expression" (ra.Pos.Replace(" of expression", ""))

    member x.LeftApplication = 
        x.Pos = "left of expression" || System.Text.RegularExpressions.Regex.IsMatch(x.Pos, "left>(\\S)+\\s+of expression")
    
    member x.RightApplication =
        x.Pos = "right of expression" || System.Text.RegularExpressions.Regex.IsMatch(x.Pos, "right>(\\S)+\\s+of expression")

and Theorem (expr: Expr, proof:Proof) = 
    let print_formula = proof.Theory.PrintFormula
    do 
        if not (sequal expr proof.Stmt) then failwithf "The provided proof is not a proof of %s." (print_formula expr)
        if not proof.Complete then 
            if not proof.IsLemma then
                failwithf "The provided proof of theorem %s is not complete." (print_formula expr)
            else failwithf "The provided proof of lemma %s is not complete. The last state is %s. This can happen in derived rules when a unexpected substitution occcurs in steps that are not specific enough."  (print_formula expr) (print_formula proof.LastState) 
    member val Stmt = expr
    member val LastState = proof.LastState
    member val Proof = proof
    member val Theory = proof.Theory
    member val Name = expr |> print_formula
    new (proof:Proof) = Theorem(proof.Stmt, proof)

[<AutoOpen>]
module ProofOps =
    type Apply = RuleApplication
    
    let apply_left = ApplyLeft
    
    let apply_right = Apply.R
    
    let apply = Apply.LR

    let apply_body = Apply.QB

    let apply_range = Apply.QR

    let after_left = NextLeft

    let after_right = Apply.R'
    
    let after_both = Apply.LR'

    let after_body = Apply.QB'

    let after_range = Apply.QR'

    let print_formula (p:Proof) = p.Theory.PrintFormula
    
    let last_state (p:Proof) = p.LastState

    let left_state p = p |> last_state |> expand_left

    let right_state p = p |> last_state |> expand_right

    let left_src p = p |> left_state |> src

    let right_src p = p |> right_state |> src

    let last_conjuncts (p:Proof) = 
        match p.LastConjuncts with
        | Some conj -> conj |> List.map (fun c -> c |> print_formula p)
        | _ -> []

[<AutoOpen>]
module LogicalRules =     
    /// Leibniz's rule : A behaves equivalently in a formula if we substitute a part of A: a with x when x = a.
    let Subst (p:Proof) = 
        let rec subst (p:Proof) = 
            function
            | l when sequal l p.L && p.Complete -> p.R
            | expr -> traverse expr (subst p) 
        if not p.Complete then 
            failwithf "The proof of %A is not complete" (p.Theory.PrintFormula p.Stmt)  
        Derive(sprintf "Substitute %s \u2261 %s into (expression)" (p.Theory.PrintFormula p.L) (p.Theory.PrintFormula p.R), p, fun proof e -> subst proof e)
        
    /// Substitute an identity with a completed proof into another proof.
    let Ident (ident:Theorem) = ident.Proof |> Subst 
        
    /// Rule of modus ponens: Substitute the consequent of a proven theorem p ==> q with true in a proof where p is one of the conjuncts of the antecedent.
    let Subst' (p:Proof) =
        let rec subst (p:Proof)  = 
            let con= 
                match p.Stmt with 
                | Argument(_, c, _) -> c
                | _ -> failwithf "The theorem %s is not a logical implication." (p.Stmt |> p.Theory.PrintFormula)
            function
            | l when sequal l con -> <@@ true @@>
            | expr -> traverse expr (subst p) 
        if not p.Complete then 
                failwithf "The proof of %A is not complete" (p.Stmt |> p.Theory.PrintFormula)  
        let ant,con = 
            match p.Stmt with 
            | Argument(a, c, _) -> a, c
            | _ -> failwithf "The theorem %s is not a logical implication." (p.Stmt |> p.Theory.PrintFormula)
        Rule.Deduce(sprintf "Deduce %s from %s and substitute with true into (expression)" (p.Theory.PrintFormula con) (p.Theory.PrintFormula ant), p, fun proof e -> subst proof e)

    /// Substitute the consequent of a proven theorem p ==> q with true in a proof where p is one of the conjuncts of the antecedent.
    let Deduce(t:Theorem) = t.Proof |> Subst'

    /// Rule of modus ponens: Substitute the LHS q of a proven identity p ==> (q = r) with the RHS r in a proof where p is one of the conjuncts of the antecedent.
    let Subst'' (p:Proof) =
        let rec subst (p:Proof)  = 
            let con = 
                match p.Stmt with 
                | Argument(_, c, _) -> c
                | _ -> failwithf "The theorem %s is not a logical implication." (p.Stmt |> p.Theory.PrintFormula)
            let lcon, rcon = 
                match con with
                | Equals(l, r) -> l, r
                | _ -> failwithf "The theorem %s is not an identity." (con |> p.Theory.PrintFormula)
            function
            | l when sequal l lcon -> rcon
            | expr -> traverse expr (subst p) 

        if not p.Complete then 
                failwithf "The proof of %A is not complete" (p.Stmt |> p.Theory.PrintFormula)  
        let ant,con = 
            match p.Stmt with 
            | Argument(a, c, _) -> a, c
            | _ -> failwithf "The theorem %s is not a logical implication." (p.Stmt |> p.Theory.PrintFormula)
        let lcon, rcon = 
            match con with
            | Equals(l, r) -> l, r
            | _ -> failwithf "The theorem %s is not an identity." (con |> p.Theory.PrintFormula)
    
        Rule.Deduce(sprintf "Deduce %s from %s and substitute %s with %s into (expression)" (p.Theory.PrintFormula con) (p.Theory.PrintFormula ant) (p.Theory.PrintFormula lcon) (p.Theory.PrintFormula rcon), p, fun proof e -> subst proof e)

    /// Substitute the LHS q of a proven identity p ==> (q = r) with the RHS r in a proof where p is one of the conjuncts of the antecedent.
    let Deduce'(t:Theorem) = t.Proof |> Subst''

    let Define (theory:Theory) (expr:Expr) = 
        let rec subst (lhs:Expr, rhs:Expr) = 
            function
            | l when sequal l lhs -> rhs
            | expr -> traverse expr (subst(lhs, rhs))
        match expr with
        | Equals(l, r) -> Rule.Define(sprintf "Substitute definition of %s \u2261 %s into (expression)" (theory.PrintFormula l) (theory.PrintFormula r), subst(l, r))
        | _ -> failwithf "The formula %A is not an identity." (theory.PrintFormula expr)


[<AutoOpen>]
module Proof = 
    let proof<'t> (theory:Theory) (e:Prop) steps =         
        let f = e.Expr |> expand in Proof(f, theory, steps)
    
    let theorem (theory:Theory) (e:Prop) steps  = 
        let f = e.Expr |> expand in Theorem(f, Proof(f, theory, steps))
    
    let lemma (theory:Theory) (e:Prop) steps =
        let f = e.Expr |> expand in Theorem(f, Proof(f, theory, steps, true))
    
    let axiom (theory:Theory) (e:Prop) = lemma theory e []

    (* Identities *)
    let ident (theory:Theory) (e:Prop) steps =
        let f = e.Expr |> expand in
        match f with
        | Equals(_, _) -> Theorem(f, Proof (f, theory, steps, true)) |> Ident
        | _ -> failwithf "The expression %s is not an identity." (theory.PrintFormula f)
    
    let logical_ident steps (e:Prop) = ident Proof.Logic e steps
    
    let id_ax theory (e:Prop) = ident theory e []
    
    let log_id_ax (e:Prop) = id_ax Proof.Logic e
    
    
    (* Deductions *)
    let deduce (p:Theorem) = p|> Deduce
    let deduce' p = p |> Deduce'
    let deduce_ident p = deduce' p
    
    (* Definitions *)
    let def (theory:Theory) (e:Prop) = 
        let f = e.Expr |> expand
        match f with
        | Equals(_, _) -> Define theory f
        | _ -> failwithf "The expression %s is not an identity." (theory.PrintFormula f)
        