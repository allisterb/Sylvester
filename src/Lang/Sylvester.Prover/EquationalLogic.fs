namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Patterns
open Descriptions

/// Formalizes the default equational propsitional logic used by Sylph called S.
/// Based on E: https://www.cs.cornell.edu/fbs/publications/94-1455.pdf
///             http://www.cs.cornell.edu/home/gries/Logic/Equational.html
/// The number after each axiom corresponds to the number of the axiom in the textbook A Logical Approach to Discrete Math by Gries et.al.
module EquationalLogic =
    let desc = axiom_desc "Equational Logic"
    
    (* Axioms *)

    /// true = p = p
    let (|True|_|) =
        function
        | Equals(Bool true, Equals(a1, a2)) when sequal a1 a2 -> pattern_desc "Definition of true" <@fun x -> x = x = true @> |> Some
        | _ -> None

    /// false = not true
    let (|False|_|) =
        function
        | Equals(Bool false, Not(Bool true)) -> pattern_desc "Definition of false" <@ false = not true @> |> Some
        | _ -> None

    /// not (p = q) = not p = q
    let (|DistribNot|_|) =
        function
        | Equals(Not(Equals(a1, a2)), Equals(Not(b1), b2)) when sequal2 a1 a2 b1 b2 -> pattern_desc "Distributivity" <@fun x y -> not(x = y) = (not x = y) @> |> Some
        | _ -> None

    /// p ||| not p
    let (|ExcludedMiddle|_|) =
        function
        | Or(a1, Not(a2)) when sequal a1 a2 -> pattern_desc "the Excluded Middle" <@fun x -> x ||| not x @> |> Some
        | _ -> None

    /// p |&| q = p = q = p ||| q 
    let (|GoldenRule|_|) =
        function
        | Equals(And(p1, q1), Equals(Equals(p2, q2), Or(p3, q3))) when sequal p1 p2 && sequal p2 p3 && sequal q1 q2 && sequal q2 q3 -> 
                                                                pattern_desc "the Golden Rule" <@fun x y -> x |&| y = (x = y) = (x ||| y) @> |> Some
        | _ -> None

    /// p ==> q = ((p ||| q) = q)
    let (|Implication|_|) =
        function
        | Equals(Implies(a1, a2), Equals(Or(a3, a4), a5)) when sequal a1 a3 && sequal a2 a4 && sequal a4 a5 -> 
                                                                pattern_desc "Implication" <@fun x y-> (x ==> y) = ((x ||| y) = y)@> |> Some
        | Equals(Conseq(a1, a2), Implies(a3, a4)) when sequal2 a1 a2 a4 a3 -> 
                                                                pattern_desc "Consequence" <@fun x y -> (x <== y) = (y ==> x) @> |> Some
        // This isn't in the original axioms for E but is included in S for convenience
        | Implies(_, Bool true) -> pattern_desc "Implication" <@fun x -> x ==> true @> |> Some
        | _ -> None

    /// (e = f) ==> E(e) = E(f) 
    let (|Leibniz|_|) =
        function
        | Implies(Equals(Var e, Var f), Equals(Ee, Ef)) when sequal (replace_var_var e f Ee) Ef -> pattern_desc "Leibniz" <@fun e f E -> (e = f) ==> E(e) = E(f)@> |> Some
        | Implies(Equals(Var p, Var q), Equals(Quantifier(_, _, R, P), Quantifier(_, _, R', P'))) when sequal (replace_var_var p q R) R' && sequal (replace_var_var p q P) P' -> 
            pattern_desc "Leibniz" <@fun p q E  -> (p = q) ==> E(p) = E(q) @> |> Some
        | Implies(Implies(R , Equals(Var p, Var q)), Equals(Quantifier(_, _, R1, P), Quantifier(_, _, R2, P'))) when sequal R R1 && sequal R1 R2 && sequal (replace_var_var p q P) P' -> 
            pattern_desc "Leibniz" <@fun p q E  -> (p = q) ==> E(p) = E(q) @> |> Some
        | _ -> None
    
    let (|EmptyRange|_|) =
        function
        | ForAll(_,_,Bool false,_) -> pattern_desc "Empty Range" <@ () @> |> Some
        | Equals(Exists(_,_,Bool false,_), Bool false) -> pattern_desc "Empty Range" <@ () @> |> Some
        | _ -> None
    
    let (|QuantifierDistrib|_|) =
        function
        | Equals(And(ForAll(_, b1, R1, P), ForAll(_, b2, R2, Q)), ForAll(_, b3, R3, PQ)) 
            when 
                vequal' b1 b2 && vequal' b2 b3 && sequal R1 R2 && sequal R2 R3 && sequal <@@ (%%P:bool) |&| (%%Q:bool) @@> PQ ->
                pattern_desc "Distributivity of \u2200" <@ () @> |> Some
        | Equals(Or(Exists(_, b1, R1, P), Exists(_, b2, R2, Q)), Exists(_, b3, R3, PQ)) 
            when 
                vequal' b1 b2 && vequal' b2 b3 && sequal R1 R2 && sequal R2 R3 && sequal <@@ (%%P:bool) ||| (%%Q:bool) @@> PQ ->
                pattern_desc "Distributivity of \u2203" <@ () @> |> Some
        | _ -> None

    let (|RangeSplit|_|) =
        function
        | Equals(ForAll(_, b1, Or(R1, S1), P1), And(ForAll(_,b2, R, P2), ForAll(_,b3, S, P3))) 
            when 
                vequal' b1 b2 && vequal' b2 b3 && sequal2 R1 S1 R S && sequal P1 P2 && sequal P2 P3 -> pattern_desc "the Range Split" <@ () @> |> Some
        | Equals(Exists(_,b1, Or(R1, S1), P1), Or(Exists(_,b2, R, P2), Exists(_,b3, S, P3))) 
            when 
                vequal' b1 b2 && vequal' b2 b3 && sequal2 R1 S1 R S && sequal P1 P2 && sequal P2 P3 -> pattern_desc "the Range Split" <@ () @> |> Some
        | _ -> None

    let (|Interchange|_|) =
        function
        | Equals(ForAll(_, x, R, ForAll(_, y, Q, P)), ForAll(_, y', Q', ForAll(_ ,x', R' ,P')))
            when 
                vequal' x x' && vequal' y y' && sequal3 P Q R P' Q' R' && not_occurs_free y R && not_occurs_free x Q
                -> pattern_desc "the Interchange of Variables" <@ () @> |> Some
        | Equals(Exists(_, x, R, Exists(_, y, Q, P)), Exists(_, y', Q', Exists(_ ,x', R' ,P')))
            when 
                vequal' x x' && vequal' y y' && sequal P P' && sequal Q Q' && sequal R R' && not_occurs_free y R && not_occurs_free x Q
                -> pattern_desc "the Interchange of Variables" <@ () @> |> Some
        | _ -> None

    let (|Trading|_|) =
        function
        | Equals(ForAll(_, x, R, P), ForAll(_, x', Bool true, Implies(R', P'))) when vequal' x x' && sequal2 R P R' P'-> pattern_desc "Trading" <@ () @> |> Some
        | Equals(Exists(_, x, R, P), Exists(_, x', Bool true, And(R', P'))) when vequal' x x' && sequal2 R P R' P' -> pattern_desc "Trading" <@ () @> |> Some
        | _ -> None
              
    let (|ForAllDistrib|_|) =
        function
        | Equals(Or(P1, ForAll(_, x1, R1, Q1)), ForAll(_, x2, R2, Or(P2, Q2))) 
            when not_occurs_free x1 P1 && vequal' x1 x2 && sequal P1 P2 && sequal2 Q1 R1 Q2 R2 -> pattern_desc "Distributivity of forall" <@ () @> |> Some
        | _ -> None
    
    let equational_logic_axioms = 
        function
        | SEqual x
        | True x // (3.3)
        | False x //(3.8)
        | BinaryOpDef <@ (=) @> <@ (<>) @> <@ (=) @> <@ not @> x // (3.10)
        
        | Assoc <@(=)@> <@ (=) @> x  // (3.1)
        | Assoc <@(=)@> <@ (|||) @> x // (3.25)
        
        | Symm <@ (=) @> x // (3.2)
        | Commute <@ (=) @> <@ (|||) @> x // (3.24)

        | DistribNot x // (3.9) 
        | Distrib <@(=)@> <@ (|||) @> <@ (=) @> x  // (3.27)      
       
        | Idempotency <@(=)@> <@ (|||) @> x // (3.26)
        
        | ExcludedMiddle x // (3.28)
        | GoldenRule x // (3.35)

        | Implication x  // (3.57 and 3.58)
        | Leibniz x  // (3.83)
        
        | EmptyRange x // (8.13)
        | OnePoint x // (8.14)
        | Nesting x
        | Renaming x
        | QuantifierDistrib x 
        | RangeSplit x 
        | Interchange x 
        | Trading x 
        | ForAllDistrib x -> Some (desc x) 
        | _ -> None

    (* Expression functions for admissible rules *) 
       
    /// Reduce logical constants.
    let _reduce_constants  =
        function
        | Equals(Bool l, Bool r) -> Expr.Value((l = r))
        | NotEquals(Bool l, Bool r) -> Expr.Value(l <> r)
        | Not(Bool l) -> Expr.Value(not l)
        | Or(Bool l, Bool r) -> Expr.Value(l ||| r)                
        | And(Bool l, Bool r) -> Expr.Value(l |&| r)
        | Implies(Bool l, Bool r) -> Expr.Value(l ==> r)
        | expr -> expr
    
    /// Binary logical operators are right associative.
    let _right_assoc =
        function
        | Equals(Equals(a1, a2), a3) -> <@@ (%%a1:bool) = ((%%a2:bool) = (%%a3:bool)) @@>
        | Or(Or(a1, a2), a3) -> <@@ (%%a1:bool) ||| ((%%a2:bool)||| (%%a3:bool)) @@>
        | And(And(a1, a2), a3) -> <@@ (%%a1:bool) |&| ((%%a2:bool) |&| (%%a3:bool)) @@>
        | expr -> expr 
    
    /// Binary logical operators are left associative.
    let _left_assoc =
        function
        | Equals(a1, Equals(a2, a3)) -> <@@ ((%%a1:bool) = (%%a2:bool)) = (%%a3:bool) @@>
        | Or(a1, Or(a2, a3)) -> <@@ ((%%a1:bool) ||| (%%a2:bool)) ||| (%%a3:bool) @@>
        | And(a1, And(a2, a3)) -> <@@ ((%%a1:bool) |&| (%%a2:bool)) |&| (%%a3:bool) @@>
        | expr -> expr 
    
    /// Binary logical operators commute.
    let _commute =
        function
        | Equals(a1, a2) -> <@@ (%%a2:bool) = (%%a1:bool) @@>
        | Or(a1, a2) -> <@@ (%%a2:bool) ||| (%%a1:bool) @@>
        | And(a1, a2) -> <@@ (%%a2:bool) |&| (%%a1:bool) @@>
        | expr -> expr 
    
    /// Distribute logical terms.
    let _distrib =
        function
        | Not(Equals(a1, a2)) -> <@@ not (%%a1:bool) = (%%a2:bool) @@>
        | Or(a1, Equals(a2, a3)) -> <@@ (((%%a1:bool))  ||| ((%%a2:bool))) = (((%%a1:bool)) ||| ((%%a3:bool))) @@>
        | Or(p, And(q, r)) -> <@@ ((%%p:bool) ||| (%%q:bool)) |&| ((%%p:bool) ||| (%%r:bool)) @@>
        | Or(p, Or(q, r)) -> <@@ ((%%p:bool) ||| (%%q:bool)) ||| ((%%p:bool) ||| (%%r:bool)) @@>
        | And(p, Or(q, r)) -> <@@ ((%%p:bool) |&| (%%q:bool)) ||| ((%%p:bool) |&| (%%r:bool)) @@>
        | Not(Or(a1, a2)) -> <@@ (not (%%a1:bool)) |&| (not (%%a2:bool)) @@>
        | Not(And(a1, a2)) -> <@@ (not (%%a1:bool)) ||| (not (%%a2:bool)) @@>
        | expr -> expr
    
    /// Collect distributed logical terms.
    let rec _collect =
        function
        | Equals(Not a1, a2)  -> <@@ not((%%a1:bool) = (%%a2:bool)) @@>
        | Equals(Or(a1, a2), Or(a3, a4)) when sequal a1 a3 -> <@@ (%%a1:bool) ||| ((%%a2:bool) = (%%a4:bool)) @@>
        | And(Or(a1, a2), Or(a3, a4)) when sequal a1 a3 -> <@@ (%%a1:bool) ||| ((%%a2:bool) |&| (%%a4:bool)) @@>
        | Or(And(a1, a2), And(a3, a4)) when sequal a1 a3 -> <@@ (%%a1:bool) |&| ((%%a2:bool) ||| (%%a4:bool)) @@>
        | Or(Or(a1, a2), Or(a3, a4)) when sequal a1 a3 -> <@@ (%%a1:bool) ||| ((%%a2:bool) ||| (%%a4:bool)) @@>
        | Or(Not p , Not q) -> <@@ not ((%%p:bool) |&| (%%q:bool)) @@>
        | And(Not p , Not q) -> <@@ not ((%%p:bool) ||| (%%q:bool)) @@>
        | expr -> expr
    
    /// ||| operator is idempotent.    
    let rec _idemp =
        function
        | Or(a1, a2) when sequal a1 a2 -> <@@ (%%a2:bool) @@>
        | Equals(a1, a2) when sequal a1 a2 -> <@@ (%%a2:bool) @@>
        | And(a1, a2) when sequal a1 a2 -> <@@ (%%a2:bool) @@>
        | expr ->expr

    let rec _excluded_middle =
        function
        | Or(a1, Not(a2)) when sequal a1 a2 -> <@@ true @@>
        | expr -> expr

    let rec _golden_rule =
        function
        | And(p, q) -> <@@ (%%p:bool) = (%%q:bool) = ((%%p:bool) ||| (%%q:bool)) @@>
        | expr -> expr

    let rec _def_implies = 
        function
        | Implies(p, q) -> <@@ ((%%p:bool) ||| (%%q:bool)) = (%%q:bool) @@>
        | expr -> expr

    let _shunt =
        function
        | Implies(And(p, q), r) -> <@@ (%%p:bool) ==> (%%q:bool) ==> (%%r:bool) @@>
        | expr -> expr
        | expr -> expr

    let _modus_ponens =
        function
        | And(p1, Implies(p2, q2)) when sequal p1 p2 -> q2
        | expr -> expr

    let _mutual_implication = 
        function
        | And(Implies(p1, q1), Implies(q2, p2)) when sequal2 p1 q1 p2 q2-> <@@ (%%p1:bool) = (%%q1:bool) @@>
        | expr -> expr

    let _subst_and =
        function
        | And(Equals(Var e, Var f), E) when E |> occurs [e] -> 
            let E' = replace_var_var e f E  
            let e' = Expr.Var e
            let f' = Expr.Var f
            <@@ ((%%e':bool) = (%%f':bool)) |&| %%E' @@>
        | expr -> expr

    let _subst_implies =
        function
        | Implies(Equals(Var e, Var f), E) when E |> occurs [e] -> 
            let E' = replace_var_var e f E 
            let e' = Expr.Var e
            let f' = Expr.Var f
            <@@ ((%%e':bool) = (%%f':bool)) ==> %%E' @@>
        | expr -> expr

    let _subst_and_implies =
        function
        | Implies(And(q, Equals(Var e, Var f)), E) when E |> occurs [e] -> 
            let E' = replace_var_var e f E 
            let e' = Expr.Var e
            let f' = Expr.Var f
            <@@ ((%%q:bool) |&| ((%%e':bool) = %%f':bool)) ==> %%E' @@>
        | expr -> expr

    let _subst_true =
        function
        | Implies(Var p,  E) when E |> occurs [p] -> 
            let E' = replace_var_expr p <@ true @> E in 
            let p' = Expr.Var p
            <@@ (%%p':bool) ==> %%E' @@>
        | Implies(And(Var q, Var p),  E) when E |> occurs [p] -> 
            let E' = replace_var_expr p <@ true @> E in 
            let p' = Expr.Var p
            let q' = Expr.Var q
            <@@ ((%%q':bool) |&| (%%p':bool)) ==> %%E' @@>
        | And(Var p, E) when E |> occurs [p] -> 
            let E' = replace_var_expr p <@ true @> E in 
            let p' = Expr.Var p
            <@@ (%%p':bool) |&| %%E' @@>
        | expr -> expr

    let _subst_false =
        function
        | Implies(E, Var p) when E |> occurs [p] -> 
            let E' = replace_var_expr p <@ false @> E in 
            let p' = Expr.Var p
            <@@ (%%E':bool) ==> (%%p':bool) @@>
        | Implies(E, Or(Var p, Var q)) when E |> occurs [p] -> 
            let E' = replace_var_expr p <@ false @> E in 
            let p' = Expr.Var p
            let q' = Expr.Var q
            <@@ ((%%p':bool) ||| (%%q':bool)) ==> %%E' @@>
        | Or(Var p, E) when E |> occurs [p] -> 
            let E' = replace_var_expr p <@ false @> E in 
            let p' = Expr.Var p
            <@@ (%%p':bool) ||| %%E' @@>
        | expr -> expr

    let _subst_or_and = 
        function
        | Equals(E, Or(And(Var p1, Et), And(Not(Var p2), Ef))) when E |> occurs [p1] && p1.Name = p2.Name && Et = replace_var_expr p1 <@ true @> E && Ef = replace_var_expr p2 <@ false @> E -> 
            <@@ true @@>
        | expr -> expr

    let rec _dual = 
        function
        | Bool true -> <@@ false @@>
        | Bool false -> <@@ true @@>
        | Equals(p, q) -> <@@ (%%p:bool) <> (%%q:bool) @@>
        | Not(Equals(p, q)) -> <@@ (%%p:bool) = (%%q:bool) @@>
        | Implies(p, q) -> <@@ not ((%%p:bool) ==> (%%q:bool)) @@>
        | Not(Implies(p, q)) -> <@@ (%%p:bool) ==> (%%q:bool) @@>
        | And(p, q) -> <@@ (%%p:bool) ||| (%%q:bool) @@>
        | Or(p, q) -> <@@ (%%p:bool) |&| (%%q:bool) @@>
        | expr -> traverse expr _dual

    let _distrib_implies =
        function
        | And(p1, Implies(p2, q)) when sequal p1 p2 -> <@@ (%%p1:bool) |&| (%%q:bool) @@>
        | And(p1, Implies(_, p2)) when sequal p1 p2 -> <@@ (%%p1:bool) @@>
        | Or(p1, Implies(p2, q)) when sequal p1 p2 -> <@@ true @@>
        | Or(p1, Implies(q, p2)) when sequal p1 p2 -> <@@ (%%q:bool) ==> (%%p1:bool) @@>
        | Implies(Or(p1,  q1), And(p2,  q2)) when sequal2 p1 q1 p2 q2 -> <@@ (%%p1:bool) = (%%q1:bool) @@>
        | expr -> expr
        
    let _empty_range = 
        function
        | ForAll(_,_,Bool false,_) -> <@@ true @@>
        | Exists(_,_,Bool false,_) -> <@@ false @@>
        | expr -> expr

    let _quantifier_distrib =
        function
        | And(ForAll(_, b1, Bool true, P), ForAll(_, b2, Bool true, Q)) when vequal' b1 b2 -> 
            let t = vars_to_tuple b1 in <@@ forall' (%%t) (%%P:bool) |&| (%%Q:bool) @@>
        | And(ForAll(_, b1, R, P), ForAll(_, b2, R', Q)) when vequal' b1 b2 && sequal R R' -> 
            let t = vars_to_tuple b1 in <@@ forall (%%t) (%%R:bool) ((%%P:bool) |&| (%%Q:bool)) @@> 
        | Or(Exists(_, b1, Bool true, P), Exists(_, b2, Bool true, Q)) when vequal' b1 b2 -> 
            let t = vars_to_tuple b1 in <@@ exists' (%%t) (%%P:bool) ||| (%%Q:bool) @@>
        | Or(Exists(_, b1, R, P), Exists(_, b2, R', Q)) when vequal' b1 b2 && sequal R R' -> 
            let t = vars_to_tuple b1 in <@@ exists (%%t) (%%R:bool) ((%%P:bool) ||| (%%Q:bool)) @@> 
        | expr -> expr

    let _trading = 
        function
        | ForAll(_, x, R, P) -> let v = vars_to_tuple x in call <@ forall @> (v::(<@@ true @@>)::(<@@ (%%R:bool) ==> (%%P:bool)@@>)::[])
        | Exists(_, x, R, P) -> let v = vars_to_tuple x in call <@ exists @> (v::(<@@ true @@>)::(<@@ (%%R:bool) |&| (%%P:bool)@@>)::[])
        | expr -> expr

    let _distrib_forall =
        function
        | Or(P, ForAll(_, x, R, Q)) when not_occurs_free x P -> let v = vars_to_tuple x in call <@ forall @> (v::R::(<@@ %%P ||| %%Q @@>)::[])
        | expr -> expr