namespace Sylvester
#nowarn "40"

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape
open FSharp.Reflection

open Descriptions

/// Patterns used in formulas and axioms
module Patterns =   
    (* Formula patterns *)  
    
    let (|Equals|_|) = 
         function
         | SpecificCall <@@ (=) @@> (None,_,l::r::[]) when l.Type = r.Type -> Some(l, r)
         | _ -> None
   
    let (|Not|_|) =
        function
        | SpecificCall <@@ not @@> (None,_,l::[]) -> Some l
        | _ -> None

    let (|NotEquals|_|) =
         function
         | SpecificCall <@@ (<>) @@> (None,_,l::r::[]) -> Some (l, r)
         | _ -> None

    let (|And|_|) =
        function
        | SpecificCall <@@ (|&|) @@> (None,_,l::r::[]) -> Some (l, r)
        | _ -> None

    let (|Or|_|) =
        function
        | SpecificCall <@@ (|||) @@> (None,_,l::r::[]) -> Some (l, r)
        | _ -> None

    let (|Implies|_|) =
        function
        | SpecificCall <@@ (==>) @@> (None,_,l::r::[]) -> Some (l, r)
        | _ -> None

    let (|Conseq|_|) =
        function
        | SpecificCall <@@ (<==) @@> (None,_,l::r::[]) -> Some (l, r)
        | _ -> None

    let (|Argument|_|) =
        let rec get_conjuncts =
            function
            | And(L, R) as c -> [c] @ get_conjuncts L @ get_conjuncts R
            | expr  -> [expr]
        function
        | Implies(a, c) -> (a, c, get_conjuncts a) |> Some
        | _ -> None

    let (|Binary|_|) (op:Expr<'t->'t->'u>) =
        function
        | SpecificCall op (None,_,l::r::[]) when l.Type = typeof<'t> && r.Type = typeof<'t> -> Some (l,r)
        | _ -> None

    let (|Unary|_|) (op:Expr<'t->'u>) =
        function
        | SpecificCall op (None,_,r::[]) when r.Type = typeof<'t> -> Some r
        | _ -> None

    let (|BinaryCall|_|) =
        function
        | Call(_,_,l::r::[]) when l.Type = r.Type -> Some (l,r)
        | _ -> None

    let (|UnaryCall|_|)  =
        function
        | Call(_,_,r::[]) -> Some r
        | _ -> None

    let (|Val|_|) (v:'t) =
        function
        | Value(z, t) when (t = typeof<'t>) && ((z :?> 't) = v) -> Some (Expr.Value(v))
        | _ -> None

    let (|Add|_|) =
        function
        | Call(None, Op "op_Addition" ,l::r::[]) -> Some(l, r)
        | _ -> None

    let (|Subtract|_|) =
        function
        | Call(None, Op "op_Subtraction" ,l::r::[]) -> Some(l, r)
        | _ -> None

    let (|Multiply|_|) =
        function
        | Call(None, Op "op_Multiply" ,l::r::[]) -> Some(l, r)
        | _ -> None

    let (|Divide|_|) =
        function
        | Call(None, Op "op_Division" ,l::r::[]) -> Some(l, r)
        | _ -> None
    
    let (|Negate|_|) =
        function
        | Call(None, Op "op_UnaryNegation" ,r::[]) -> Some r 
        | _ -> None

    let (|Range|_|) =
        function
        | SpecificCall <@@ (..) @@> (None,_,l::r::[]) -> Some(l,r)
        | _ -> None

    let (|Sequence|_|) =
        function
        | Call(None, mi0, Call(None, mi1, Lambda(_, Call(None, mi2, v))::[])::[]) when mi0.Name = "CreateSequence" && mi1.Name = "Delay" && mi2.Name = "Singleton" 
            ->  v |> List.map expand |> Some
        | Call (None, mi, e) when mi.Name = "InitializeInfinite" -> e |> List.map expand |> Some
        | List l -> l |> List.map expand |> Some
        | NewArray(_, a) -> a |> List.map expand |> Some
        | _ -> None

    let (|BoundVars|_|) =
        function
        | NewTuple(bound) -> bound |> List.map get_vars |> List.concat |> Some
        | ExprShape.ShapeVar bound -> [bound] |> Some
        | ValueWithName(v,t,n) -> let bound = Var(n, t) in [bound] |> Some 
        | Coerce(ValueWithName(v,t,n), ct) when ct = typeof<obj> -> let bound = Var(n, t) in [bound] |> Some
        | _ -> None

    let (|Index|_|) =
        function
        | Call(Some h, mi,  BoundVars(bound)::[]) when mi.Name = "Item" -> Some(h, bound)
        | _ -> None

    let (|ForAll|_|) =
        function
        | Call(None, mi, BoundVars(bound)::range::body::[]) when mi.Name = "forall" -> Some(<@@ forall @@>, bound, range, body)
        | _ -> None

    let (|Exists|_|) =
        function
        | Call(None, mi, BoundVars(bound)::range::body::[]) when mi.Name = "exists" -> Some(<@@ exists @@>, bound, range, body)
        | _ -> None

    let (|Sum|_|) =
        function
        | Call(None, mi, op::Value(_, t)::BoundVars(bound)::range::body::[]) when mi.Name = "sum" && t = typeof<string> -> Some(op, bound, range, body)
        | _ -> None

    let (|Product|_|) =
        function
        | Call(None, mi, op::Value(_, t)::BoundVars(bound)::range::body::[]) when mi.Name = "product" && t = typeof<string> -> Some(op, bound, range, body)
        | _ -> None

    let (|Quantifier|_|) =
        function
        | Sum x
        | Product x
        | ForAll x
        | Exists x -> let (op, bound, range, body) = x in Some (op, bound, range, body)
        | _ -> None
    
    let bound_vars =
        function
        | Quantifier(_,bound, _, _) -> bound 
        | expr -> failwithf "The expression %s is not a valid quantifier expression." (src expr)

    let fail_if_not_bound_vars =
        function
        | BoundVars _ -> ()
        | n -> failwithf "The expression %s is not a bound variables expression." (src n)

    let rec occurs_free (vars:Var list) = 
        function
        | Quantifier(_, bound, _, Quantifier(_, _, _, body)) -> 
            let all_vars = body |> get_vars
            vars |> List.exists (fun v -> (all_vars |> List.exists (fun av -> vequal av v)) && not (bound |> List.exists (fun bv -> vequal bv v)))
        | Quantifier(_,bound, _, body) -> 
            let all_vars = body |> get_vars
            vars |> List.exists (fun v -> (all_vars |> List.exists (fun av -> vequal av v)) && not (bound |> List.exists (fun bv -> vequal bv v)))
        | Quantifier(_,_,_,Application(_, _)) -> false 
        | ShapeVar _ -> false
        | ShapeLambda (bound, body) -> 
            let all_vars = body |> get_vars
            vars |> List.exists (fun v -> (all_vars |> List.exists (fun av -> vequal av v)) && not (vequal bound v))
        | ShapeCombination (_, exprs) -> List.map (occurs_free vars) exprs |> List.contains(true)
        
    let not_occurs_free (vars:Var list) expr  = not (occurs_free vars expr)
    
    let get_quantifiers expr =
        let rec rget_quantifiers prev =
            function
            | Quantifier (_,_,_,body) as q -> prev @ [q] @ (rget_quantifiers prev body)
            | ShapeVar _ -> prev
            | ShapeLambda (_, body) -> rget_quantifiers prev body
            | ShapeCombination (_, exprs) ->  List.map (rget_quantifiers prev) exprs |> List.collect id
        rget_quantifiers [] expr
        
    (* Formula display patterns *)

    let (|Const|_|) =
        function
        | ValueWithName(_, _, x) -> box x |> Some
        | NewUnionCase(x, _) -> box x |> Some
        | Call(None, x, []) -> box x |> Some
        | PropertyGet(None, x, []) -> box x |> Some
        | _ -> None

    let (|UnaryTerm|_|)  =
        function
        | Call(_, mi, l::[]) -> Some (mi, l)
        | _ -> None

    let (|BinaryTerm|_|) =
        function
        | Call(_, mi,l::r::[]) when l.Type = r.Type -> Some (mi, l, r)
        | _ -> None

    let (|SumTerm|_|) =
        function
        | Call(None, mi, op::Value(symbol, t)::BoundVars(bound)::range::body::[]) when mi.Name = "sum" && t = typeof<string> -> Some(op, symbol :?> string, bound, range, body)
        | _ -> None

    let (|ProductTerm|_|) =
        function
        | Call(None, mi, op::Value(symbol, t)::BoundVars(bound)::range::body::[]) when mi.Name = "product" && t = typeof<string> -> Some(op, symbol :?> string, bound, range, body)
        | _ -> None

    let (|PrimitiveTerm|_|) = 
        function
        | Var _
        | Const _ -> Some PrimitiveTerm
        | _ -> None

    let (|BoolVarTerm|_|) =
        function
        | Var v when v.Type = typeof<bool> -> v.Name |> Some
        | _ -> None

    let (|NatVarTerm|_|) =
        function
        | Var v when v.Type = typeof<uint32> -> v.Name |> Some
        | _ -> None
    
    (* Axiom patterns *)

    /// Main axiom of Sylph's symbolic equality. A and B are equal if they are: 
    /// * Syntactically valid and type-checked F# expressions
    /// * Decomposed to the same sequence of symbols i.e. strings.
    /// This law encompasses all 4 of the equational logic laws of equality:
    /// Symmetry, reflexivity, transitivity, and Leibniz's rule: A = B <=> S(A) = S(B)
    let (|SEqual|_|) =
        function
        | Equals(A, B) when sequal A B -> pattern_desc' "Symbolic Equality" |> Some
        | _ -> None

    /// (x = x)
    let (|Reflex|_|) (op:Expr<'t->'t->bool>) =
        function
        | Binary op (a1, a2) when sequal a1 a2 -> 
            pattern_desc' (sprintf "Reflexivity of %s" (src op)) |> Some
        | _ -> None

    /// (x + y) + z = x + (y + z)
    let (|Assoc|_|) (eq:Expr<'t->'t->bool>)  (op:Expr<'t->'t->'t>) =
        function
        | Binary eq (Binary op (Binary op (a1, a2), a3), Binary op (b1, Binary op (b2, b3))) when sequal3 a1 a2 a3 b1 b2 b3 -> 
            pattern_desc' "Associativity" |> Some
        | _ -> None

    /// (x = y = y) = x
    let (|Symm|_|) (op:Expr<'t->'t->'t>)   =
        function
        | Binary op (Binary op (Binary op (a1, a2), a3), a4)  when sequal2 a1 a2 a4 a3-> pattern_desc' "Symmetry" |> Some
        | _ -> None 

    /// (x + y) = (y + x)
    let (|Commute|_|) (eq:Expr<'t->'t->bool>)  (op: Expr<'t->'t->'t>) =
        function
        | Binary eq (Binary op (a1, a2), Binary op (b1, b2)) when sequal2 a1 a2 b2 b1 -> pattern_desc' "Commutativity" |> Some   
        | _ -> None

    /// x + 0 = x
    let (|Identity|_|) (eq:Expr<'t->'t->bool>) (op: Expr<'t->'t->'t>) (zero:Expr<'t>)   = 
        function
        | Binary eq (Binary op (a1, z), a2) when sequal a1 a2 && sequal zero z -> pattern_desc' "Identity" |> Some
        | _ -> None

    /// x * (y + z) = x * y + x * z
    let (|Distrib|_|) (eq:Expr<'t->'t->bool>)  (op1: Expr<'t->'t->'t>) (op2: Expr<'t->'t->'t>)  = 
        function
        | Binary eq (Binary op1 (a3, Binary op2 (b3, b4)), Binary op2 (Binary op1 (a1, b1), Binary op1 (a2, b2))) when (sequal a1 a2) && (sequal a1 a3) && sequal2 b1 b2 b3 b4 -> 
                pattern_desc' "Distributivity" |> Some
        | _ -> None

    ///  -(y + z) = -y  * - z
    let (|UnaryDistrib|_|) (eq:Expr<'t->'t->bool>)  (op1: Expr<'t->'t>) (op2: Expr<'t->'t->'t>)  = 
        function
        | Binary eq (Unary op1 (Binary op2 (a1, a2)), Binary op2 (Unary op1 a3, Unary op1 a4)) when sequal a1 a3 && sequal a2 a4 -> 
                pattern_desc' "Distributivity" |> Some
        | _ -> None

    /// x + (-x) = zero
    let (|Inverse|_|) (eq:Expr<'t->'t->bool>)  (op: Expr<'t->'t->'t>) (inverse: Expr<'t -> 't>) (zero: Expr<'t>)   =
        function
        | Binary eq (Binary op (a1, Unary inverse (a2)), z)  when sequal a1 a2 && sequal zero z -> pattern_desc' "Definition of Inverse" |> Some
        | _ -> None

    /// x + x = x
    let (|Idempotency|_|) (eq:Expr<'t->'t->bool>)  (op: Expr<'t->'t->'t>) = 
        function
        | Binary eq (Binary op (a1, a2), a3) when sequal a1 a2 && sequal a1 a3 -> pattern_desc' "Idempotency" |> Some
        | _ -> None

    /// not (x = y) = not(x) <> not(y)
    let (|Duality|_|) (eq:Expr<'t->'t->bool>)  (op1: Expr<'t->'t->'t>) (op2: Expr<'t->'t->'t>) (inverse: Expr<'t->'t>) =
        function
        | Binary eq (Unary inverse (Binary op1 (a1, a2)), Binary op2 (Unary inverse b1, Unary inverse b2)) when sequal2 a1 a2 b2 b1 -> 
            pattern_desc' "Duality" |> Some
        | _ -> None

    /// Define the LHS by the RHS
    let (|Def|_|) (eq:Expr<'t->'t->bool>) (l:Expr<'t>) (r:Expr<'t>) =
        function
        | Binary eq (a1, a2) when sequal2 a1 a2 l r -> pattern_desc (sprintf "Definition of %s" (src l)) <@ (%eq) %l %r @> |> Some
        | _ -> None

    /// Define a binary operator by another binary operator and a unary operator applied to the entire expression e.g p <> q = not (p = q).
    let (|BinaryOpDef|_|) (eq:Expr<'t->'t->bool>)  (op1:Expr<'t->'t->'t>) (op2:Expr<'t->'t->'t>) (op3:Expr<'t->'t>)=
        function
        | Binary eq (Binary op1 (a1, a2), Unary op3 (Binary op2 (a3, a4))) when sequal2 a1 a2 a3 a4 -> pattern_desc' (sprintf "Definition of %s" (src op1)) |> Some
        | _ -> None

    //. Define a binary operator by another binary operator and a unary operator applied to the left of the expression
    let (|BinaryOpDefL|_|) (eq:Expr<'t->'t->bool>)  (op1:Expr<'t->'t->'t>) (op2:Expr<'t->'t->'t>) (op3:Expr<'t->'t>)=
        function
        | Binary eq (Binary op1 (a1, a2), (Binary op2 (Unary op3 a3, a4))) when sequal2 a1 a2 a3 a4 -> pattern_desc' (sprintf "Definition of %s" (src op1)) |> Some
        | _ -> None

    let (|BinaryOpDefR|_|) (eq:Expr<'t->'t->bool>)  (op1:Expr<'t->'t->'t>) (op2:Expr<'t->'t->'t>) (op3:Expr<'t->'t>)=
        function
        | Binary eq (Binary op1 (a1, a2), (Binary op2 (a3, Unary op3 a4))) when sequal2 a1 a2 a3 a4 -> pattern_desc' (sprintf "Definition of %s" (src op1)) |> Some
        | _ -> None

    let (|LeftCancel|_|) (op:Expr<'t->'t->'t>)  =
        function
        | Equals (Equals(Binary op (a1, b), Binary op (a2, c)), Equals(b1, c1)) when sequal a1 a2 && sequal b b1 && sequal c c1 
            -> pattern_desc' "Left Cancellation" |> Some
        | _ -> None

    let (|RightCancel|_|) (op:Expr<'t->'t->'t>)  =
        function
        | Equals (Equals (Binary op (b, a1), Binary op (c, a2)), Equals (b1, c1)) when sequal a1 a2 && sequal b b1 && sequal c c1 
            -> pattern_desc' "Right Cancellation" |> Some
        | _ -> None

    let (|LeftCancelNonZero|_|) (op:Expr<'t->'t->'t>) (zero:Expr<'t>)  =
        function
        | Implies(NotEquals(a, z), Equals (Equals(Binary op (a1, b), Binary op (a2, c)), Equals(b1, c1))) when sequal z zero && sequal a a1 && sequal a1 a2 && sequal b b1 && sequal c c1 
            -> pattern_desc "Left Cancellation" <@ fun a b c -> (a <> %zero) ==> (((%op) a b = (%op) a c)) = ((b = c)) @> |> Some
        | _ -> None

    let (|OnePoint|_|) =
        function
        | Equals(Quantifier(_,[x], Equals(Var x', E), P), P') when not (occurs_free [x] E) && vequal x x' && sequal P' (subst_var_value x E P) -> 
            pattern_desc "the One-Point Rule" <@ fun x E P -> (forall x (x = E) P) = P @> |> Some
        | _ -> None

    let (|Nesting|_|) =
        function
        | Equals(Quantifier(_, x::y, R1, P), Quantifier(_, [x'], R2, Quantifier(_,y', R3, P'))) 
            when not_occurs_free y R1 && vequal x x' && vequal' y y' && sequal P P' && sequal R1 R2 && sequal R2 R3-> pattern_desc' "Interchange Variables" |> Some
        | Equals(Quantifier(_, x::y, And(R, Q), P), Quantifier(_, [x'], R',Quantifier(_,y', Q', P'))) 
            when not_occurs_free y R && vequal x x' && vequal' y y' && sequal3 R Q P R' Q' P'-> pattern_desc' "Interchange Variables" |> Some
        | _ -> None

    let (|Renaming|_|) =
        function
        | Equals(Quantifier(_, x, R, P), Quantifier(_, y, R', P')) 
            when not (vequal' x y) && x.Length = y.Length && not_occurs_free y R && not_occurs_free y P && sequal R' (replace_var_var' x y R) && sequal P' (replace_var_var' x y P) -> pattern_desc' "Renaming Variables" |> Some
        | _ -> None