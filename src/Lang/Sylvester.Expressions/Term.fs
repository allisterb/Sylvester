namespace Sylvester

open System
open System.Collections.Generic

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

[<AbstractClass; StructuredFormatDisplay("{Display}")>]
type Term<'t when 't: equality> (expr:Expr<'t>, ?h:TermHistory) =
    member val Expr = expr

    member val Val = match expr with | Patterns.Value(v, _) -> v :?> 't |> Some | _ -> None
    
    member val History = h

    member val Attrs:Dictionary<string, obj> = new Dictionary<string, obj>()
    
    abstract member Display: string
    
    interface IEquatable<Term<'t>> with
        member a.Equals b = a.Display = b.Display

    interface IAttrs with member x.Attrs = x.Attrs

    override a.Equals (_b:obj) = 
            match _b with 
            | :? Term<'t> as b -> (a :> IEquatable<Term<'t>>).Equals b
            | _ -> false
    
    override a.GetHashCode() = a.Expr.GetHashCode()

    override a.ToString() = a.Display

    static member op_Implicit (l:Term<'t>):Expr<'t> = l.Expr
 
    static member (==) (l:Term<'t>, r:Term<'t>) = <@ %l.Expr = %r.Expr @> |> Prop

    static member (==) (l:Term<'t>, r:'t) = let r' = exprv r in <@ %l.Expr = %r' @> |> Prop
    
    static member (==) (l:'t, r:Term<'t>) = let l' = exprv l in <@ %l' = %r.Expr @> |> Prop

and [<AbstractClass>] TermVar<'t when 't: equality>(n: string) = 
    inherit Term<'t>(Expr.Var(Var(n, typeof<'t>)) |> expand_as<'t>)
    override x.Display = sprintf "{%A}" n
    member x.Name = n
    member x.Var = match x.Expr with | Var v -> v | _ -> failwith ""
    
and IndexVar(expr: Expr<int>) = 
    inherit Term<int>(expr)
    member val Name = src expr
    new (n: string) = IndexVar(Expr.Var(Var(n, typeof<int>)) |> expand_as<int>)
    override x.Display = src expr
    static member (+) (l:IndexVar, r:int) = let v = exprv r in IndexVar(<@ %l.Expr + %v @>)
    static member (+) (l:int, r:IndexVar) = let v = exprv l in IndexVar(<@ %v + %r.Expr @>)
    static member (+) (l:IndexVar, r:IndexVar) = IndexVar(<@ %l.Expr + %r.Expr @>)
    static member (-) (l:IndexVar, r:int) = let v = exprv r in IndexVar(<@ %l.Expr - %v @>)
    static member (-) (l:int, r:IndexVar) = let v = exprv l in IndexVar(<@ %v - %r.Expr @>)
    static member (-) (l:IndexVar, r:IndexVar) = IndexVar(<@ %l.Expr - %r.Expr @>)

and [<AbstractClass>] TermConst<'t when 't: equality>(n: string) = 
    inherit Term<'t>(Expr.ValueWithName(Unchecked.defaultof<'t>, n) |> expand_as<'t>)

and Scalar<'t when 't: equality and 't :> ValueType and 't :> IEquatable<'t>> (expr:Expr<'t>, ?h:TermHistory) =
    inherit Term<'t>(expr, ?h=h)
    
    new (v: 't) = Scalar<'t>(exprv v)

    override x.Display = sprinte expr

    override a.Equals (_b:obj) = 
        match _b with 
        | :? Scalar<'t> as b -> (a :> IEquatable<Term<'t>>).Equals b
        | _ -> false
    
    override a.GetHashCode() = a.Display.GetHashCode()

    member x.Html() = "$" + latexe x.Expr + "$"

    interface ISymbolic<Scalar<'t>, 't> with
           member a.Term = a
           member a.Expr = expr
           member a.Attrs = a.Attrs
           member a.Symbol = None
           member a.Transform(e:Expr<'t>, ?attrs, ?s) = 
                let s = Scalar e
                do s.Attrs.Replace(defaultArg attrs null) |> ignore
                s
           
    interface IHtmlDisplay with
        member x.Html() = x.Html()    

    static member Zero = typeof<'t> |> zero_val |> expand_as<'t> |> Scalar<'t>

    static member One = typeof<'t> |> one_val |> expand_as<'t> |> Scalar<'t>

    static member NegOne = typeof<'t> |> neg_one_val |> expand_as<'t> |> Scalar<'t>

    static member op_Implicit (l:Scalar<'t>):Expr<'t> = l.Expr

    static member op_Implicit (l:'t):Scalar<'t> = Scalar (exprv l)

    static member op_Implicit (l:int):Scalar<real> = let v = real l in Scalar (exprv v)

    static member op_Implicit (l:rat):Scalar<real> = let v = real l in Scalar (exprv v)

    static member op_Implicit (l:nat):Scalar<real> = let v = real l in Scalar (exprv v)

    (* Unary operators *)

    static member (~-) (l:Scalar<'t>) = call_neg l.Expr |> expand_as<'t> |> Scalar<'t>

    static member (!) (l:Scalar<'t>) = l |> sexpr |> ev

    (* Binary operators *)

    static member (+) (l:Scalar<'t>, r:Scalar<'t>) = call_add (l.Expr) (r.Expr) |> expand_as<'t> |> simplifye |> Scalar<'t>
    
    static member (+) (l:Scalar<'t>, r:'t) = call_add (l.Expr) (Expr.Value r) |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member (+) (l:'t, r:Scalar<'t>) = call_add (Expr.Value l) r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member (+) (l:Expr<'t>, r:Scalar<'t>) = call_add l r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>
    
    static member (+) (l:Scalar<'t>, r:Expr<'t>) = call_add (l.Expr) r |> expand_as<'t> |> simplifye |> Scalar<'t>
    
    static member (+) (l:Scalar<real>, r:int) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member (+) (l:Scalar<real>, r:rat) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand_as<'t> |> simplifye |> Scalar<'t>
    
    static member (+) (l:Scalar<real>, r:nat) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member (+) (l:int, r:Scalar<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member (+) (l:rat, r:Scalar<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>
    
    static member (+) (l:nat, r:Scalar<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>
    
    static member (+) (l:Scalar<rat>, r:int) = call_add (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member (+) (l:int, r:Scalar<rat>) = call_add (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member (+) (l:Scalar<int>, r:nat) = call_add (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> simplifye |> Scalar<'t>
    
    static member (-) (l:Scalar<'t>, r:Scalar<'t>) = call_sub (l.Expr) (r.Expr) |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (-) (l:Scalar<'t>, r:'t) = call_sub (l.Expr) (Expr.Value r) |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (-) (l:'t, r:Scalar<'t>) = call_sub (Expr.Value l) r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (-) (l:Expr<'t>, r:Scalar<'t>) = call_sub l r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (-) (l:Scalar<'t>, r:Expr<'t>) = call_sub (l.Expr) r |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (-) (l:Scalar<real>, r:int) = let r' = (real) r in call_sub (l.Expr) (Expr.Value r') |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (-) (l:Scalar<real>, r:rat) = let r' = (real) r in call_sub (l.Expr) (Expr.Value r') |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (-) (l:Scalar<real>, r:nat) = let r' = (real) r in call_sub (l.Expr) (Expr.Value r') |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (-) (l:int, r:Scalar<real>) = let l' = (real) l in call_sub (Expr.Value l') r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (-) (l:rat, r:Scalar<real>) = let l' = (real) l in call_sub (Expr.Value l') r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (-) (l:nat, r:Scalar<real>) = let l' = (real) l in call_sub (Expr.Value l') r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (-) (l:Scalar<rat>, r:int) = call_sub (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (-) (l:int, r:Scalar<rat>) = call_sub (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (-) (l:Scalar<int>, r:nat) = call_sub (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (*) (l:Scalar<'t>, r:Scalar<'t>) = call_mul (l.Expr) (r.Expr) |> expand_as<'t> |> simplifye|> Scalar<'t>
    
    static member (*) (l:Scalar<'t>, r:'t) = call_mul (l.Expr) (Expr.Value r) |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (*) (l:'t, r:Scalar<'t>) = call_mul (Expr.Value l) r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (*) (l:Expr<'t>, r:Scalar<'t>) = call_mul l r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>
    
    static member (*) (l:Scalar<'t>, r:Expr<'t>) = call_mul (l.Expr) r |> expand_as<'t> |> simplifye|> Scalar<'t>
    
    static member (*) (l:Scalar<real>, r:int) = let r' = (real) r in call_mul (l.Expr) (Expr.Value r') |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (*) (l:Scalar<real>, r:rat) = let r' = (real) r in call_mul (l.Expr) (Expr.Value r') |> expand_as<'t> |> simplifye|> Scalar<'t>
    
    static member (*) (l:Scalar<real>, r:nat) = let r' = (real) r in call_mul (l.Expr) (Expr.Value r') |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (*) (l:int, r:Scalar<real>) = let l' = (real) l in call_mul (Expr.Value l') r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (*) (l:rat, r:Scalar<real>) = let l' = (real) l in call_mul (Expr.Value l') r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>
    
    static member (*) (l:nat, r:Scalar<real>) = let l' = (real) l in call_mul (Expr.Value l') r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>
    
    static member (*) (l:Scalar<rat>, r:int) = call_mul (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (*) (l:int, r:Scalar<rat>) = call_mul (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (*) (l:Scalar<int>, r:nat) = call_mul (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (/) (l:Scalar<'t>, r:Scalar<'t>) = call_div (l.Expr) (r.Expr) |> expand_as<'t> |> simplifye|> Scalar<'t>
    
    static member (/) (l:Scalar<'t>, r:'t) = call_div (l.Expr) (Expr.Value r) |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (/) (l:'t, r:Scalar<'t>) = call_div (Expr.Value l) r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (/) (l:Expr<'t>, r:Scalar<'t>) = call_div l r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>
    
    static member (/) (l:Scalar<'t>, r:Expr<'t>) = call_div (l.Expr) r |> expand_as<'t> |> simplifye|> Scalar<'t>
    
    static member (/) (l:Scalar<real>, r:int) = let r' = (real) r in call_div (l.Expr) (Expr.Value r') |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (/) (l:Scalar<real>, r:rat) = let r' = (real) r in call_div (l.Expr) (Expr.Value r') |> expand_as<'t> |> simplifye|> Scalar<'t>
    
    static member (/) (l:Scalar<real>, r:nat) = let r' = (real) r in call_div (l.Expr) (Expr.Value r') |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (/) (l:int, r:Scalar<real>) = let l' = (real) l in call_div (Expr.Value l') r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (/) (l:rat, r:Scalar<real>) = let l' = (real) l in call_div (Expr.Value l') r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>
    
    static member (/) (l:nat, r:Scalar<real>) = let l' = (real) l in call_div (Expr.Value l') r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>
    
    static member (/) (l:Scalar<rat>, r:int) = call_div (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (/) (l:int, r:Scalar<rat>) = call_div (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member (/) (l:Scalar<int>, r:nat) = call_div (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> simplifye|> Scalar<'t>

    static member ( ***) (l : Scalar<'t>, r : Scalar<'t>) = call_pow l.Expr r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>
    
    static member ( ***) (l : Scalar<'t>, r : int) =  
        Convert.ChangeType(r, typeof<'t>) :?> 't |> exprv |> call_pow l.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member ( ***) (l : Scalar<real>, r : real) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> simplifye |> Scalar<'t>
    
    static member ( ***) (l : Scalar<real>, r : rat) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member ( ***) (l : Scalar<real>, r : nat) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member ( ***) (l : Scalar<rat>, r : real) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member ( ***) (l : Scalar<rat>, r : rat) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member ( ***) (l : Scalar<rat>, r : int) = call_pow l.Expr (Expr.Value (Rational(r, 1))) |> expand_as<'t> |> simplifye |> Scalar<'t>
         
    static member ( ***) (l : Scalar<int>, r : int) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> simplifye |> Scalar<'t>
         
    static member ( ***) (l : Scalar<nat>, r : rat) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> simplifye |> Scalar<'t>
    
    static member ( ***) (l : real, r : Scalar<real>) = call_pow (Expr.Value l) r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member ( ***) (l : int, r : Scalar<real>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member ( ***) (l : rat, r : Scalar<real>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member ( ***) (l : nat, r : Scalar<real>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member ( ***) (l : int, r : Scalar<int>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>
    
    static member ( ***) (l : rat, r : Scalar<rat>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>
    
    static member ( ***) (l : nat, r : Scalar<nat>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> simplifye |> Scalar<'t>

    static member (==) (l:Scalar<'t>, r:Scalar<'t>) = ScalarEquation<'t>(l, r)

    static member (==) (l:Scalar<'t>, r:'t) = ScalarEquation<'t>(l, r |> exprv |> Scalar<'t>)

    static member (==) (l:'t, r:Scalar<'t>) = ScalarEquation<'t>(l |> exprv |> Scalar<'t>, r)

    static member (==) (l:ScalarVar<'t>, r:Scalar<'t>) = ScalarVarMap<'t>(l, r)

    static member (==) (l:ScalarVar<'t>, r:'t) = ScalarVarMap<'t>(l, r |> exprv |> Scalar<'t>)
    
    static member (<+) (l:Scalar<real>, r:Scalar<real>)  = ScalarRelation<real>(l, r, <@ (<) @>)

    static member (<+) (l:Scalar<real>, r:real)  = ScalarRelation<real>(l, r |> exprv |> Scalar<real>, <@ (<) @>)

    static member (<+) (l:real, r:Scalar<real>)  = ScalarRelation<real>(l |> exprv |> Scalar<real>, r, <@ (<) @>)

    static member (<+) (l:ScalarVar<real>, r:Scalar<real>) = ScalarVarRelation<real>(l, r, <@ (<) @>)
    
    static member (<+) (l:ScalarVar<real>, r:real)  = ScalarVarRelation<real>(l, r |> exprv |> Scalar<real>, <@ (<) @>)

    static member (+>) (l:Scalar<real>, r:Scalar<real>)  = ScalarRelation<real>(l, r, <@ (>) @>)

    static member (+>) (l:Scalar<real>, r:real)  = ScalarRelation<real>(l, r |> exprv |> Scalar<real>, <@ (>) @>)

    static member (+>) (l:real, r:Scalar<real>)  = ScalarRelation<real>(l |> exprv |> Scalar<real>, r, <@ (>) @>)

    static member (+>) (l:ScalarVar<real>, r:Scalar<real>)  = ScalarVarRelation<real>(l, r, <@ (>) @>)
    
    static member (+>) (l:ScalarVar<real>, r:real)  = ScalarVarRelation<real>(l, r |> exprv |> Scalar<real>, <@ (>) @>)
    
and ScalarVar<'t when 't: equality and 't :> ValueType and 't :> IEquatable<'t>> (expr:Expr<'t>, ?label:string) = 
    inherit Scalar<'t>(expr)
    new(n:string, ?label:string) = ScalarVar<'t>(Expr.Var(Var(n, typeof<'t>)) |> expand_as<'t>, ?label=label)
    member x.Name = src expr
    member val Label = (defaultArg label (src expr)) with get,set 
    member x.Var = match x.Expr with | Var v -> v | _ -> failwith ""
    member x.Item(i:IndexVar) = ScalarIndexedVar<'t>(x, i)
    member x.Item(i:int) = ScalarVar<'t>(x.Name + i.ToString())
    member internal x.Item(e:Expr<int>) = Unchecked.defaultof<'t>

and ScalarIndexedVar<'t when 't: equality and 't :> ValueType and 't :> IEquatable<'t>>(var:ScalarVar<'t>, index:IndexVar) =
    inherit ScalarVar<'t>(<@ var.[index.Expr] @>)
    
and ScalarConst<'t when 't: equality and 't :> ValueType and 't :> IEquatable<'t>>(n: string, ?v:'t) = 
    inherit Scalar<'t>(Expr.ValueWithName((defaultArg v Unchecked.defaultof<'t>), n) |> expand_as<'t>)
    member val Name = n
    member val Val = defaultArg v Unchecked.defaultof<'t>

    //static member (~%!) (l:ScalarConst<'t>) = ScalarVar<'t> (l.Name)

and ScalarRelation<'t when 't: equality and 't :> ValueType and 't :> IEquatable<'t>>(lhs:Scalar<'t>, rhs:Scalar<'t>, op:Expr<'t->'t->bool>) =
    inherit Prop(expand_as<bool> <@ (%op) %lhs.Expr %rhs.Expr @>)
    member val Lhs = lhs
    member val Rhs = rhs
    member val Op = op
    member val ScalarVars = (lhs.Expr |> get_vars |> List.map (exprvar<'t> >> ScalarVar<'t>)) @ (rhs.Expr |> get_vars |> List.map (exprvar<'t> >> ScalarVar<'t>))
    member x.Fix<'a>(o:'a) = 
        let lhs = fix o x.Lhs in
        let rhs = fix o x.Rhs in
        ScalarRelation<'t>(lhs, rhs, x.Op)
    member x.SubstVar(v:ScalarVar<'t>, s:Scalar<'t>) =
        let l = subst_var_value v.Var s.Expr lhs.Expr |> expand_as<'t> |> simplifye |> Scalar<'t> in
        let r = subst_var_value v.Var s.Expr rhs.Expr |> expand_as<'t> |> simplifye |> Scalar<'t> in
        ScalarRelation(l, r, op)
    member x.Html() = "$$" + latexe x.Expr + "$$"
    override x.Display = sprintf "%s %s %s" (sprinte x.Lhs.Expr) ((src op).Replace("(", "").Replace(")", "")) (sprinte x.Rhs.Expr)
    
    interface IHtmlDisplay with
        member x.Html() = "$$" + latexe x.Expr + "$$"

and ScalarEquation<'t when 't: equality and 't :> ValueType and 't :> IEquatable<'t>>(lhs:Scalar<'t>, rhs:Scalar<'t>) =
    inherit ScalarRelation<'t>(lhs, rhs, <@ (=) @>)
    member x.Fix<'a>(o:'a) = 
        let lhs = fix o x.Lhs in
        let rhs = fix o x.Rhs in
        ScalarEquation<'t>(lhs, rhs)
    member x.SubstVar(v:ScalarVar<'t>, s:Scalar<'t>) =
        let l = subst_var_value v.Var s.Expr lhs.Expr |> expand_as<'t> |> simplifye |> Scalar<'t> in
        let r = subst_var_value v.Var s.Expr rhs.Expr |> expand_as<'t> |> simplifye |> Scalar<'t> in
        ScalarEquation(l, r)
   
and ScalarVarMap<'t when 't: equality and 't :> ValueType and 't :> IEquatable<'t>>(var:ScalarVar<'t>, expr:Scalar<'t>) =
    inherit ScalarEquation<'t>(var, expr)
    member val Var = var
    
and ScalarVarRelation<'t when 't: equality and 't :> ValueType and 't :> IEquatable<'t>>(var:ScalarVar<'t>, expr:Scalar<'t>, op:Expr<'t->'t->bool>) =
    inherit ScalarRelation<'t>(var, expr, op)
    member val Var = var
    
and Prop (expr:Expr<bool>) =
    inherit Term<bool>(expr)
    
    static member (!!) (l:#Prop) = Prop <@ not %l.Expr @>

    static member (*) (l:Prop, r:Prop) = Prop <@ %l.Expr |&| %r.Expr @>
    
    static member (+) (l:Prop, r:Prop) = Prop <@ %l.Expr ||| %r.Expr @>

    static member (==) (l:Prop, r:Prop) = Prop <@ %l.Expr = %r.Expr @>

    static member (!=) (l:Prop, r:Prop) = Prop <@ %l.Expr <> %r.Expr @>

    static member (==>) (l:Prop, r:Prop) = Prop <@ %l.Expr ===> %r.Expr @>

    static member (<==) (l:Prop, r:Prop) = Prop <@ %r.Expr <=== %l.Expr @>

    override x.Display = src expr

    static member op_Implicit(l:Prop) : Expr<bool> = l.Expr

    static member op_Implicit (l:Expr<bool>):Prop = Prop l

    static member op_Explicit(l:Prop) : Expr<bool> = l.Expr

and PropVar(n: string) =
    inherit Prop(Expr.Var(Var(n, typeof<bool>)) |> expand_as<bool>)
    
and PropConst(n: string, ?v:bool) = 
    inherit Prop(Expr.ValueWithName(v, n) |> expand_as<bool>)
    member val Name = n
    member val Val = defaultArg v false

and TermHistory =
| UnaryOp of string * obj
| BinaryOp of string * obj * obj

and IHistory = 
    abstract member History:TermHistory option

[<RequireQualifiedAccess>]
module NumericLiteralR = 
  let FromZero() = 0.0 |> real |> exprv |> Scalar
  let FromOne() = 1.0 |> real |> exprv |> Scalar
  let FromInt32 n = n |> real |> exprv |> Scalar
  let FromInt64 n = n |> real |> exprv |> Scalar
  
type realexpr = Scalar<real>

type ratexpr = Scalar<rat>

type intexpr = Scalar<int>

type natexpr = Scalar<nat>

type complexexpr = Scalar<complex>

type realvar = ScalarVar<real>

type ratvar = ScalarVar<rat>

type intvar = ScalarVar<int>

type natvar = ScalarVar<nat>

type complexvar = ScalarVar<complex>

type boolvar = PropVar

type indexvar = IndexVar

type realconst = ScalarConst<real>

type ratconst = ScalarConst<rat>

type intconst = ScalarConst<int>

type natconst = ScalarConst<nat>

type complexconst = ScalarConst<complex>

[<AutoOpen>]
module Scalar =
    let (|ScalarVar|_|) : obj -> ScalarVar<_> option =
        function
        | :? ScalarVar<_> as v -> Some v
        | _ -> None
    
    let intexpr x = 
        match box x with
        | :? Scalar<int> as s -> s.Expr
        | :? Expr<int> as e -> e
        | :? int as n -> Expr.Value n |> expand_as<int>
        | :? real as n -> Expr.Value ((int) n) |> expand_as<int>
        | _ -> failwithf "The expression %A is not an integer expression." x

    let realexpr x = 
        match box x with
        | :? Scalar<real> as s -> s.Expr
        | :? Expr<real> as e -> e
        | :? real as n -> Expr.Value n |> expand_as<real>
        | :? int as n -> Expr.Value (real n) |> expand_as<real>
        | _ -> failwithf "The expression %A is not a real number expression." x

    let ratexpr x = 
        match box x with
        | :? Scalar<rat> as s -> s.Expr
        | :? Expr<rat> as e -> e
        | :? rat as n -> Expr.Value n |> expand_as<rat>
        | :? int as n -> Expr.Value (rat n) |> expand_as<rat>
        | _ -> failwithf "The expression %A is not a rational number expression." x

    let sterm<'t when 't : equality and 't :> ValueType and 't :> IEquatable<'t>> (t:obj) =
        match box t with
        | :? 't as v -> v |> exprv |> Scalar
        | v when v.GetType().IsValueType -> Convert.ChangeType(v, typeof<'t>) :?> 't |> exprv |> Scalar
        | :? Scalar<'t> as t -> t
        | :? Expr<'t> as e -> e |> Scalar
        | x -> failwithf "Cannot convert %A of type %A to Scalar of type %A." x (x.GetType()) (typeof<'t>)
           
    let scalar_terms<'t when 't : equality and 't :> ValueType and 't :> IEquatable<'t>> (t:obj[]) =
        t |> Array.map(
            function
            | :? 't as v -> v |> exprv |> Scalar
            | :? Scalar<'t> as t -> t
            | :? Expr<'t> as e -> e |> Scalar
            | x -> failwithf "Cannot convert %A of type %A to Scalar of type %A." x (x.GetType()) (typeof<'t>)
        )

    let realterm:obj->Scalar<real> =
        function
        | :? real as v -> v |> exprv |> Scalar
        | :? int as v -> v |> real |> exprv |> Scalar
        | :? rat as v -> v |> real |> exprv |> Scalar
        | :? nat as v -> v |> real |> exprv |> Scalar
        | :? Scalar<real> as t -> t
        | :? Expr<real> as e -> e |> Scalar
        | x -> failwithf "Cannot convert %A of type %A to real Scalar." x (x.GetType())

    let realterms (t:seq<_>) = t |> Seq.toArray |> Array.map realterm
           
    let fail_if_not_var(t:Scalar<_>) =
        match t.Expr with
        | Var _ -> ()
        | _ -> failwithf "The term %A is not a variable." t

    let scalarvar<'t when 't : equality and 't: comparison and 't :> ValueType and 't :> IEquatable<'t>> (n:string) = ScalarVar<'t> n

    let (!%) (q:Scalar<'t>) = q |> sexpr |> ev 

    let realvar2 (p:string) (q:string) = realvar p, realvar q

    let realvar3 (p:string) (q:string) (r:string) = realvar p, realvar q, realvar r

    let realvar4 (p:string) (q:string) (r:string) (s:string) = realvar p, realvar q, realvar r, realvar s

    let realconst2 (p:string) (q:string) = realconst p, realconst q

    let realconst3 (p:string) (q:string) (r:string) = realconst p, realconst q, realconst r

    let realconst4 (p:string) (q:string) (r:string) (s:string) = realconst p, realconst q, realconst r, realconst s

    let const_as_var (c:ScalarConst<'a>) = ScalarVar<'a>(c.Name)

    let var_name (v:ScalarVar<_>) = v.Name

    let const_name (c:ScalarConst<_>) = c.Name
    
    let inline get_scalar_vars (x : ^T) = (^T : (member Expr : Expr<'t>) (x)) |> get_vars |> List.map (exprvar<'t> >> ScalarVar<'t>)
    
    let inline get_real_vars (x : ^T) = (^T : (member Expr : Expr<real>) (x)) |> get_vars |> List.map (exprvar<real> >> realvar)

    let inline get_real_consts (x : ^T) = (^T : (member Expr : Expr<real>) (x)) |> get_consts |> List.map (snd >> realconst)

    let scalar_eqn<'t when 't : equality and 't: comparison and 't :> ValueType and 't :> IEquatable<'t>> (e:Expr<bool>) =
        match e with
        | SpecificCall <@@ (=) @@> (_,_,l::r::[]) -> ScalarEquation<'t> ((l |> expand_as<'t> |> Scalar<'t>), (l |> expand_as<'t> |> Scalar<'t>))
        | _ -> failwithf "The expression %s is not an equation" (src e)
    
    let scalar_varmap<'t when 't : equality and 't: comparison and 't :> ValueType and 't :> IEquatable<'t>> (e:Expr<bool>) =
        match e with
        | SpecificCall <@@ (=) @@> (_,_,Var v::r::[]) when v.Type = typeof<'t> -> ScalarVarMap<'t> (v |> exprvar |> ScalarVar<'t>, (r |> expand_as<'t> |> Scalar<'t>))
        | _ -> failwithf "The expression %s is not an equation" (src e)
    
    let fix_eqn (o:'a) (e:ScalarEquation<'t>) = e.Fix o

    let lhs (s:#ScalarRelation<'t>) = s.Lhs

    let rhs (s:#ScalarRelation<'t>) = s.Rhs

    let fixvar (c:seq<ScalarVar<'b>>) (s:'s when 's :> #ISymbolic<'s,'b>) = fixconst (c |> Seq.map(fun _c -> _c.Name)) s

    let collect_linear_terms (e:realexpr) = ()
    
    let zero<'t when 't : equality and 't :> ValueType and 't :> IEquatable<'t>> = Scalar<'t>.Zero

    let one<'t when 't : equality and 't :> ValueType and 't :> IEquatable<'t>> = Scalar<'t>.One

    let negone<'t when 't : equality and 't :> ValueType and 't :> IEquatable<'t>> = Scalar<'t>.NegOne

    let sconst<'t when 't : equality and 't :> ValueType and 't :> IEquatable<'t>> (n:obj) = Convert.ChangeType(n, typeof<'t>) :?> 't |> exprv |> Scalar

    let inf = realterm Double.MaxValue

    let neginf = realterm Double.MinValue

    let as_var_map(eqn:ScalarEquation<_>) =
        match eqn.Lhs.Expr with
        | Var v -> ScalarVarMap(v |> exprvar |> ScalarVar, eqn.Rhs)
        | _ -> failwithf "The lhs of %A is not a variable expression." eqn.Expr

    let (|Division|_|) (s:Scalar<'t>) =
        match s.Expr with
        | FsExpr.Division(l,r) -> Some(l |> expand_as<'t> |> Scalar<'t>, r |> expand_as<'t> |> Scalar<'t>)
        | _ -> None

    let (|Zero|PositiveInfinity|NegativeInfinity|Finite|) (s:Scalar<real>) = 
        if s = zero then Zero s else if s = inf then PositiveInfinity else if s = neginf then NegativeInfinity s else Finite s

[<AutoOpen>]
module Prop =
    let prop e = Prop e

    let T = Prop <@ true @>

    let F = Prop <@ false @>

    let boolvar2 p q = boolvar p, boolvar q

    let boolvar3 p q r = boolvar p, boolvar q, boolvar r

    let boolvar4 p q r s = boolvar p, boolvar q, boolvar r, boolvar s
  