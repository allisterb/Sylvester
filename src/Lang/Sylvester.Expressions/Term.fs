namespace Sylvester

open System

open FSharp.Quotations
open FSharp.Quotations.Patterns

[<StructuredFormatDisplay("{Display}")>]
type Term<'t when 't: equality> (expr:Expr<'t>) =
    member val Expr = expr
    //member val MathNetExpr = mnexpr
    member val Val = match expr with | Patterns.Value(v, _) -> v :?> 't |> Some | _ -> None
    
    member val Display = sprinte expr
    
    interface IEquatable<Term<'t>> with
        member a.Equals b = a.Display = b.Display

    interface IComparable<Term<'t>> with
       member a.CompareTo b = match (a.Val, b.Val) with | (Some av, Some bv) -> 0 | _, _ -> failwith ""
    
    interface IComparable with
        member a.CompareTo b = 
            match b with
            | :? Term<'t> as bs -> (a :> IComparable<Term<'t>>).CompareTo bs
            | _ -> failwith "This object is not a term."
    
    interface ISymbolic<Term<'t>, 't> with
        member a.Expr = expr
        member a.Mutate(e:Expr<'t>) = Term(e)
        
    override a.Equals (_b:obj) = 
            match _b with 
            | :? Term<'t> as b -> (a :> IEquatable<Term<'t>>).Equals b
            | _ -> false
    
    override a.GetHashCode() = a.Expr.GetHashCode()

    override a.ToString() = a.Display

    static member Zero = typeof<'t> |> zero_val |> expand_as<'t> |> Term<'t>

    static member One = typeof<'t> |> one_val |> expand_as<'t> |> Term<'t>

    static member op_Implicit (l:Term<'t>):Expr<'t> = l.Expr

    static member op_Implicit (l:'t):Term<'t> = Term (exprv l)

    static member op_Implicit (l:int):Term<real> = let v = real l in Term (exprv v)

    static member op_Implicit (l:rat):Term<real> = let v = real l in Term (exprv v)

    static member op_Implicit (l:nat):Term<real> = let v = real l in Term (exprv v)

    (* Binary operators *)

    static member (+) (l:Term<'t>, r:Term<'t>) = call_add (l.Expr) (r.Expr) |> expand_as<'t> |> Term<'t>
    
    static member (+) (l:Term<'t>, r:'t) = call_add (l.Expr) (Expr.Value r) |> expand_as<'t> |> Term<'t>

    static member (+) (l:'t, r:Term<'t>) = call_add (Expr.Value l) r.Expr |> expand_as<'t> |> Term<'t>

    static member (+) (l:Expr<'t>, r:Term<'t>) = call_add l r.Expr |> expand_as<'t> |> Term<'t>
    
    static member (+) (l:Term<'t>, r:Expr<'t>) = call_add (l.Expr) r |> expand_as<'t> |> Term<'t>
    
    static member (+) (l:Term<real>, r:int) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand_as<'t> |> Term<'t>

    static member (+) (l:Term<real>, r:rat) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand_as<'t> |> Term<'t>
    
    static member (+) (l:Term<real>, r:nat) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand_as<'t> |> Term<'t>

    static member (+) (l:int, r:Term<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand_as<'t> |> Term<'t>

    static member (+) (l:rat, r:Term<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand_as<'t> |> Term<'t>
    
    static member (+) (l:nat, r:Term<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand_as<'t> |> Term<'t>
    
    static member (+) (l:Term<rat>, r:int) = call_add (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> Term<'t>

    static member (+) (l:int, r:Term<rat>) = call_add (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> Term<'t>

    static member (+) (l:Term<int>, r:nat) = call_add (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> Term<'t>

    
    static member (-) (l:Term<'t>, r:Term<'t>) = call_sub (l.Expr) (r.Expr) |> expand_as<'t> |> Term<'t>

    static member (-) (l:Term<'t>, r:'t) = call_sub (l.Expr) (Expr.Value r) |> expand_as<'t> |> Term<'t>

    static member (-) (l:'t, r:Term<'t>) = call_sub (Expr.Value l) r.Expr |> expand_as<'t> |> Term<'t>

    static member (-) (l:Expr<'t>, r:Term<'t>) = call_sub l r.Expr |> expand_as<'t> |> Term<'t>

    static member (-) (l:Term<'t>, r:Expr<'t>) = call_sub (l.Expr) r |> expand_as<'t> |> Term<'t>

    static member (-) (l:Term<real>, r:int) = let r' = (real) r in call_sub (l.Expr) (Expr.Value r') |> expand_as<'t> |> Term<'t>

    static member (-) (l:Term<real>, r:rat) = let r' = (real) r in call_sub (l.Expr) (Expr.Value r') |> expand_as<'t> |> Term<'t>

    static member (-) (l:Term<real>, r:nat) = let r' = (real) r in call_sub (l.Expr) (Expr.Value r') |> expand_as<'t> |> Term<'t>

    static member (-) (l:int, r:Term<real>) = let l' = (real) l in call_sub (Expr.Value l') r.Expr |> expand_as<'t> |> Term<'t>

    static member (-) (l:rat, r:Term<real>) = let l' = (real) l in call_sub (Expr.Value l') r.Expr |> expand_as<'t> |> Term<'t>

    static member (-) (l:nat, r:Term<real>) = let l' = (real) l in call_sub (Expr.Value l') r.Expr |> expand_as<'t> |> Term<'t>

    static member (-) (l:Term<rat>, r:int) = call_sub (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> Term<'t>

    static member (-) (l:int, r:Term<rat>) = call_sub (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> Term<'t>

    static member (-) (l:Term<int>, r:nat) = call_sub (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> Term<'t>


    static member (*) (l:Term<'t>, r:Term<'t>) = call_add (l.Expr) (r.Expr) |> expand_as<'t> |> Term<'t>
    
    static member (*) (l:Term<'t>, r:'t) = call_add (l.Expr) (Expr.Value r) |> expand_as<'t> |> Term<'t>

    static member (*) (l:'t, r:Term<'t>) = call_add (Expr.Value l) r.Expr |> expand_as<'t> |> Term<'t>

    static member (*) (l:Expr<'t>, r:Term<'t>) = call_mul l r.Expr |> expand_as<'t> |> Term<'t>
    
    static member (*) (l:Term<'t>, r:Expr<'t>) = call_mul (l.Expr) r |> expand_as<'t> |> Term<'t>
    
    static member (*) (l:Term<real>, r:int) = let r' = (real) r in call_mul (l.Expr) (Expr.Value r') |> expand_as<'t> |> Term<'t>

    static member (*) (l:Term<real>, r:rat) = let r' = (real) r in call_mul (l.Expr) (Expr.Value r') |> expand_as<'t> |> Term<'t>
    
    static member (*) (l:Term<real>, r:nat) = let r' = (real) r in call_mul (l.Expr) (Expr.Value r') |> expand_as<'t> |> Term<'t>

    static member (*) (l:int, r:Term<real>) = let l' = (real) l in call_mul (Expr.Value l') r.Expr |> expand_as<'t> |> Term<'t>

    static member (*) (l:rat, r:Term<real>) = let l' = (real) l in call_mul (Expr.Value l') r.Expr |> expand_as<'t> |> Term<'t>
    
    static member (*) (l:nat, r:Term<real>) = let l' = (real) l in call_mul (Expr.Value l') r.Expr |> expand_as<'t> |> Term<'t>
    
    static member (*) (l:Term<rat>, r:int) = call_mul (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> Term<'t>

    static member (*) (l:int, r:Term<rat>) = call_mul (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> Term<'t>

    static member (*) (l:Term<int>, r:nat) = call_mul (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> Term<'t>


    static member (/) (l:Term<'t>, r:Term<'t>) = call_div (l.Expr) (r.Expr) |> expand_as<'t> |> Term<'t>
    
    static member (/) (l:Term<'t>, r:'t) = call_div (l.Expr) (Expr.Value r) |> expand_as<'t> |> Term<'t>

    static member (/) (l:'t, r:Term<'t>) = call_div (Expr.Value l) r.Expr |> expand_as<'t> |> Term<'t>

    static member (/) (l:Expr<'t>, r:Term<'t>) = call_div l r.Expr |> expand_as<'t> |> Term<'t>
    
    static member (/) (l:Term<'t>, r:Expr<'t>) = call_div (l.Expr) r |> expand_as<'t> |> Term<'t>
    
    static member (/) (l:Term<real>, r:int) = let r' = (real) r in call_div (l.Expr) (Expr.Value r') |> expand_as<'t> |> Term<'t>

    static member (/) (l:Term<real>, r:rat) = let r' = (real) r in call_div (l.Expr) (Expr.Value r') |> expand_as<'t> |> Term<'t>
    
    static member (/) (l:Term<real>, r:nat) = let r' = (real) r in call_div (l.Expr) (Expr.Value r') |> expand_as<'t> |> Term<'t>

    static member (/) (l:int, r:Term<real>) = let l' = (real) l in call_div (Expr.Value l') r.Expr |> expand_as<'t> |> Term<'t>

    static member (/) (l:rat, r:Term<real>) = let l' = (real) l in call_div (Expr.Value l') r.Expr |> expand_as<'t> |> Term<'t>
    
    static member (/) (l:nat, r:Term<real>) = let l' = (real) l in call_div (Expr.Value l') r.Expr |> expand_as<'t> |> Term<'t>
    
    static member (/) (l:Term<rat>, r:int) = call_div (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> Term<'t>

    static member (/) (l:int, r:Term<rat>) = call_div (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> Term<'t>

    static member (/) (l:Term<int>, r:nat) = call_div (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> Term<'t>


    static member Pow (l : Term<real>, r : Term<real>) = call_pow l.Expr r.Expr |> expand_as<'t> |> Term<'t>

    static member Pow (l : Term<real>, r : real) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> Term<'t>
    
    static member Pow (l : Term<real>, r : rat) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> Term<'t>

    static member Pow (l : Term<real>, r : nat) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> Term<'t>

    static member Pow (l : Term<real>, r : int) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> Term<'t>

    static member Pow (l : Term<rat>, r : real) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> Term<'t>

    static member Pow (l : Term<rat>, r : rat) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> Term<'t>

    static member Pow (l : Term<rat>, r : int) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> Term<'t>
         
    static member Pow (l : Term<int>, r : int) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> Term<'t>
         
    static member Pow (l : Term<nat>, r : rat) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> Term<'t>
    
    static member Pow (l : real, r : Term<real>) = call_pow (Expr.Value l) r.Expr |> expand_as<'t> |> Term<'t>

    static member Pow (l : int, r : Term<real>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> Term<'t>

    static member Pow (l : rat, r : Term<real>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> Term<'t>

    static member Pow (l : nat, r : Term<real>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> Term<'t>

    static member Pow (l : int, r : Term<int>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> Term<'t>
    
    static member Pow (l : rat, r : Term<rat>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> Term<'t>
    
    static member Pow (l : nat, r : Term<nat>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> Term<'t>

type realexpr = Term<real>

type ratexpr = Term<rat>

type intexpr = Term<int>

type natexpr = Term<nat>

//type VarTerm<'t when 't: equality>(n: string) = inherit Term<'t>(Expr.Var(Var(n, typeof<'t>)) |> expand_as<'t>)

[<RequireQualifiedAccess>]
module NumericLiteralR = 
  let FromZero() = Term <@ 0.0 @>
  let FromOne() = Term <@ 1.0 @>
  let FromInt32 n = n |> real |> exprv |> Term
  let FromInt64 n = n |> real |> exprv |> Term
  
[<AutoOpen>]
module Term =
    let int_expr x = 
        match box x with
        | :? Term<int> as s -> s.Expr
        | :? Expr<int> as e -> e
        | :? int as n -> Expr.Value n |> expand_as<int>
        | :? real as n -> Expr.Value ((int) n) |> expand_as<int>
        | _ -> failwithf "The expression %A is not an integer expression." x

    let real_expr x = 
        match box x with
        | :? Term<real> as s -> s.Expr
        | :? Expr<real> as e -> e
        | :? real as n -> Expr.Value n |> expand_as<real>
        | :? int as n -> Expr.Value (real n) |> expand_as<real>
        | _ -> failwithf "The expression %A is not a real number expression." x

    let rat_expr x = 
        match box x with
        | :? Term<rat> as s -> s.Expr
        | :? Expr<rat> as e -> e
        | :? rat as n -> Expr.Value n |> expand_as<rat>
        | :? int as n -> Expr.Value (rat n) |> expand_as<rat>
        | _ -> failwithf "The expression %A is not a rational number expression." x

    let var6<'t when 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IFormattable> name = 
        Expr.Var(Var(name, typeof<'t>)) |> expand_as<'t> |> Term<'t>

    let terms<'t when 't : equality> (t:obj[]) =
        let m:obj->Term<'t> = 
            function
            | :? 't as v -> v |> exprv |> Term
            | :? Term<'t> as t -> t
            | :? Expr<'t> as e -> e |> Term
            | x -> failwithf "Cannot convert %A of type %A to Term of type %A." x (x.GetType()) (typeof<'t>)
        t |> Array.map m

    let realterms (t:obj[]) =
        let m:obj->Term<real> = 
            function
            | :? real as v -> v |> exprv |> Term
            | :? int as v -> v |> real |> exprv |> Term
            | :? rat as v -> v |> real |> exprv |> Term
            | :? nat as v -> v |> real |> exprv |> Term
            | :? Term<real> as t -> t
            | :? Expr<real> as e -> e |> Term
            | x -> failwithf "Cannot convert %A of type %A to real Term." x (x.GetType())
        t |> Array.map m

    let fail_if_not_var(t:Term<_>) =
        match t.Expr with
        | Var _ -> ()
        | _ -> failwithf "The term %A is not a variable." t

  