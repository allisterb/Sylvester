namespace Sylvester

open System

open FSharp.Quotations
open FSharp.Quotations.Patterns

[<AbstractClass; StructuredFormatDisplay("{Display}")>]
type Term<'t when 't: equality> (expr:Expr<'t>) =
    member val Expr = expr

    member val Val = match expr with | Patterns.Value(v, _) -> v :?> 't |> Some | _ -> None
    
    abstract member Display: string
    
    interface IEquatable<Term<'t>> with
        member a.Equals b = a.Display = b.Display

    override a.Equals (_b:obj) = 
            match _b with 
            | :? Term<'t> as b -> (a :> IEquatable<Term<'t>>).Equals b
            | _ -> false
    
    override a.GetHashCode() = a.Expr.GetHashCode()

    override a.ToString() = a.Display

    static member op_Implicit (l:Term<'t>):Expr<'t> = l.Expr

and [<AbstractClass>] TermVar<'t when 't: equality>(n: string) = 
    inherit Term<'t>(Expr.Var(Var(n, typeof<'t>)) |> expand_as<'t>)
    override x.Display = sprintf "{%A}" n
    member x.Name = n
    member x.Var = match x.Expr with | Var v -> v | _ -> failwith ""
    //member x.Item(i:IndexVar) = ScalarIndexedVar(x, i)
    //member x.Item(i:int) = ScalarTermVar(x.Name + i.ToString())

and IndexVar(n: string) = inherit TermVar<int>(n)

type ScalarTerm<'t when 't: equality> (expr:Expr<'t>) =
    inherit Term<'t>(expr)
    
    override x.Display = sprinte expr

    override a.Equals (_b:obj) = 
        match _b with 
        | :? ScalarTerm<'t> as b -> (a :> IEquatable<Term<'t>>).Equals b
        | _ -> false
    
    override a.GetHashCode() = a.Expr.GetHashCode()

    interface IComparable<ScalarTerm<'t>> with
       member a.CompareTo b = match (a.Val, b.Val) with | (Some av, Some bv) -> 0 | _, _ -> failwith ""
    
    interface IComparable with
        member a.CompareTo b = 
            match b with
            | :? ScalarTerm<'t> as bs -> (a :> IComparable<ScalarTerm<'t>>).CompareTo bs
            | _ -> failwith "This object is not a term."
    
    interface ISymbolic<ScalarTerm<'t>, 't> with
           member a.Expr = expr
           member a.Mutate(e:Expr<'t>) = ScalarTerm e
           
    static member Zero = typeof<'t> |> zero_val |> expand_as<'t> |> ScalarTerm<'t>

    static member One = typeof<'t> |> one_val |> expand_as<'t> |> ScalarTerm<'t>

    static member op_Implicit (l:ScalarTerm<'t>):Expr<'t> = l.Expr

    static member op_Implicit (l:'t):ScalarTerm<'t> = ScalarTerm (exprv l)

    static member op_Implicit (l:int):ScalarTerm<real> = let v = real l in ScalarTerm (exprv v)

    static member op_Implicit (l:rat):ScalarTerm<real> = let v = real l in ScalarTerm (exprv v)

    static member op_Implicit (l:nat):ScalarTerm<real> = let v = real l in ScalarTerm (exprv v)

    (* Binary operators *)

    static member (+) (l:ScalarTerm<'t>, r:ScalarTerm<'t>) = call_add (l.Expr) (r.Expr) |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (+) (l:ScalarTerm<'t>, r:'t) = call_add (l.Expr) (Expr.Value r) |> expand_as<'t> |> ScalarTerm<'t>

    static member (+) (l:'t, r:ScalarTerm<'t>) = call_add (Expr.Value l) r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (+) (l:Expr<'t>, r:ScalarTerm<'t>) = call_add l r.Expr |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (+) (l:ScalarTerm<'t>, r:Expr<'t>) = call_add (l.Expr) r |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (+) (l:ScalarTerm<real>, r:int) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand_as<'t> |> ScalarTerm<'t>

    static member (+) (l:ScalarTerm<real>, r:rat) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (+) (l:ScalarTerm<real>, r:nat) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand_as<'t> |> ScalarTerm<'t>

    static member (+) (l:int, r:ScalarTerm<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (+) (l:rat, r:ScalarTerm<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (+) (l:nat, r:ScalarTerm<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (+) (l:ScalarTerm<rat>, r:int) = call_add (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> ScalarTerm<'t>

    static member (+) (l:int, r:ScalarTerm<rat>) = call_add (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (+) (l:ScalarTerm<int>, r:nat) = call_add (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> ScalarTerm<'t>

    
    static member (-) (l:ScalarTerm<'t>, r:ScalarTerm<'t>) = call_sub (l.Expr) (r.Expr) |> expand_as<'t> |> ScalarTerm<'t>

    static member (-) (l:ScalarTerm<'t>, r:'t) = call_sub (l.Expr) (Expr.Value r) |> expand_as<'t> |> ScalarTerm<'t>

    static member (-) (l:'t, r:ScalarTerm<'t>) = call_sub (Expr.Value l) r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (-) (l:Expr<'t>, r:ScalarTerm<'t>) = call_sub l r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (-) (l:ScalarTerm<'t>, r:Expr<'t>) = call_sub (l.Expr) r |> expand_as<'t> |> ScalarTerm<'t>

    static member (-) (l:ScalarTerm<real>, r:int) = let r' = (real) r in call_sub (l.Expr) (Expr.Value r') |> expand_as<'t> |> ScalarTerm<'t>

    static member (-) (l:ScalarTerm<real>, r:rat) = let r' = (real) r in call_sub (l.Expr) (Expr.Value r') |> expand_as<'t> |> ScalarTerm<'t>

    static member (-) (l:ScalarTerm<real>, r:nat) = let r' = (real) r in call_sub (l.Expr) (Expr.Value r') |> expand_as<'t> |> ScalarTerm<'t>

    static member (-) (l:int, r:ScalarTerm<real>) = let l' = (real) l in call_sub (Expr.Value l') r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (-) (l:rat, r:ScalarTerm<real>) = let l' = (real) l in call_sub (Expr.Value l') r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (-) (l:nat, r:ScalarTerm<real>) = let l' = (real) l in call_sub (Expr.Value l') r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (-) (l:ScalarTerm<rat>, r:int) = call_sub (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> ScalarTerm<'t>

    static member (-) (l:int, r:ScalarTerm<rat>) = call_sub (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (-) (l:ScalarTerm<int>, r:nat) = call_sub (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> ScalarTerm<'t>


    static member (*) (l:ScalarTerm<'t>, r:ScalarTerm<'t>) = call_add (l.Expr) (r.Expr) |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (*) (l:ScalarTerm<'t>, r:'t) = call_add (l.Expr) (Expr.Value r) |> expand_as<'t> |> ScalarTerm<'t>

    static member (*) (l:'t, r:ScalarTerm<'t>) = call_add (Expr.Value l) r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (*) (l:Expr<'t>, r:ScalarTerm<'t>) = call_mul l r.Expr |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (*) (l:ScalarTerm<'t>, r:Expr<'t>) = call_mul (l.Expr) r |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (*) (l:ScalarTerm<real>, r:int) = let r' = (real) r in call_mul (l.Expr) (Expr.Value r') |> expand_as<'t> |> ScalarTerm<'t>

    static member (*) (l:ScalarTerm<real>, r:rat) = let r' = (real) r in call_mul (l.Expr) (Expr.Value r') |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (*) (l:ScalarTerm<real>, r:nat) = let r' = (real) r in call_mul (l.Expr) (Expr.Value r') |> expand_as<'t> |> ScalarTerm<'t>

    static member (*) (l:int, r:ScalarTerm<real>) = let l' = (real) l in call_mul (Expr.Value l') r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (*) (l:rat, r:ScalarTerm<real>) = let l' = (real) l in call_mul (Expr.Value l') r.Expr |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (*) (l:nat, r:ScalarTerm<real>) = let l' = (real) l in call_mul (Expr.Value l') r.Expr |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (*) (l:ScalarTerm<rat>, r:int) = call_mul (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> ScalarTerm<'t>

    static member (*) (l:int, r:ScalarTerm<rat>) = call_mul (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (*) (l:ScalarTerm<int>, r:nat) = call_mul (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> ScalarTerm<'t>


    static member (/) (l:ScalarTerm<'t>, r:ScalarTerm<'t>) = call_div (l.Expr) (r.Expr) |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (/) (l:ScalarTerm<'t>, r:'t) = call_div (l.Expr) (Expr.Value r) |> expand_as<'t> |> ScalarTerm<'t>

    static member (/) (l:'t, r:ScalarTerm<'t>) = call_div (Expr.Value l) r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (/) (l:Expr<'t>, r:ScalarTerm<'t>) = call_div l r.Expr |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (/) (l:ScalarTerm<'t>, r:Expr<'t>) = call_div (l.Expr) r |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (/) (l:ScalarTerm<real>, r:int) = let r' = (real) r in call_div (l.Expr) (Expr.Value r') |> expand_as<'t> |> ScalarTerm<'t>

    static member (/) (l:ScalarTerm<real>, r:rat) = let r' = (real) r in call_div (l.Expr) (Expr.Value r') |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (/) (l:ScalarTerm<real>, r:nat) = let r' = (real) r in call_div (l.Expr) (Expr.Value r') |> expand_as<'t> |> ScalarTerm<'t>

    static member (/) (l:int, r:ScalarTerm<real>) = let l' = (real) l in call_div (Expr.Value l') r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (/) (l:rat, r:ScalarTerm<real>) = let l' = (real) l in call_div (Expr.Value l') r.Expr |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (/) (l:nat, r:ScalarTerm<real>) = let l' = (real) l in call_div (Expr.Value l') r.Expr |> expand_as<'t> |> ScalarTerm<'t>
    
    static member (/) (l:ScalarTerm<rat>, r:int) = call_div (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> ScalarTerm<'t>

    static member (/) (l:int, r:ScalarTerm<rat>) = call_div (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member (/) (l:ScalarTerm<int>, r:nat) = call_div (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> ScalarTerm<'t>


    static member Pow (l : ScalarTerm<real>, r : ScalarTerm<real>) = call_pow l.Expr r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member Pow (l : ScalarTerm<real>, r : real) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> ScalarTerm<'t>
    
    static member Pow (l : ScalarTerm<real>, r : rat) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> ScalarTerm<'t>

    static member Pow (l : ScalarTerm<real>, r : nat) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> ScalarTerm<'t>

    static member Pow (l : ScalarTerm<real>, r : int) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> ScalarTerm<'t>

    static member Pow (l : ScalarTerm<rat>, r : real) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> ScalarTerm<'t>

    static member Pow (l : ScalarTerm<rat>, r : rat) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> ScalarTerm<'t>

    static member Pow (l : ScalarTerm<rat>, r : int) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> ScalarTerm<'t>
         
    static member Pow (l : ScalarTerm<int>, r : int) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> ScalarTerm<'t>
         
    static member Pow (l : ScalarTerm<nat>, r : rat) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> ScalarTerm<'t>
    
    static member Pow (l : real, r : ScalarTerm<real>) = call_pow (Expr.Value l) r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member Pow (l : int, r : ScalarTerm<real>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member Pow (l : rat, r : ScalarTerm<real>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member Pow (l : nat, r : ScalarTerm<real>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> ScalarTerm<'t>

    static member Pow (l : int, r : ScalarTerm<int>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> ScalarTerm<'t>
    
    static member Pow (l : rat, r : ScalarTerm<rat>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> ScalarTerm<'t>
    
    static member Pow (l : nat, r : ScalarTerm<nat>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> ScalarTerm<'t>

and ScalarTermVar<'t when 't: equality>(n: string) = 
    inherit Term<'t>(Expr.Var(Var(n, typeof<'t>)) |> expand_as<'t>)
    override x.Display = sprintf "{%A}" n
    member x.Name = n
    member x.Var = match x.Expr with | Var v -> v | _ -> failwith ""
    member x.Item(i:ScalarIndexVar) = ScalarIndexedVar(x, i)
    member x.Item(i:int) = ScalarTermVar(x.Name + i.ToString())

and ScalarIndexVar(n: string) = inherit ScalarTermVar<int>(n)

and ScalarIndexedVar<'t when 't: equality>(var:ScalarTermVar<'t>, index:ScalarIndexVar) =
    inherit ScalarTermVar<'t>(var.Name + "_" + index.Name)
    member x.Item(i:int) = ScalarTermVar(x.Name.Replace("_" + index.Name, i.ToString()))
type realexpr = ScalarTerm<real>

type ratexpr = ScalarTerm<rat>

type intexpr = ScalarTerm<int>

type natexpr = ScalarTerm<nat>

[<RequireQualifiedAccess>]
module NumericLiteralR = 
  let FromZero() = ScalarTerm <@ 0.0 @>
  let FromOne() = ScalarTerm <@ 1.0 @>
  let FromInt32 n = n |> real |> exprv |> ScalarTerm
  let FromInt64 n = n |> real |> exprv |> ScalarTerm
  
[<AutoOpen>]
module ScalarTerm =
    let int_expr x = 
        match box x with
        | :? ScalarTerm<int> as s -> s.Expr
        | :? Expr<int> as e -> e
        | :? int as n -> Expr.Value n |> expand_as<int>
        | :? real as n -> Expr.Value ((int) n) |> expand_as<int>
        | _ -> failwithf "The expression %A is not an integer expression." x

    let real_expr x = 
        match box x with
        | :? ScalarTerm<real> as s -> s.Expr
        | :? Expr<real> as e -> e
        | :? real as n -> Expr.Value n |> expand_as<real>
        | :? int as n -> Expr.Value (real n) |> expand_as<real>
        | _ -> failwithf "The expression %A is not a real number expression." x

    let rat_expr x = 
        match box x with
        | :? ScalarTerm<rat> as s -> s.Expr
        | :? Expr<rat> as e -> e
        | :? rat as n -> Expr.Value n |> expand_as<rat>
        | :? int as n -> Expr.Value (rat n) |> expand_as<rat>
        | _ -> failwithf "The expression %A is not a rational number expression." x

    let var6<'t when 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IFormattable> name = 
        Expr.Var(Var(name, typeof<'t>)) |> expand_as<'t> |> ScalarTerm<'t>

    let scalar_terms<'t when 't : equality> (t:obj[]) =
        let m:obj->ScalarTerm<'t> = 
            function
            | :? 't as v -> v |> exprv |> ScalarTerm
            | :? ScalarTerm<'t> as t -> t
            | :? Expr<'t> as e -> e |> ScalarTerm
            | x -> failwithf "Cannot convert %A of type %A to ScalarTerm of type %A." x (x.GetType()) (typeof<'t>)
        t |> Array.map m

    let realterms (t:obj[]) =
        let m:obj->ScalarTerm<real> = 
            function
            | :? real as v -> v |> exprv |> ScalarTerm
            | :? int as v -> v |> real |> exprv |> ScalarTerm
            | :? rat as v -> v |> real |> exprv |> ScalarTerm
            | :? nat as v -> v |> real |> exprv |> ScalarTerm
            | :? ScalarTerm<real> as t -> t
            | :? Expr<real> as e -> e |> ScalarTerm
            | x -> failwithf "Cannot convert %A of type %A to real ScalarTerm." x (x.GetType())
        t |> Array.map m

    let fail_if_not_var(t:ScalarTerm<_>) =
        match t.Expr with
        | Var _ -> ()
        | _ -> failwithf "The term %A is not a variable." t

    //let index_var<'t> n = ScalarIndexVar(n)


  