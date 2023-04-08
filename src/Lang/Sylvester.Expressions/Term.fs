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
    
    static member (==) (l:Term<'t>, r:Term<'t>) = <@ %l.Expr = %r.Expr @> |> Scalar<bool>

    static member (==) (l:Term<'t>, r:'t) = let r' = exprv r in <@ %l.Expr = %r' @> |> Scalar<bool>
    
    static member (==) (l:'t, r:Term<'t>) = let l' = exprv l in <@ %l' = %r.Expr @> |> Scalar<bool>

and [<AbstractClass>] TermVar<'t when 't: equality>(n: string) = 
    inherit Term<'t>(Expr.Var(Var(n, typeof<'t>)) |> expand_as<'t>)
    override x.Display = sprintf "{%A}" n
    member x.Name = n
    member x.Var = match x.Expr with | Var v -> v | _ -> failwith ""
    
and IndexVar(n: string) = inherit TermVar<int>(n)

and Scalar<'t when 't: equality> (expr:Expr<'t>) =
    inherit Term<'t>(expr)
    
    override x.Display = sprinte expr

    override a.Equals (_b:obj) = 
        match _b with 
        | :? Scalar<'t> as b -> (a :> IEquatable<Term<'t>>).Equals b
        | _ -> false
    
    override a.GetHashCode() = a.Expr.GetHashCode()

    interface IComparable<Scalar<'t>> with
       member a.CompareTo b = match (a.Val, b.Val) with | (Some av, Some bv) -> 0 | _, _ -> failwith ""
    
    interface IComparable with
        member a.CompareTo b = 
            match b with
            | :? Scalar<'t> as bs -> (a :> IComparable<Scalar<'t>>).CompareTo bs
            | _ -> failwith "This object is not a term."
    
    interface ISymbolic<Scalar<'t>, 't> with
           member a.Expr = expr
           member a.Mutate(e:Expr<'t>) = Scalar e
           
    interface IHtmlDisplay with
        member x.Html() = latex' x.Expr

    static member Zero = typeof<'t> |> zero_val |> expand_as<'t> |> Scalar<'t>

    static member One = typeof<'t> |> one_val |> expand_as<'t> |> Scalar<'t>

    static member op_Implicit (l:Scalar<'t>):Expr<'t> = l.Expr

    static member op_Implicit (l:'t):Scalar<'t> = Scalar (exprv l)

    static member op_Implicit (l:int):Scalar<real> = let v = real l in Scalar (exprv v)

    static member op_Implicit (l:rat):Scalar<real> = let v = real l in Scalar (exprv v)

    static member op_Implicit (l:nat):Scalar<real> = let v = real l in Scalar (exprv v)

    (* Binary operators *)

    static member (+) (l:Scalar<'t>, r:Scalar<'t>) = call_add (l.Expr) (r.Expr) |> expand_as<'t> |> Scalar<'t>
    
    static member (+) (l:Scalar<'t>, r:'t) = call_add (l.Expr) (Expr.Value r) |> expand_as<'t> |> Scalar<'t>

    static member (+) (l:'t, r:Scalar<'t>) = call_add (Expr.Value l) r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (+) (l:Expr<'t>, r:Scalar<'t>) = call_add l r.Expr |> expand_as<'t> |> Scalar<'t>
    
    static member (+) (l:Scalar<'t>, r:Expr<'t>) = call_add (l.Expr) r |> expand_as<'t> |> Scalar<'t>
    
    static member (+) (l:Scalar<real>, r:int) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand_as<'t> |> Scalar<'t>

    static member (+) (l:Scalar<real>, r:rat) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand_as<'t> |> Scalar<'t>
    
    static member (+) (l:Scalar<real>, r:nat) = let r' = (real) r in call_add (l.Expr) (Expr.Value r') |> expand_as<'t> |> Scalar<'t>

    static member (+) (l:int, r:Scalar<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (+) (l:rat, r:Scalar<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand_as<'t> |> Scalar<'t>
    
    static member (+) (l:nat, r:Scalar<real>) = let l' = (real) l in call_add (Expr.Value l') r.Expr |> expand_as<'t> |> Scalar<'t>
    
    static member (+) (l:Scalar<rat>, r:int) = call_add (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> Scalar<'t>

    static member (+) (l:int, r:Scalar<rat>) = call_add (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (+) (l:Scalar<int>, r:nat) = call_add (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> Scalar<'t>

    
    static member (-) (l:Scalar<'t>, r:Scalar<'t>) = call_sub (l.Expr) (r.Expr) |> expand_as<'t> |> Scalar<'t>

    static member (-) (l:Scalar<'t>, r:'t) = call_sub (l.Expr) (Expr.Value r) |> expand_as<'t> |> Scalar<'t>

    static member (-) (l:'t, r:Scalar<'t>) = call_sub (Expr.Value l) r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (-) (l:Expr<'t>, r:Scalar<'t>) = call_sub l r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (-) (l:Scalar<'t>, r:Expr<'t>) = call_sub (l.Expr) r |> expand_as<'t> |> Scalar<'t>

    static member (-) (l:Scalar<real>, r:int) = let r' = (real) r in call_sub (l.Expr) (Expr.Value r') |> expand_as<'t> |> Scalar<'t>

    static member (-) (l:Scalar<real>, r:rat) = let r' = (real) r in call_sub (l.Expr) (Expr.Value r') |> expand_as<'t> |> Scalar<'t>

    static member (-) (l:Scalar<real>, r:nat) = let r' = (real) r in call_sub (l.Expr) (Expr.Value r') |> expand_as<'t> |> Scalar<'t>

    static member (-) (l:int, r:Scalar<real>) = let l' = (real) l in call_sub (Expr.Value l') r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (-) (l:rat, r:Scalar<real>) = let l' = (real) l in call_sub (Expr.Value l') r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (-) (l:nat, r:Scalar<real>) = let l' = (real) l in call_sub (Expr.Value l') r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (-) (l:Scalar<rat>, r:int) = call_sub (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> Scalar<'t>

    static member (-) (l:int, r:Scalar<rat>) = call_sub (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (-) (l:Scalar<int>, r:nat) = call_sub (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> Scalar<'t>


    static member (*) (l:Scalar<'t>, r:Scalar<'t>) = call_mul (l.Expr) (r.Expr) |> expand_as<'t> |> Scalar<'t>
    
    static member (*) (l:Scalar<'t>, r:'t) = call_mul (l.Expr) (Expr.Value r) |> expand_as<'t> |> Scalar<'t>

    static member (*) (l:'t, r:Scalar<'t>) = call_mul (Expr.Value l) r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (*) (l:Expr<'t>, r:Scalar<'t>) = call_mul l r.Expr |> expand_as<'t> |> Scalar<'t>
    
    static member (*) (l:Scalar<'t>, r:Expr<'t>) = call_mul (l.Expr) r |> expand_as<'t> |> Scalar<'t>
    
    static member (*) (l:Scalar<real>, r:int) = let r' = (real) r in call_mul (l.Expr) (Expr.Value r') |> expand_as<'t> |> Scalar<'t>

    static member (*) (l:Scalar<real>, r:rat) = let r' = (real) r in call_mul (l.Expr) (Expr.Value r') |> expand_as<'t> |> Scalar<'t>
    
    static member (*) (l:Scalar<real>, r:nat) = let r' = (real) r in call_mul (l.Expr) (Expr.Value r') |> expand_as<'t> |> Scalar<'t>

    static member (*) (l:int, r:Scalar<real>) = let l' = (real) l in call_mul (Expr.Value l') r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (*) (l:rat, r:Scalar<real>) = let l' = (real) l in call_mul (Expr.Value l') r.Expr |> expand_as<'t> |> Scalar<'t>
    
    static member (*) (l:nat, r:Scalar<real>) = let l' = (real) l in call_mul (Expr.Value l') r.Expr |> expand_as<'t> |> Scalar<'t>
    
    static member (*) (l:Scalar<rat>, r:int) = call_mul (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> Scalar<'t>

    static member (*) (l:int, r:Scalar<rat>) = call_mul (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (*) (l:Scalar<int>, r:nat) = call_mul (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> Scalar<'t>


    static member (/) (l:Scalar<'t>, r:Scalar<'t>) = call_div (l.Expr) (r.Expr) |> expand_as<'t> |> Scalar<'t>
    
    static member (/) (l:Scalar<'t>, r:'t) = call_div (l.Expr) (Expr.Value r) |> expand_as<'t> |> Scalar<'t>

    static member (/) (l:'t, r:Scalar<'t>) = call_div (Expr.Value l) r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (/) (l:Expr<'t>, r:Scalar<'t>) = call_div l r.Expr |> expand_as<'t> |> Scalar<'t>
    
    static member (/) (l:Scalar<'t>, r:Expr<'t>) = call_div (l.Expr) r |> expand_as<'t> |> Scalar<'t>
    
    static member (/) (l:Scalar<real>, r:int) = let r' = (real) r in call_div (l.Expr) (Expr.Value r') |> expand_as<'t> |> Scalar<'t>

    static member (/) (l:Scalar<real>, r:rat) = let r' = (real) r in call_div (l.Expr) (Expr.Value r') |> expand_as<'t> |> Scalar<'t>
    
    static member (/) (l:Scalar<real>, r:nat) = let r' = (real) r in call_div (l.Expr) (Expr.Value r') |> expand_as<'t> |> Scalar<'t>

    static member (/) (l:int, r:Scalar<real>) = let l' = (real) l in call_div (Expr.Value l') r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (/) (l:rat, r:Scalar<real>) = let l' = (real) l in call_div (Expr.Value l') r.Expr |> expand_as<'t> |> Scalar<'t>
    
    static member (/) (l:nat, r:Scalar<real>) = let l' = (real) l in call_div (Expr.Value l') r.Expr |> expand_as<'t> |> Scalar<'t>
    
    static member (/) (l:Scalar<rat>, r:int) = call_div (l.Expr) (Expr.Value (rat r)) |> expand_as<'t> |> Scalar<'t>

    static member (/) (l:int, r:Scalar<rat>) = call_div (Expr.Value (rat l)) r.Expr |> expand_as<'t> |> Scalar<'t>

    static member (/) (l:Scalar<int>, r:nat) = call_div (l.Expr) (Expr.Value (int r)) |> expand_as<'t> |> Scalar<'t>


    static member Pow (l : Scalar<real>, r : Scalar<real>) = call_pow l.Expr r.Expr |> expand_as<'t> |> Scalar<'t>

    static member Pow (l : Scalar<real>, r : real) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> Scalar<'t>
    
    static member Pow (l : Scalar<real>, r : rat) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> Scalar<'t>

    static member Pow (l : Scalar<real>, r : nat) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> Scalar<'t>

    static member Pow (l : Scalar<real>, r : int) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> Scalar<'t>

    static member Pow (l : Scalar<rat>, r : real) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> Scalar<'t>

    static member Pow (l : Scalar<rat>, r : rat) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> Scalar<'t>

    static member Pow (l : Scalar<rat>, r : int) = call_pow l.Expr (Expr.Value r) |> expand_as<'t> |> Scalar<'t>
         
    static member Pow (l : Scalar<int>, r : int) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> Scalar<'t>
         
    static member Pow (l : Scalar<nat>, r : rat) = call_pow l.Expr (Expr.Value(real r)) |> expand_as<'t> |> Scalar<'t>
    
    static member Pow (l : real, r : Scalar<real>) = call_pow (Expr.Value l) r.Expr |> expand_as<'t> |> Scalar<'t>

    static member Pow (l : int, r : Scalar<real>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> Scalar<'t>

    static member Pow (l : rat, r : Scalar<real>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> Scalar<'t>

    static member Pow (l : nat, r : Scalar<real>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> Scalar<'t>

    static member Pow (l : int, r : Scalar<int>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> Scalar<'t>
    
    static member Pow (l : rat, r : Scalar<rat>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> Scalar<'t>
    
    static member Pow (l : nat, r : Scalar<nat>) = call_pow (Expr.Value(real l)) r.Expr |> expand_as<'t> |> Scalar<'t>

    //static member (==) (l:Scalar<'t>, r:Scalar<'t>) = <@ %l.Expr = %r.Expr @> |> Scalar<bool>

and ScalarVar<'t when 't: equality>(n: string) = 
    inherit Scalar<'t>(Expr.Var(Var(n, typeof<'t>)) |> expand_as<'t>)
    override x.Display = sprintf "{%A}" n
    member x.Name = n
    member x.Var = match x.Expr with | Var v -> v | _ -> failwith ""
    member x.Item(i:IndexVar) = ScalarIndexedVar<'t>(x, i)
    member x.Item(i:int) = ScalarVar<'t>(x.Name + i.ToString())

and ScalarIndexedVar<'t when 't: equality>(var:ScalarVar<'t>, index:IndexVar) =
    inherit ScalarVar<'t>(var.Name + "_" + index.Name)
    member x.Item(i:int) = ScalarVar<'t>(x.Name.Replace("_" + index.Name, i.ToString()))

type realexpr = Scalar<real>

type ratexpr = Scalar<rat>

type intexpr = Scalar<int>

type natexpr = Scalar<nat>

[<RequireQualifiedAccess>]
module NumericLiteralR = 
  let FromZero() = Scalar <@ 0.0 @>
  let FromOne() = Scalar <@ 1.0 @>
  let FromInt32 n = n |> real |> exprv |> Scalar
  let FromInt64 n = n |> real |> exprv |> Scalar
  
[<AutoOpen>]
module Scalar =
    let (|ScalarVar|_|) : obj -> ScalarVar<_> option =
        function
        | :? ScalarVar<_> as v -> Some v
        | _ -> None
    
    let int_expr x = 
        match box x with
        | :? Scalar<int> as s -> s.Expr
        | :? Expr<int> as e -> e
        | :? int as n -> Expr.Value n |> expand_as<int>
        | :? real as n -> Expr.Value ((int) n) |> expand_as<int>
        | _ -> failwithf "The expression %A is not an integer expression." x

    let real_expr x = 
        match box x with
        | :? Scalar<real> as s -> s.Expr
        | :? Expr<real> as e -> e
        | :? real as n -> Expr.Value n |> expand_as<real>
        | :? int as n -> Expr.Value (real n) |> expand_as<real>
        | _ -> failwithf "The expression %A is not a real number expression." x

    let rat_expr x = 
        match box x with
        | :? Scalar<rat> as s -> s.Expr
        | :? Expr<rat> as e -> e
        | :? rat as n -> Expr.Value n |> expand_as<rat>
        | :? int as n -> Expr.Value (rat n) |> expand_as<rat>
        | _ -> failwithf "The expression %A is not a rational number expression." x

    let var6<'t when 't: equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IFormattable> name = 
        Expr.Var(Var(name, typeof<'t>)) |> expand_as<'t> |> Scalar<'t>

    let scalar_terms<'t when 't : equality> (t:obj[]) =
        let m:obj->Scalar<'t> = 
            function
            | :? 't as v -> v |> exprv |> Scalar
            | :? Scalar<'t> as t -> t
            | :? Expr<'t> as e -> e |> Scalar
            | x -> failwithf "Cannot convert %A of type %A to Scalar of type %A." x (x.GetType()) (typeof<'t>)
        t |> Array.map m

    let realterms (t:obj[]) =
        let m:obj->Scalar<real> = 
            function
            | :? real as v -> v |> exprv |> Scalar
            | :? int as v -> v |> real |> exprv |> Scalar
            | :? rat as v -> v |> real |> exprv |> Scalar
            | :? nat as v -> v |> real |> exprv |> Scalar
            | :? Scalar<real> as t -> t
            | :? Expr<real> as e -> e |> Scalar
            | x -> failwithf "Cannot convert %A of type %A to real Scalar." x (x.GetType())
        t |> Array.map m

    let fail_if_not_var(t:Scalar<_>) =
        match t.Expr with
        | Var _ -> ()
        | _ -> failwithf "The term %A is not a variable." t

    let scalar_var<'t when 't : equality> n = ScalarVar<'t> n
    
    let index_var n = IndexVar n
  