#r ".\\..\\..\\..\\src\\Lang\\Sylvester.Expressions\\bin\\Debug\\netstandard2.0\\Sylvester.Expressions.dll"
open System
open FSharp.Quotations

open Sylvester

let find_expr expr =
    
    expr |> traverse' (fun q -> 
    match q with 
    | Patterns.Value(v, typ) -> printfn "Constant: %A" v
    | _ -> ()
    None ) 
  |> ignore


let is_inst_expr (bv:Var) (l:Expr) (r:Expr)=
    let s = src l
    let s' = src r
    let m = l.Substitute(fun v -> if vequal v bv then Expr.Var(new Var("$$_$$", l.Type)) |> Some else None)
    let p = (src m).IndexOf("$$_$$")
    if p = -1 || p > s'.Length - 1 then
        sequal l r
        
    else 
        let v = s' |> Seq.skip p |> Seq.takeWhile(fun c -> c <> ' ') |> Seq.toArray |> String
        let s'' = (src m).Replace("$$_$$", v)
        s' = s''


    //seq {for c in s do yield c}


is_inst_expr (new Var("x", typeof<int>)) <@fun x y -> x + y + 11 @> <@fun x y -> 89 + y + 11 @>    


