// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open FSharp.Quotations
open FunScript
open FunScript.Bindings.JSXGraph

open Sylvester

let exprvar2<'t>(n:string) = expand_as<'t>(Expr.Var(Var(n, typeof<'t>)))
[<EntryPoint>]
let main argv =
        
    let draw_v (attrs:'a) (v:Vec<dim<2>>) = 
        let origin = if has_prop "origin" typeof<real*real> attrs then get_prop "origin" typeof<real*real> attrs :?> real*real else 0. ,0.
        let originx, originy = exprv <| fst origin, exprv <| snd origin 
        let mutable m0 = v.Expr.[0].Raw 
        let mutable m1 = v.Expr.[1].Raw
        let _bv = Var("b", typeof<Board>) 
        let bv = Expr.Var(_bv) |> expand_as<Board> 
        let mutable ll = nullv.Raw
        get_symbols m0 |> List.iter(fun (t, n) -> 
            if has_prop n typeof<real*real> attrs then 
                let s = get_prop n typeof<real*real> attrs :?> real*real in 
                let min = exprv (fst s) in 
                let max = exprv (snd s) in 
                let _vv = Var(n, typeof<Slider>)
                let vv = <@ %%Expr.Var(_vv):Slider @> in 
                m0 <- replace_expr (Expr.ValueWithName(0., n)) (<@ (%vv).Value() @>) m0
                m0 <- replace_expr (exprvar2<real> n) (<@ (%vv).Value() @>) m0
                ll <- Expr.Let(_vv, (<@slider 1. 2. 3. %min %max 1. defaults %bv @>), ll))
        ll <- replace_expr nullv bv ll   
        ll <- Expr.Let(_bv, <@ board {| boundingbox = bbox -0.5 10. 10. -0.5 |} @>, ll)  
        let lll = ll
        <@ (%%lll:Board) @>
    //printfn "%s" (compile j)
    //printfn "%s" (compile <@ let x = sin 0.45 in x @>)
    let r, s = realvar2 "r" "s"
    let rr = vec2 r (r + s)
    //printfn "%s" (compile (draw_v {|r = 6.,7. |} rr))
    let jj = <@
                let b = board {||}
                let s = slider 0. 1. 2. 3. 4. 5. {||} b
                b
            @>
    let ggg = draw_v {|r = 6.,7. |} rr
    printfn "%s" (compile (draw_v {|r = 6.,7. |} rr))
    //printfn "%s" (compile jj)
    0 // return an integer exit code
