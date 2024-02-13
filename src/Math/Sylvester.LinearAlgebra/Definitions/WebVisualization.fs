namespace Sylvester

open System
open System.Collections
open System.Collections.Generic
open System.Linq

open FSharp.Quotations

open MathNet.Numerics

open FunScript
open FunScript.Bindings.JSXGraph

module WebVisualization =
    let draw_vec2<'a> (attrs:'a) (e0:Expr) (e1:Expr) = 
        let origin = get_prop_else<real*real> "origin" (0. ,0.) attrs
        let range = get_prop_else<real*real> "range" (10. ,10.) attrs
        let originx, originy = exprv <| fst origin, exprv <| snd origin
        let rangex, rangey = exprv <| fst range, exprv <| snd range
        let widthx, widthy = <@ (%rangex - %originx) @>, <@ (%rangey - %originy) @>
        let title = sprintf "$$ \mathbb{R}^ 2 vector \\begin{pmatrix} %s \\\\ %s \\end{pmatrix} $$" (latexe e0) (latexe e1)
        let n = title |> exprv
        let intervalx = <@ %widthx / 10. @>
        let intervaly = <@ %widthy / 10. @>
        let mutable m0 = e0 
        let mutable m1 = e1
        let sliders = new List<Var>()
        let _bv = Var("b", typeof<Board>) 
        let bv = Expr.Var(_bv) |> expand_as<Board> 
        let mutable ll = nullv.Raw

        get_symbols m0 |> List.iter(fun (t, n) -> 
            if has_prop<real> n attrs then 
                let s = get_prop<real> n attrs in 
                let v = exprv s in 
                m0 <- replace_expr (Expr.ValueWithName(0., n)) v m0
                m0 <- replace_expr (expr_var<real> n) v m0
        )

        get_symbols m1 |> List.iter(fun (t, n) -> 
            if has_prop<real> n attrs then 
                let s = get_prop<real> n attrs in 
                let v = exprv s in 
                m1 <- replace_expr (Expr.ValueWithName(0., n)) v m1
                m1 <- replace_expr (expr_var<real> n) v m1
        )
    
        get_symbols m0 |> List.iter(fun (t, n) -> 
            if has_prop<real*real> n attrs then 
                let s = get_prop<real*real> n attrs in 
                let min = exprv (fst s) in 
                let max = exprv (snd s) in 
                if not <| sliders.Any(fun s -> s.Name = n) then 
                    sliders.Add(Var(n, typeof<Slider>))
                    let sl = exprv(float sliders.Count)
                    ll <- Expr.Let(sliders.First(fun s -> s.Name = n), (<@slider (0.75 * %rangex) ((0.75 * %rangey) - ((%sl-1.) * %intervaly)) 2. %min %max 1. {|name = %exprv(n)|} %bv @>), ll)
                let _vv =  sliders.First(fun s -> s.Name = n)  
                let vv = <@ %%Expr.Var(_vv):Slider @> in 
                m0 <- replace_expr (Expr.ValueWithName(0., n)) (<@ (%vv).Value() @>) m0
                m0 <- replace_expr (expr_var<real> n) (<@ (%vv).Value() @>) m0
            else
                m0 <- replace_expr (Expr.ValueWithName(0., n)) (<@ 0. @>) m0
                m0 <- replace_expr (expr_var<real> n) (<@ 0. @>) m0
        )
    
        get_symbols m1 |> List.iter(fun (t, n) -> 
            if has_prop<real*real> n attrs then 
                let s = get_prop<real*real> n attrs in 
                let min = exprv (fst s) in 
                let max = exprv (snd s) in 
                if not <| sliders.Any(fun s -> s.Name = n) then 
                    sliders.Add(Var(n, typeof<Slider>))
                    let sl = exprv(float sliders.Count)
                    ll <- Expr.Let(sliders.First(fun s -> s.Name = n), (<@slider (0.75 * %rangex) ((0.75 * %rangey) - ((%sl-1.) * %intervaly)) 2. %min %max 1. {|name = %exprv(n)|} %bv @>), ll)
                let _vv =  sliders.First(fun s -> s.Name = n)  
                let vv = <@ %%Expr.Var(_vv):Slider @> in 
                m1 <- replace_expr (Expr.ValueWithName(0., n)) (<@ (%vv).Value() @>) m1
                m1 <- replace_expr (expr_var<real> n) (<@ (%vv).Value() @>) m1
            else
                m1 <- replace_expr (Expr.ValueWithName(0., n)) (<@ 0. @>) m1
                m1 <- replace_expr (expr_var<real> n) (<@ 0. @>) m1
        )

        let _ov = Var("o", typeof<Point>)
        let ov = <@ %%(Expr.Var(_ov)):Point @>
        let o = Expr.Let(_ov, <@ point %originx %originy invisible %bv @>, nullv)
        let _pv = Var("p", typeof<Point>)
        let pv = <@ %%(Expr.Var(_pv)):Point @>
        let p = Expr.Let(_pv, <@ pointf (fun () -> %%m0:real) (fun () -> %%m1:real) defaults %bv @>, nullv)
        let _av = Var("a", typeof<Arrow>)
        let av = Expr.Var _av
        let a = Expr.Let(_av, <@ arrow %ov %pv defaults %bv @>, nullv)
        let _tv = Var("t", typeof<Text>)
        let tv = <@ %%(Expr.Var(_tv)):Text @>
        let t = Expr.Let(_tv, <@ text (%widthx * 0.5) (%rangey + %intervaly - 0.5) %n invisible %bv @>, nullv)
    
        ll <- replace_expr nullv o ll 
        ll <- replace_expr nullv p ll
        ll <- replace_expr nullv a ll
        ll <- replace_expr nullv t ll
        ll <- replace_expr nullv bv ll   
        let grid = 
            <@ {| 
                    boundingbox = [|(%originx - %intervalx); (%rangey + %intervaly); (%rangex + (%rangex / 10.)); (%originy - (%rangey / 10.));|]
                    showNavigation = true 
                    showCopyright = false
                    keepAspectRatio = false
                    axis = true 
                |} 
            @>
        ll <- Expr.Let(_bv, <@ board %grid @>, ll)  
        let lll = ll
        <@ (%%lll:Board) @>
