namespace Sylvester

open System
open System.Collections
open System.Collections.Generic
open System.Linq

open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns

open MathNet.Numerics

open FunScript
open FunScript.Bindings.JSXGraph

module WebVisualization =
    
    let rec make_JS_compat    = 
        function
        | SpecificCall <@@ ( ** ) @@> (_, _, [xt; yt])  -> 
            let xxt, yyt = make_JS_compat xt, make_JS_compat yt
            <@@ FunScript.Arithmetic.MathJS.Pow((%%xxt:float), (%%yyt:float)) @@>
        | expr -> traverse expr make_JS_compat

    let draw_realfun<'a> (attrs:'a) (e:Expr<real->real>)  = 
        let origin = if has_prop "origin" typeof<real*real> attrs then get_prop "origin" typeof<real*real> attrs :?> real*real else 0. ,0.
        let xrange = if has_prop "xrange" typeof<real*real> attrs then get_prop "xrange" typeof<real*real> attrs :?> real*real else 0. ,10.
        let yrange = if has_prop "yrange" typeof<real*real> attrs then get_prop "xrange" typeof<real*real> attrs :?> real*real else 0. ,0.
        let originx, originy = exprv <| fst origin, exprv <| snd origin
        let xmin = exprv (fst xrange)
        let xmax = exprv (snd xrange)
        let __ymin, __ymax = Var("ymin", typeof<real>), Var("ymax", typeof<real>)
        let ymin = if fst yrange <> 0.0 then exprv(fst yrange) else exprvar __ymin
        let ymax = if snd yrange <> 0.0 then exprv(snd yrange) else exprvar __ymax
        let widthx, widthy = <@ (%xmax - %originx) @>, <@ (%ymax - %originy) @>
        let intervalx = <@ %widthx / 10. @>
        let intervaly = <@ %widthy / 10. @>

        let strokeColor = if has_prop "strokeColor" typeof<string> attrs then exprv (get_prop "strokeColor" typeof<string> attrs :?> string) else exprv "black"
        let strokeWidth = if has_prop "strokeWidth" typeof<int> attrs then exprv (get_prop "strokeWidth" typeof<int> attrs :?> int) else exprv 1
        let farg = param_var e
        let fbody = body' e
        let mutable m = fbody.Raw
        let n = sprintf "$$ %s \mapsto %s $$" (latexe (Expr.Var(farg))) (latexe fbody) |> exprv
        let sliders = new List<Var>()
        let _bv = Var("b", typeof<Board>) 
        let bv = Expr.Var(_bv) |> expand_as<Board> 
        let mutable ll = nullv.Raw

        get_symbols m |> List.iter(fun (t, n) -> 
            if has_prop n typeof<real> attrs then 
                let s = get_prop n typeof<real> attrs :?> real in 
                let v = exprv s in 
                m <- replace_expr (Expr.ValueWithName(0., n)) v m
                m <- replace_expr (expr_var<real> n) v m
        )
        
        get_symbols m |> List.iter(fun (t, n) -> 
            if has_prop n typeof<real*real> attrs then 
                if (yrange = (0.,0.)) then failwith "You must specify the yrange if using intervals for parameters"
                let s = get_prop n typeof<real*real> attrs :?> real*real in 
                let min = exprv (fst s) in 
                let max = exprv (snd s) in 
                if not <| sliders.Any(fun s -> s.Name = n) then 
                    sliders.Add(Var(n, typeof<Slider>))
                    let sl = exprv(float sliders.Count)
                    ll <- Expr.Let(sliders.First(fun s -> s.Name = n), (<@slider (0.75 * %xmax) ((0.75 * %ymax) - ((%sl-1.) * %intervaly)) 2. %min %max 1. {|name = %exprv(n)|} %bv @>), ll)
                let _vv =  sliders.First(fun s -> s.Name = n)  
                let vv = <@ %%Expr.Var(_vv):Slider @> in 
                m <- replace_expr (Expr.ValueWithName(0., n)) (<@ (%vv).Value() @>) m
                m <- replace_expr (expr_var<real> n) (<@ (%vv).Value() @>) m
            else
                m <- replace_expr (Expr.ValueWithName(0., n)) (<@ 0. @>) m
                m <- replace_expr (expr_var<real> n) (<@ 0. @>) m
        )
        
        let _jsf = recombine_func [farg] ( m)
        let jsf = <@ %%_jsf:real->real @>
        let ejsf = ev jsf    
        let _nsf = recombine_func [farg] (m |> make_JS_compat)
        let nsf = <@ %%_nsf:real->real @>

        let _fgv = Var("fg", typeof<Functiongraph>)
        let fgv = Expr.Var _fgv
        let fg = Expr.Let(_fgv, <@ functiongraph %nsf %xmin %xmax defaults %bv @>, nullv)
        let _tv = Var("t", typeof<Text>)
        let tv = <@ %%(Expr.Var(_tv)):Text @>
        let t = Expr.Let(_tv, <@ text (%widthx * 0.5) (%ymax + %intervaly - 0.5) %n invisible %bv @>, nullv)

        ll <- replace_expr nullv fg ll
        ll <- replace_expr nullv t ll
        ll <- replace_expr nullv bv ll

        let grid = 
            <@ {| 
                    boundingbox = [|(%originx - %intervalx); (%ymax + %intervaly); (%xmax + (%xmax / 10.)); (%originy - (%ymax / 10.));|]
                    showNavigation = true 
                    showCopyright = false
                    keepAspectRatio = false
                    axis = true 
                |} 
            @>
        ll <- Expr.Let(_bv, <@ board %grid @>, ll)  

        if yrange = (0.,0.) then
            let _ymin,_ymax = ejsf (fst xrange), ejsf (snd xrange)
            ll <- ll.Substitute(fun v -> if v = __ymin then Some(exprv(_ymin).Raw) else None)
            ll <- ll.Substitute(fun v -> if v = __ymax then Some(exprv(_ymax).Raw) else None)

        let lll = ll
        <@ (%%lll:Board) @>
