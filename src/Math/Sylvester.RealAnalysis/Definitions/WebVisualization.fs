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

    let draw_realfun<'a> (attrs:'a) (name:string) (e:Expr<real->real>)  = 
       let xrange = if has_prop "xrange" typeof<real*real> attrs then get_prop "xrange" typeof<real*real> attrs :?> real*real else 0. ,10.
       let  has_yrange = has_prop "yrange" typeof<real*real> attrs
       let yrange = if has_yrange  then get_prop "yrange" typeof<real*real> attrs :?> real*real else 0. ,0.
   
       let xmin = exprv (fst xrange)
       let xmax = exprv (snd xrange)
       let __ymin, __ymax = Var("ymin", typeof<real>), Var("ymax", typeof<real>)
       let ymin = if has_yrange then exprv(fst yrange) else exprvar __ymin
       let ymax = if has_yrange then exprv(snd yrange) else exprvar __ymax
       let widthx, widthy = <@ (%xmax - %xmin) @>, <@ (%ymax - %ymin) @>
       let intervalx = <@ %widthx / 10. @>
       let intervaly = <@ %widthy / 10. @>
   
       let strokeColor = if has_prop "strokeColor" typeof<string> attrs then exprv (get_prop "strokeColor" typeof<string> attrs :?> string) else exprv "blue"
       let strokeWidth = if has_prop "strokeWidth" typeof<int> attrs then exprv (get_prop "strokeWidth" typeof<int> attrs :?> int) else exprv 1
       let farg = param_var e
       let fbody = body' e
       let mutable m = fbody.Raw
       let n = exprv name
       //let n = sprintf "$$ %s \mapsto %s $$" (latexe (Expr.Var(farg))) (latexe fbody) |> exprv
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
       )
   
          
       let _nsf = recombine_func [farg] (m |> make_JS_compat)
       let nsf = <@ %%_nsf:real->real @>
   
       let _fgv = Var("fg", typeof<Functiongraph>)
       let fgv = Expr.Var _fgv
       let fg = Expr.Let(_fgv, <@ functiongraph %nsf %xmin %xmax {|strokeColor=(%strokeColor);strokeWidth=(%strokeWidth)|} %bv @>, nullv)
       let _tv = Var("t", typeof<Text>)
       let tv = <@ %%(Expr.Var(_tv)):Text @>
       let t = Expr.Let(_tv, <@ text (%widthx * 0.5) (%ymax - %intervaly) %n invisible %bv @>, nullv)
   
       ll <- replace_expr nullv fg ll
       ll <- replace_expr nullv t ll
       ll <- replace_expr nullv bv ll
   
       let grid = 
           <@ {| 
                   boundingbox = [|(%xmin - %intervalx); (%ymax + %intervaly); (%xmax + (%xmax / 10.)); (%ymin - (%ymax / 10.));|]
                   showNavigation = true 
                   showCopyright = false
                   keepAspectRatio = false
                   axis = true 
               |} 
           @>
       ll <- Expr.Let(_bv, <@ board %grid @>, ll)  
   
       if not has_yrange then
           let _jsf = recombine_func [farg] ( m)
           let jsf = <@ %%_jsf:real->real @>
           let ejsf = ev jsf 
           let _ymin,_ymax = ejsf (fst xrange), ejsf (snd xrange)
           ll <- ll.Substitute(fun v -> if v = __ymin then Some(exprv(_ymin).Raw) else None)
           ll <- ll.Substitute(fun v -> if v = __ymax then Some(exprv(_ymax).Raw) else None)
       else
           ll <- ll.Substitute(fun v -> if v = __ymin then Some(ymin.Raw) else None)
           ll <- ll.Substitute(fun v -> if v = __ymax then Some(ymax.Raw) else None)
       let lll = ll
       <@ (%%lll:Board) @>

    let draw_realfun2<'a> (attrs:'a) (name:string) (e:Expr<real->real>)  = 
        let xrange = if has_prop "xrange" typeof<real*real> attrs then get_prop "xrange" typeof<real*real> attrs :?> real*real else 0. ,10.
        let  has_yrange = has_prop "yrange" typeof<real*real> attrs
        let yrange = if has_yrange  then get_prop "yrange" typeof<real*real> attrs :?> real*real else 0. ,0. 
        let xmin = exprv (fst xrange)
        let xmax = exprv (snd xrange)
        let __ymin, __ymax = Var("ymin", typeof<real>), Var("ymax", typeof<real>)
        let ymin = if has_yrange then exprv(fst yrange) else exprvar __ymin
        let ymax = if has_yrange then exprv(snd yrange) else exprvar __ymax
        let widthx, widthy = <@ (%xmax - %xmin) @>, <@ (%ymax - %ymin) @>
        let intervalx = <@ %widthx / 10. @>
        let intervaly = <@ %widthy / 10. @>
    
        let xaxis_title = if has_prop "xtitle" typeof<string> attrs then get_prop "xtitle" typeof<string> attrs :?> string else "X"
        let yaxis_title = if has_prop "ytitle" typeof<string> attrs then get_prop "ytitle" typeof<string> attrs :?> string else "Y"
        let title = if has_prop "title" typeof<string> attrs then get_prop "title" typeof<string> attrs :?> string else ""
        let _name = if has_prop "name" typeof<string> attrs then get_prop "name" typeof<string> attrs :?> string else name
        let namev = _name |> exprv
        let strokeColor = if has_prop "strokeColor" typeof<string> attrs then exprv (get_prop "strokeColor" typeof<string> attrs :?> string) else exprv "blue"
        let strokeWidth = if has_prop "strokeWidth" typeof<int> attrs then exprv (get_prop "strokeWidth" typeof<int> attrs :?> int) else exprv 1
        let pointsl = if has_prop "points" typeof<(real*string) list> attrs then (get_prop "points" typeof<(real*string) list> attrs :?> (real*string) list) else []
        let points = pointsl |> List.map fst
        
        let farg = param_var e
        let fbody = body' e
        let mutable m = fbody.Raw
        let n = exprv title
        //let n = sprintf "$$ %s \mapsto %s $$" (latexe (Expr.Var(farg))) (latexe fbody) |> exprv
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
                do if has_prop "points" typeof<(real*string) list> attrs then failwith "Cannot use a range for a parameter if drawing points on the curve."
                
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
        )
    
           
        let _nsf = recombine_func [farg] (m |> make_JS_compat)
        let nsf = <@ %%_nsf:real->real @>
        let _jsf = recombine_func [farg] ( m)
        let jsf = <@ %%_jsf:real->real @>
        
        let _fgv = Var("fg", typeof<Functiongraph>)
        let fgv = Expr.Var _fgv
        let fg = Expr.Let(_fgv, <@ functiongraph %nsf %xmin %xmax {|strokeColor=(%strokeColor);strokeWidth=(%strokeWidth); withLabel=true; name=(%namev)|} %bv @>, nullv)
        let _tv = Var("t", typeof<Text>)
        let tv = <@ %%(Expr.Var(_tv)):Text @>
        let t = Expr.Let(_tv, <@ text (%widthx * 0.5) (%ymax - %intervaly) %n invisible %bv @>, nullv)
    
        ll <- replace_expr nullv fg ll
        ll <- replace_expr nullv t ll
        
        do if has_prop "points" typeof<(real*string) list> attrs then
            let ejsf = ev jsf
            let vpoints = points |> List.map exprv<real>
            let fvpoints = points |> List.map (ejsf >> exprv<real>) 
            let points_labels = pointsl |> List.map snd
            points |> List.iteri (fun i p -> 
        
                let _ppvar = Var("pt" + i.ToString(), typeof<Point>)
                let l = sprintf "%s(%A, %A)" points_labels.[i] (points.[i]) (ejsf points.[i])
                let pt = Expr.Let(_ppvar, <@ point %vpoints.[i] %fvpoints.[i] {| name=(%(exprv l)) |} %bv @>, nullv)
                ll <- replace_expr nullv pt ll
            
                let _ppsxvar = Var("ptsx" + i.ToString(), typeof<PerpendicularSegment>)
                let ex = Expr.Let(_ppsxvar, <@ perp_segment (%bv).defaultAxes.x %(exprvar<Point> _ppvar) {|size = 0; dash = 1; strokeColor=(%strokeColor)|} %bv @>, nullv)
                ll <- replace_expr nullv ex ll
            
                let _ppsyvar = Var("ptsy" + i.ToString(), typeof<PerpendicularSegment>)
                let ey = Expr.Let(_ppsyvar, <@ perp_segment (%bv).defaultAxes.y %(exprvar<Point> _ppvar) {|size = 0; dash = 1; strokeColor=(%strokeColor)|} %bv @>, nullv)
                ll <- replace_expr nullv ey ll
            )
        
        ll <- replace_expr nullv bv ll
    
        let grid = 
            <@ {| 
                    boundingbox = [|(%xmin - %intervalx); (%ymax + %intervaly); (%xmax + (%xmax / 10.)); (%ymin - (%ymax / 10.));|]
                    showNavigation = true 
                    showCopyright = false
                    keepAspectRatio = false
                    axis = true
                    defaultAxes = {|
                                    x = {|name = %(exprv xaxis_title); withLabel = true; label = {|position = "rt";offset = [|-300; -40|];anchorX="middle" |} |}
                                    y = {|name = %(exprv yaxis_title); withLabel = true; label = {|position = "rt";rotate=90; offset = [|-40; -300|]; anchorX="middle" |} |}
                                  |}
               |} 
            @>
        ll <- Expr.Let(_bv, <@ board %grid @>, ll)  
    
        if not has_yrange then
            let ejsf = ev jsf
            let _ymin_,_ymax_ = ejsf (fst xrange), ejsf (snd xrange)
            let _ymin, _ymax = if _ymin_ > _ymax_ then _ymax_, _ymin_ else _ymin_, _ymax_
            
            ll <- ll.Substitute(fun v -> if v = __ymin then Some(exprv(_ymin).Raw) else None)
            ll <- ll.Substitute(fun v -> if v = __ymax then Some(exprv(_ymax).Raw) else None)
        else
            ll <- ll.Substitute(fun v -> if v = __ymin then Some(ymin.Raw) else None)
            ll <- ll.Substitute(fun v -> if v = __ymax then Some(ymax.Raw) else None)
        let lll = ll
        <@ (%%lll:Board) @>
  
    let draw_realfuns<'a> (attrs:'a) (names:string[]) (exprs:Expr<real->real>[])  = 
        let xrange = if has_prop "xrange" typeof<real*real> attrs then get_prop "xrange" typeof<real*real> attrs :?> real*real else 0. ,10.
        do if has_prop "yrange" typeof<real*real> attrs |> not then failwith "You must specify the yrange whn drawing multiple curves"
        let yrange = get_prop "yrange" typeof<real*real> attrs :?> real*real 
        let xmin = exprv (fst xrange)
        let xmax = exprv (snd xrange)
        let __ymin, __ymax = Var("ymin", typeof<real>), Var("ymax", typeof<real>)
        let ymin = exprv(fst yrange)
        let ymax = exprv(snd yrange) 
        let widthx, widthy = <@ (%xmax - %xmin) @>, <@ (%ymax - %ymin) @>
        let intervalx = <@ %widthx / 10. @>
        let intervaly = <@ %widthy / 10. @>
    
        let xaxis_title = if has_prop "xtitle" typeof<string> attrs then get_prop "xtitle" typeof<string> attrs :?> string else "X"
        let yaxis_title = if has_prop "ytitle" typeof<string> attrs then get_prop "ytitle" typeof<string> attrs :?> string else "Y"
        let title = if has_prop "title" typeof<string> attrs then get_prop "title" typeof<string> attrs :?> string else ""
        let _names = if has_prop "names" typeof<string array> attrs then get_prop "names" typeof<string array> attrs :?> string[] else names
        let namesv = _names |> Array.map exprv
        let strokeColor = if has_prop "strokeColor" typeof<string> attrs then exprv (get_prop "strokeColor" typeof<string> attrs :?> string) else exprv "blue"
        let strokeWidth = if has_prop "strokeWidth" typeof<int> attrs then exprv (get_prop "strokeWidth" typeof<int> attrs :?> int) else exprv 1
        let pointsl = if has_prop "points" typeof<(real*string) list> attrs then (get_prop "points" typeof<(real*string) list> attrs :?> (real*string) list) else []
        let points = pointsl |> List.map fst
        
        let farg = exprs |> Array.map param_var
        let fbody = exprs |> Array.map body'
        let mutable mbody = fbody |> Array.map(fun b -> b.Raw)
        let n = exprv title
        //let n = sprintf "$$ %s \mapsto %s $$" (latexe (Expr.Var(farg))) (latexe fbody) |> exprv
        let sliders = new List<Var>()
        let _bv = Var("b", typeof<Board>) 
        let bv = Expr.Var(_bv) |> expand_as<Board> 
        let mutable ll = nullv.Raw
    
        mbody |> Array.iteri(fun i _m ->
            let mutable m = _m
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
            )
            let _nsf = recombine_func [farg.[i]] (m |> make_JS_compat)
            let nsf = <@ %%_nsf:real->real @>
            let _jsf = recombine_func [farg.[i]] (m)
            let jsf = <@ %%_jsf:real->real @>
            let ejsf = ev jsf

            let sc = if has_prop ("strokeColor" + (i+1).ToString()) typeof<string> attrs then get_prop ("strokeColor" + (i+1).ToString()) typeof<string> attrs :?> string |> exprv else strokeColor
            let sw = if has_prop ("strokeWidth" + (i+1).ToString()) typeof<int> attrs then get_prop ("strokeWidth" + (i+1).ToString()) typeof<int> attrs :?> int |> exprv else strokeWidth
            let namev = if has_prop ("name" + (i+1).ToString()) typeof<string> attrs then get_prop ("name" + (i+1).ToString()) typeof<string> attrs :?> string |> exprv else namesv.[i]
            let _fgv = Var("fg" + i.ToString(), typeof<Functiongraph>)
            let fgv = Expr.Var _fgv
            let fg = Expr.Let(_fgv, <@ functiongraph %nsf %xmin %xmax {|strokeColor=(%sc);strokeWidth=(%sw); withLabel=true; name=(%namev) |} %bv @>, nullv)
            ll <- replace_expr nullv fg ll
            
            let vpoints = points |> List.map exprv<real>
            let fvpoints = points |> List.map (ejsf >> exprv<real>) 
            let points_labels = pointsl |> List.map snd
            points |> List.iteri (fun j p -> 
                let _ppvar = Var("pt" + i.ToString() + "_j" + j.ToString(), typeof<Point>)
                let l = sprintf "%s(%A, %A)" points_labels.[j] (points.[j]) (ejsf points.[j])
                let pt = Expr.Let(_ppvar, <@ point %vpoints.[j] %fvpoints.[j] {| name=(%(exprv l)) |} %bv @>, nullv)
                ll <- replace_expr nullv pt ll
                (*
                let _ppsxvar = Var("ptsx" + i.ToString() + "_j" + j.ToString(), typeof<PerpendicularSegment>)
                let ex = Expr.Let(_ppsxvar, <@ perp_segment (%bv).defaultAxes.x %(exprvar<Point> _ppvar) {|size = 0; dash = 1; strokeColor=(%strokeColor)|} %bv @>, nullv)
                ll <- replace_expr nullv ex ll
            
                let _ppsyvar = Var("ptsy" + i.ToString() + "_j" + j.ToString(), typeof<PerpendicularSegment>)
                let ey = Expr.Let(_ppsyvar, <@ perp_segment (%bv).defaultAxes.y %(exprvar<Point> _ppvar) {|size = 0; dash = 1; strokeColor=(%strokeColor)|} %bv @>, nullv)
                ll <- replace_expr nullv ey ll
                *)
            )
        )
        
        let _tv = Var("t", typeof<Text>)
        let tv = <@ %%(Expr.Var(_tv)):Text @>
        let t = Expr.Let(_tv, <@ text (%widthx * 0.5) (%ymax - %intervaly) %n invisible %bv @>, nullv)
        ll <- replace_expr nullv t ll
        
        ll <- replace_expr nullv bv ll
    
        let grid = 
            <@ {| 
                    boundingbox = [|(%xmin - %intervalx); (%ymax + %intervaly); (%xmax + (%xmax / 10.)); (%ymin - (%ymax / 10.));|]
                    showNavigation = true 
                    showCopyright = false
                    keepAspectRatio = false
                    axis = true
                    defaultAxes = {|
                                    x = {|name = %(exprv xaxis_title); withLabel = true; label = {|position = "rt";offset = [|-300; -40|];anchorX="middle" |} |}
                                    y = {|name = %(exprv yaxis_title); withLabel = true; label = {|position = "rt";rotate=90; offset = [|-40; -300|]; anchorX="middle" |} |}
                                  |}
               |} 
            @>
        ll <- Expr.Let(_bv, <@ board %grid @>, ll)  
        ll <- ll.Substitute(fun v -> if v = __ymin then Some(ymin.Raw) else None)
        ll <- ll.Substitute(fun v -> if v = __ymax then Some(ymax.Raw) else None)
        let lll = ll
        <@ (%%lll:Board) @>    
