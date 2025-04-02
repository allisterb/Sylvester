namespace Sylvester

open System

open FSharp.Quotations.Patterns

open MathNet.Numerics
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearRegression

open Sylvester.Data

[<AutoOpen>]
module Data =
    let csv_file name = new CsvFile(name)

    let csv_file_with_delim name delim = new CsvFile(name, delim)
    
    let csv_fields (f:obj) = 
        match f with
        | :? string as fn -> fn |> CsvFile |> Seq.map(fun fn -> sprintf "%A:%A" fn.Label fn.Type) |> Seq.toArray
        | :? CsvFile as csvf -> csvf |> Seq.map(fun fn -> sprintf "%A:%A" fn.Label fn.Type) |> Seq.toArray
        | _ -> failwithf "Cannot get CSV fields from %A" f

    let with_field_type<'t> (col:int) (f:CsvFile) = f.[col].Type <- typeof<'t>; f

    let with_all_field_types<'t> (f:CsvFile) = 
        for i in 0..f.Fields.Count - 1 do f.[i].Type <- typeof<'t> 
        f
    
    let frame (file:CsvFile) = new Frame(file)

    let samples (cols:seq<string>) (f:CsvFile) =
        let n = Seq.length cols - 1
        let df = frame f
        let x = cols |> Seq.take (Seq.length cols - 1) |> Seq.toArray |> df.Sel in
        let y = cols |> Seq.last 
        Seq.zip (seq {for r in x -> seq {for i in 0 .. n - 1 -> r.[i]}}) (seq {for r in df.[y] -> r})

type LinearRegressionModel(eqn:ScalarVarMap<real>, samples: (real array*real) array, ?var_changes:ScalarVarMap<real> array) =
    let dv = eqn.Var
    let rvn = eqn |> rhs |> get_real_vars |> Seq.map (fun v -> v.Name)
    let terms = 
        match eqn |> rhs |> sexpr |> simplifye with
        | LinearTermsOf rvn t -> t
        | _ -> failwithf "%A is not a linear expression of variables %A." eqn.Rhs rvn
    let b0 = 
        match (terms |> List.tryFind(function| [Constant _] -> true | _ -> false)) with
        | Some [Constant (ValueWithName(_,_,n))] -> ScalarConst<real> n
        | _ -> failwithf "Cannot determine the slope intercept parameter symbol."
    let b1 = 
        terms |> List.filter(function|[Constant c; VariableWithOneOfNames rvn _] -> true | _ -> false) 
        |> List.map (function | [Constant (ValueWithName(_,_,n)); VariableWithOneOfNames rvn v] -> (v |> get_var |> exprvar |> realvar), ScalarConst<real> n | _ -> failwithf "Cannot determine the slope coefficient parameter symbol.")
        |> List.sortBy (snd >> const_name)
    let rv = b1 |> List.map fst 
    let n = samples |> Array.length
    let dof = n - (1 + List.length b1)
    let xsamples,ysamples = samples |> Array.map fst |> Array.transpose, samples |> Array.map snd
    let xmean,xvar = let mv = xsamples |> Array.map Statistics.MeanVariance in Array.map fst mv, Array.map snd mv
    let ymean,yvar = ysamples |> Statistics.MeanVariance
    let a = 
        if rv.Length = 1 then 
            let p = SimpleRegression.Fit(samples |> Array.map(fun (x,y) -> x.[0], y)) in [|fst p; snd p|] 
        else 
            MultipleRegression.QR(samples, true)
    let re = dv == (a |> Array.skip 1 |> Array.mapi (fun i v -> rv.[i] * v) |> Array.reduce (+)) + a.[0]
    let rf (x:float[]) = (a |> Array.skip 1 |> Array.mapi (fun i v -> x.[i] * v) |> Array.reduce (+)) + (a.[0])  
    let ypred = samples |> Array.map (fst >> rf)
    let ssr = samples |> Array.sumBy(fun (x,y) -> (y - rf x) ** 2.)
    let sse = ysamples |> Array.sumBy(fun y -> (y - ymean) ** 2.)
    let sst = sse + ssr
    let xsst = xsamples |> Array.mapi(fun i s -> Array.sumBy(fun x -> (x - xmean.[i]) ** 2.) s)
    let oeqn : ScalarEquation<real> option = var_changes |> Option.map(Array.fold(fun e cv -> e.SubstVar(cv.Var, cv.Rhs)) (eqn :> ScalarEquation<real>)) 
    member val ModelEquation = eqn
    member val OriginalModelEquation = oeqn 
    member val Samples = samples 
    member val N = n
    member val DependentVariable = eqn.Var
    member val IndependentVariables = rv |> List.toArray
    member val Variables = rv @ [dv] |> List.toArray 
    member val Parameters = [b0] @ (b1 |> List.map snd) |> List.toArray
    member val DegreesOfFreedom = dof
    member val RegressionCoefficients = a
    member val XSamples = xsamples
    member val YSamples = ysamples
    member val XMean = xmean
    member val XVar = xvar
    member val XSd = xvar |> Array.map sqrt
    member val YMean = ymean
    member val YVar = yvar
    member val Ysd = sqrt yvar
    member val Ssr = ssr
    member val Sse = sse
    member val Sst = sst
    member val Ser = sqrt (ssr / (real) dof)
    member val XSst = xsst
    member val RegressionEquation = re
    member val OriginalRegressionEquation : ScalarEquation<real> option = var_changes |> Option.map(fun vc ->  vc |> Array.fold(fun e cv -> e.SubstVar(cv.Var, cv.Rhs)) (re :> ScalarEquation<real>))   
    member val RegressionFunction = rf 
    member val YPredictions = samples |> Array.map (fst >> rf)
    member val R2 : real = GoodnessOfFit.CoefficientOfDetermination(ypred, ysamples)
    member val r : real= GoodnessOfFit.R(ypred, ysamples)

    member __.Item([<ParamArray>] (x:real array)) = rf x
    member __.Copy() = LinearRegressionModel(eqn, samples)
    override x.ToString() = 
        match var_changes with
        | None -> sprintf "%A: %A" (x.Samples) re 
        | Some vc -> sprintf "%A: %A with [%s]" (x.Samples) re (vc |> Array.map(sprintf "%A") |> Array.reduce(sprintf "%s,%s"))

    new (eqn:ScalarVarMap<real>, samples: (real array*real) seq) = LinearRegressionModel(eqn, samples |> Seq.toArray)

module LinearRegression =
    let lr (eqn:ScalarVarMap<real>) (data:seq<seq<_>*_>) =
           let d1, d2 = data |> Seq.map (fst>> Seq.map box >> Seq.toArray), data |> Seq.map (snd>>box)
           LinearRegressionModel(eqn, data |> Seq.map(fun(x,y) -> ((Seq.map to_real x) |> Seq.toArray, to_real y))) 
    
    let slr (eqn:ScalarVarMap<real>) (data:seq<_*_>) =
         lr eqn (data |> Seq.map(fun (x, y) -> Seq.singleton x, y))

    let lrvars (m:LinearRegressionModel) = m.Variables

    let lrsamp (m:LinearRegressionModel) = m.Samples
    
    let lrN (m:LinearRegressionModel) = m.N

    let lrdvar (m:LinearRegressionModel) = m.DependentVariable

    let lrivars (m:LinearRegressionModel) = m.IndependentVariables

    let lrparams (m:LinearRegressionModel) = m.Parameters

    let lreqn (m:LinearRegressionModel) = m.RegressionEquation

    let lroeqn (m:LinearRegressionModel) = m.OriginalRegressionEquation

    let lrmeqn (m:LinearRegressionModel) = m.ModelEquation :> ScalarEquation<real> 

    let lromeqn (m:LinearRegressionModel) = m.OriginalModelEquation

    let lrcoeffs (m:LinearRegressionModel) = m.RegressionCoefficients

    let lrxsamp (m:LinearRegressionModel) = m.XSamples

    let lrysamp (m:LinearRegressionModel) = m.YSamples

    let lrxmean (m:LinearRegressionModel) = m.XMean

    let lrymean (m:LinearRegressionModel) = m.YMean

    let lrssr (m:LinearRegressionModel) = m.Ssr

    let lrsse (m:LinearRegressionModel) = m.Sse

    let lrsst (m:LinearRegressionModel) = m.Sst

    let lrser (m:LinearRegressionModel) = m.Ser

    //let lrsd (m:LinearRegressionModel) = m.YSD

    let lrR2 (m:LinearRegressionModel) = m.R2

    let change_var (eqn:ScalarVarMap<real>) (m:LinearRegressionModel) =
        let rvs = eqn.Rhs |> get_real_vars
        if rvs.Length <> 1 then failwithf "The RHS of the equation must be an expression of a single variable."
        let rv = List.exactlyOne rvs
        if not (m.ModelEquation.Var = rv || Array.contains rv m.IndependentVariables) then failwithf "The RHS of the equation does not contain a dependent or independent variable of the regression equation."
        let f = RealFunction eqn

        if m.ModelEquation.Var = rv then
             let samples = m.Samples |> Array.map(fun s -> fst s, s |> snd |> f.Item) in
             LinearRegressionModel(eqn.Var == m.ModelEquation.Rhs, samples, [|eqn|])
        else
            let index = Array.findIndex((=) rv) m.IndependentVariables
            let samples = m.Samples |> Array.map(fun (x,y) -> x |> Array.mapi (fun i _x -> if index = i then f.Item _x else _x), y) in
            let v = eqn.Lhs in
            let p = subst_var_value rv.Var (v.Expr) m.ModelEquation.Rhs.Expr |> expand_as<real> |> simplifye |> Scalar in
            LinearRegressionModel(m.DependentVariable == p, samples, [|eqn|])
        
    let change_vars (eqns:ScalarVarMap<real> seq) (m:LinearRegressionModel) = 
        let cm = eqns |> Seq.fold (fun s e -> change_var e s) m in 
        LinearRegressionModel(cm.ModelEquation, cm.Samples, eqns |> Seq.toArray)

    let new_vars (eqns:ScalarVarMap<real> seq) (m:LinearRegressionModel) = 
           let cm = eqns |> Seq.fold (fun s e -> change_var e s) m in 
           LinearRegressionModel(cm.ModelEquation, cm.Samples)