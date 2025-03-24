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

    let csv_file_delim name delim = new CsvFile(name, delim)
    
    let with_col_type<'t> (col:int) (f:CsvFile) = f.[col].Type <- typeof<'t>; f

    let with_all_col_types<'t> (f:CsvFile) = 
        for i in 0..f.Fields.Count - 1 do f.[i].Type <- typeof<'t> 
        f
    
    let frame (file:CsvFile) = new Frame(file)

    let samples (cols:seq<string>) (f:CsvFile) =
        let n = Seq.length cols - 1
        let df = frame f
        let x = cols |> Seq.take (Seq.length cols - 1) |> Seq.toArray |> df.Sel in
        let y = cols |> Seq.last 
        Seq.zip (seq {for r in x -> seq {for i in 0 .. n - 1 -> r.[i]}}) (seq {for r in df.[y] -> r})

type LinearRegressionModel(eqn:ScalarVarMap<real>, samples: (real array*real) array) =
    let dv = eqn.Var
    let rvn = eqn |> rhs |> get_real_vars |> Seq.map (fun v -> v.Name)
    let terms = 
        match eqn |> rhs |> sexpr |> simplifye with
        | LinearTerms rvn t -> t
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
    let rss = samples |> Seq.sumBy(fun (x,y) -> (y - rf x) ** 2.)
    let ess = ysamples |> Seq.sumBy(fun y -> (y - ymean) ** 2.)

    member val ModelEquation = eqn
    member val Samples = samples |> Seq.toArray
    member val N = samples |> Seq.length
    member val DependentVariable = eqn.Var
    member val IndependentVariables = rv |> List.toArray
    member val Variables = rv @ [dv] |> List.toArray 
    member val Parameters = [b0] @ (b1 |> List.map snd) |> List.toArray
    member val RegressionCoefficients = a
    member val XSamples = xsamples
    member val YSamples = ysamples
    member val XMean = xmean
    member val XVariance = xvar
    member val YMean = ymean
    member val YVariance = yvar
    member val Rss = rss
    member val Ess = ess
    member val Tss = ess + rss
    member val RegressionEquation = re
    member val RegressionFunction = rf 
    member val YPredictions = samples |> Array.map (fst >> rf)
    member val R2 : real = GoodnessOfFit.CoefficientOfDetermination(ypred, ysamples)
    member val r : real= GoodnessOfFit.R(ypred, ysamples)
    member x.XSE = Distributions.StudentT.InvCDF(0., 1., ((float) (x.N) - 1.), (1. - 0.05) / 2.)
    member x.YSE : real = GoodnessOfFit.StandardError(x.YPredictions, x.YSamples, x.Parameters.Length)
    member x.XSD : real= sqrt x.YVariance

    member __.Item([<ParamArray>] (x:real array)) = rf x
    
    override x.ToString() = sprintf "%A: %A" (x.Samples) re

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

    let lrcoeffs (m:LinearRegressionModel) = m.RegressionCoefficients

    let lrxsamp (m:LinearRegressionModel) = m.XSamples

    let lrysamp (m:LinearRegressionModel) = m.YSamples

    let lrxmean (m:LinearRegressionModel) = m.XMean

    let lrymean (m:LinearRegressionModel) = m.YMean

    let lrrss (m:LinearRegressionModel) = m.Rss

    let lress (m:LinearRegressionModel) = m.Ess

    let lrtss (m:LinearRegressionModel) = m.Tss

    let lrse (m:LinearRegressionModel) = m.YSE

    //let lrsd (m:LinearRegressionModel) = m.YSD

    let lrR2 (m:LinearRegressionModel) = m.R2

    let change_var (eqn:ScalarVarMap<real>) (m:LinearRegressionModel) =
        let rvs = eqn.Rhs |> get_real_vars
        if rvs.Length <> 1 then failwithf "The RHS of the equation must be an expression of a single variable."
        let rv = List.exactlyOne rvs
        if not (m.ModelEquation.Var = rv || Array.contains rv m.IndependentVariables) then failwithf "The RHS of the equation does not contain a dependent or independent variable of the regression equation."
        let f = RealFunction eqn

        if m.ModelEquation.Var = rv then
             let samples = m.Samples |> Array.map(fun s -> fst s, s |> snd |> f.Map) in
             LinearRegressionModel(eqn.Var == m.ModelEquation.Rhs, samples)
        else
            let index = Array.findIndex((=) rv) m.IndependentVariables
            let samples = m.Samples |> Array.map(fun (x,y) -> x |> Array.mapi (fun i _x -> if index = i then f.Map _x else _x), y) in
            let v = eqn.Lhs in
            let p = subst_var_value rv.Var (v.Expr) m.ModelEquation.Rhs.Expr |> expand_as<real> |> simplifye |> Scalar in
            LinearRegressionModel(m.DependentVariable == p, samples)
        
    let change_vars (eqns:ScalarVarMap<real> seq) (m:LinearRegressionModel) = eqns |> Seq.fold (fun s e -> change_var e s) m

