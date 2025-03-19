namespace Sylvester

open System

open FSharp.Quotations
open FSharp.Quotations.Patterns
open MathNet.Numerics.LinearRegression
open MathNet.Numerics.Statistics

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

[<AbstractClass>]
type LinearRegressionModel(eqn:ScalarVarMap<real>, samples: (real array*real) seq) =
    let rv = eqn.Rhs |> get_real_vars
    do if samples |> Seq.forall (fun (x, _) -> x.Length = rv.Length) |> not then 
        failwithf "The dimension of a row of the sample data (%A) does not match the number of dependent variables in the regression equation (%A)." (samples |> Seq.find(fun (x,_) -> x.Length <> rv.Length)) rv.Length 
    member val Samples = samples |> Seq.toArray
    member val N = samples |> Seq.length
    member val DependentVariable = eqn.Var
    member val IndependentVariables = rv |> List.toArray
    abstract Variables:ScalarVar<real> array with get
    abstract Parameters:ScalarConst<real> array with get
    abstract RegressionEquation:ScalarEquation<real> with get
    abstract RegressionCoefficients:real array with get
    abstract XSamples:real array array with get
    abstract XMean:real array with get
    abstract XVariance:real array with get
    abstract YSamples:real array with get
    abstract YMean:real with get
    abstract YVariance:real with get

type SimpleLinearRegressionModel(eqn:ScalarVarMap<real>, data1:obj seq, data2: obj seq) =
    inherit LinearRegressionModel(eqn, Seq.zip (Seq.map (to_real >> Array.singleton) data1) (Seq.map to_real data2))
    let dv = eqn.Var
    let rv = eqn |> rhs |> get_real_vars |> function | [v] -> v | _ -> failwithf "%A is not a linear expression of a single variable." (rhs eqn)
    let terms = 
        match eqn |> rhs |> sexpr |> simplifye with
        | LinearTerms [rv.Name] t -> t
        | _ -> failwithf "%A is not a linear expression of a single variable %A." eqn.Rhs rv
    let b0 = 
        match (terms |> List.tryFind(function| [Constant _] -> true | _ -> false)) with
        | Some [Constant (ValueWithName(_,_,n))] -> ScalarConst<real> n
        | _ -> failwithf "Cannot determine the slope intercept parameter symbol."
    let b1 = 
        match (terms |> List.tryFind(function|[Constant _; Variable _] -> true | _ -> false)) with
        | Some [Constant (ValueWithName(_,_,n)); VariableWithName rv.Name _] -> ScalarConst<real> n
        | _ -> failwithf "Cannot determine the slope coefficient parameter symbol."
    let samples = let d = Seq.zip data1 data2 in seq { for x, y in d -> System.Convert.ToDouble x, System.Convert.ToDouble y  } 
    let xsamples,ysamples = samples |> Seq.map fst |> Seq.toArray, samples |> Seq.map snd |> Seq.toArray
    let xmean,xvar = xsamples |> Statistics.MeanVariance
    let ymean,yvar = ysamples |> Statistics.MeanVariance
    let a, b = SimpleRegression.Fit samples
    let rf = fun x -> a + b * x
    let rss = samples |> Seq.sumBy(fun (x,y) -> (y - rf x) ** 2.)
    let ess = ysamples |> Seq.sumBy(fun y -> (y - ymean) ** 2.)
    override val Variables = [|rv; dv|]
    override val Parameters = [|b0; b1|]
    override val RegressionEquation = dv == a + b * rv 
    override val RegressionCoefficients = [|a;b|]
    override val XSamples = [|xsamples|]
    override val XMean = [|xmean|]
    override val XVariance = [|xvar|]
    override val YSamples = ysamples
    override val YMean = ymean
    override val YVariance = yvar

    member val RegressionFunc = rf 
    member val Rss = rss
    member val Ess = ess
    member val Tss = ess + rss
    member val Rsquared = 1. - (ess / rss)
    member x.Item(i:obj) = i |> to_real |> rf

    override x.ToString() = sprintf "%A: %A = %A + %A*%A" (x.Samples) dv a b rv
   
    new(eqn:ScalarEquation<real>, data1:obj seq, data2: obj seq) = SimpleLinearRegressionModel(as_var_map eqn, data1, data2)

type MultipleLinearRegressionModel(eqn:ScalarVarMap<real>, data1: obj[] seq, data2:obj seq) =
    inherit LinearRegressionModel(eqn, Seq.zip (Seq.map(Array.map to_real) data1) (Seq.map to_real data2))
    let dv = eqn.Var
    let rv = eqn |> rhs |> get_real_vars
    let rvn = rv |> Seq.map (fun v -> v.Name)
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
    let samples = base.Samples
    let xsamples,ysamples = samples |> Array.map fst |> Array.transpose, samples |> Array.map snd
    let xmean,xvar = let mv = xsamples |> Array.map Statistics.MeanVariance in Array.map fst mv, Array.map snd mv
    let ymean,yvar = ysamples |> Statistics.MeanVariance
    let a = MultipleRegression.QR(samples, true)
    let re = dv == (a |> Array.skip 1 |> Array.mapi (fun i v -> rv.[i] * v) |> Array.reduce (+)) + a.[0]
    let rf (x:float[]) = (x |> Array.mapi (fun i v -> a.[i] * v) |> Array.reduce (+)) + (a.[0])  
    override val Variables = rv @ [dv] |> List.toArray 
    override val Parameters = [b0] @ (b1 |> List.map snd) |> List.toArray
    override val RegressionEquation = re 
    override val RegressionCoefficients = a
    override val XSamples = xsamples
    override val YSamples = ysamples
    override val XMean = xmean
    override val XVariance = xvar
    override val YMean = ymean
    override val YVariance = yvar

    member __.Item([<ParamArray>] (x:real array)) = rf x
    
    override x.ToString() = sprintf "%A: %A" (x.Samples) re

    new (eqn:ScalarEquation<real>, data1:obj[] seq, data2: obj seq) = MultipleLinearRegressionModel(as_var_map eqn, data1, data2)

module LinearRegression =
    let slrm (eqn:ScalarEquation<real>) (data:seq<seq<_>*_>) =
        let d1 = data |> Seq.map fst |> Seq.map (Seq.item 0 >> box) |> Seq.toArray
        let d2 = data |> Seq.map (snd>>box)
        SimpleLinearRegressionModel(eqn, d1, d2)

    let slrm' (eqn:ScalarEquation<real>) (data:seq<_*_>) =
         let d1, d2 = data |> Seq.map (fst>>box), data |> Seq.map (snd>>box)
         SimpleLinearRegressionModel(eqn, d1, d2)

    let mlrm (eqn:ScalarEquation<real>) (data:seq<seq<_>*_>) =
        let d1, d2 = data |> Seq.map (fst>> Seq.map box >> Seq.toArray), data |> Seq.map (snd>>box)
        MultipleLinearRegressionModel(eqn, d1, d2)

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
   
