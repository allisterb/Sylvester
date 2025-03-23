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

[<AbstractClass>]
type LinearRegressionModel(eqn:ScalarVarMap<real>, samples: (real array*real) seq) =
    let rv = eqn.Rhs |> get_real_vars
    do if samples |> Seq.forall (fun (x, _) -> x.Length = rv.Length) |> not then 
        failwithf "The dimension of a row of the sample data (%A) does not match the number of dependent variables in the regression equation (%A)." (samples |> Seq.find(fun (x,_) -> x.Length <> rv.Length)) rv.Length 
    
    abstract Variables:ScalarVar<real> array with get
    abstract Parameters:ScalarConst<real> array with get
    abstract RegressionEquation:ScalarVarMap<real> with get
    abstract RegressionCoefficients:real array with get
    abstract RegressionFunction:real array->real
    abstract XSamples:real array array with get
    abstract XMean:real array with get
    abstract XVariance:real array with get
    abstract YSamples:real array with get
    abstract YMean:real with get
    abstract YVariance:real with get
    abstract Rss:real with get
    
    member val ModelEquation = eqn
    member val Samples = samples |> Seq.toArray
    member val N = samples |> Seq.length
    member val DependentVariable = eqn.Var
    member val IndependentVariables = rv |> List.toArray
    member x.YPredictions = x.Samples |> Array.map (fst >> x.RegressionFunction)
    member x.Ess = x.YSamples |> Seq.sumBy(fun y -> (y - x.YMean) ** 2.)
    member x.Tss = x.Rss + x.Ess
    member x.R2 : real = GoodnessOfFit.CoefficientOfDetermination(x.YPredictions, x.YSamples)
    member x.r : real= GoodnessOfFit.R(x.YPredictions, x.YSamples)
    member x.XSE = Distributions.StudentT.InvCDF(0., 1., ((float) (x.N) - 1.), (1. - 0.05) / 2.)
    member x.YSE : real = GoodnessOfFit.StandardError(x.YPredictions, x.YSamples, x.Parameters.Length)
    member x.XSD : real= sqrt x.YVariance
    //member x.T = Distributions.StudentT. 
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
    let rf = fun (x:real array) -> a + b * x.[0]
    let rss = samples |> Seq.sumBy(fun (x,y) -> (y - rf [|x|]) ** 2.)
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
    override val Rss = rss

    override x.RegressionFunction(o:real array) = rf o
   
    member x.Item(i:obj) = i |> to_real |> Array.singleton |> rf

    override x.ToString() = sprintf "%A: %A = %A + %A*%A" (x.Samples) dv a b rv
   
    

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
    let rf (x:float[]) = (a |> Array.skip 1 |> Array.mapi (fun i v -> x.[i] * v) |> Array.reduce (+)) + (a.[0])  
    let rss = samples |> Seq.sumBy(fun (x,y) -> (y - rf x) ** 2.)
    let ess = ysamples |> Seq.sumBy(fun y -> (y - ymean) ** 2.)
    override val Variables = rv @ [dv] |> List.toArray 
    override val Parameters = [b0] @ (b1 |> List.map snd) |> List.toArray
    override val RegressionEquation = re 
    override val RegressionCoefficients = a
    override x.RegressionFunction(o:real array) = rf o
    override val XSamples = xsamples
    override val YSamples = ysamples
    override val XMean = xmean
    override val XVariance = xvar
    override val YMean = ymean
    override val YVariance = yvar
    override val Rss = rss
   
    member __.Item([<ParamArray>] (x:real array)) = rf x
    
    override x.ToString() = sprintf "%A: %A" (x.Samples) re

module LinearRegression =
    let slr (eqn:ScalarVarMap<real>) (data:seq<seq<_>*_>) =
        let d1 = data |> Seq.map fst |> Seq.map (Seq.item 0 >> box) |> Seq.toArray
        let d2 = data |> Seq.map (snd>>box)
        SimpleLinearRegressionModel(eqn, d1, d2)

    let slr' (eqn:ScalarVarMap<real>) (data:seq<_*_>) =
         let d1, d2 = data |> Seq.map (fst>>box), data |> Seq.map (snd>>box)
         SimpleLinearRegressionModel(eqn, d1, d2)

    let mlr (eqn:ScalarVarMap<real>) (data:seq<seq<_>*_>) =
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

    let lrrss (m:LinearRegressionModel) = m.Rss

    let lress (m:LinearRegressionModel) = m.Ess

    let lrtss (m:LinearRegressionModel) = m.Tss

    let lrse (m:LinearRegressionModel) = m.YSE

    //let lrsd (m:LinearRegressionModel) = m.YSD

    let lrR2 (m:LinearRegressionModel) = m.R2

    let change_var (eqn:ScalarVarMap<real>) (m:LinearRegressionModel) =
        let rvs = eqn.Rhs |> get_real_vars
        if rvs.Length <> 1 then failwithf "The RHS of the equation must be an expresion of a single variable."
        let rv = List.exactlyOne rvs
        if not (m.ModelEquation.Var = rv || Array.contains rv m.IndependentVariables) then failwithf "The RHS of the equation does not contain a dependent or independent variable of the regression equation."
        let f = RealFunction eqn
        let create = 
            match m with
            | :? SimpleLinearRegressionModel -> fun (g:ScalarVarMap<real>) s -> slr g (s |> Array.map(fun (x,y) -> x :> seq<real>,y)) :> LinearRegressionModel
            | :? MultipleLinearRegressionModel -> fun (g:ScalarVarMap<real>) s -> mlr g (s |> Array.map(fun (x,y) -> x :> seq<real>,y)) :> LinearRegressionModel
            | _ -> failwithf "Unknown linear regression type: %A" (m.GetType())
        if m.ModelEquation.Var = rv then
             let samples = m.Samples |> Array.map(fun s -> fst s, s |> snd |> f.Map) in
             create (eqn.Var == m.ModelEquation.Rhs) samples
        else
            let index = Array.findIndex((=) rv) m.IndependentVariables
            let samples = m.Samples |> Array.map(fun (x,y) -> x |> Array.mapi (fun i _x -> if index = i then f.Map _x else _x), y) in
            let v = eqn.Lhs in
            let p = subst_var_value rv.Var (v.Expr) m.ModelEquation.Rhs.Expr |> expand_as<real> |> simplifye |> Scalar in
            create (m.DependentVariable == p) samples
        
    let change_vars (eqns:ScalarVarMap<real> seq) (m:LinearRegressionModel) = eqns |> Seq.fold (fun s e -> change_var e s) m

