namespace Sylvester

open System
open FSharp.Quotations.Patterns
open MathNet.Numerics.LinearRegression
open MathNet.Numerics.Statistics
type LinearRegressionModel(eqn:ScalarVarMap<real>, y: float seq, x: obj seq) =
    do if Seq.length y <> Seq.length x then failwithf "The size of the dependent variable sample must be the same as the size of the independent variable sample."
    member val DependentVariable = eqn.Var
    member val IndependentVariables = eqn.Rhs |> get_real_vars

type SimpleLinearRegressionModel(eqn:ScalarVarMap<real>, data1:obj seq, data2: obj seq) =
    inherit LinearRegressionModel(eqn, data1 |> Seq.map System.Convert.ToDouble, data2)
    let dv = eqn.Var
    let rv = eqn |> rhs |> get_real_vars |> function | [v] -> v | _ -> failwithf "%A is not a linear expression of a single variable." (rhs eqn)
    let terms = 
        match eqn |> rhs |> sexpr |> simplifye with
        | LinearTerms rv.Name t -> t
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
    let xsamples,ysamples = samples |> Seq.map fst, samples |> Seq.map snd
    let xmean,xvar = xsamples |> Statistics.MeanVariance
    let ymean,yvar = ysamples |> Statistics.MeanVariance
    let a, b = SimpleRegression.Fit samples
    let rf = fun x -> a + b * x
    let rss = samples |> Seq.sumBy(fun (x,y) -> (y - rf x) ** 2.)
    let ess = ysamples |> Seq.sumBy(fun y -> (y - ymean) ** 2.)
    member val Variables = [|rv; dv|]
    member val Samples = seq { for x, y in samples -> [ x; y] } |> array2D
    member val ParameterConsts = [b0; b1]
    member val RegressionEquation = b0 + b1 * rv 
    member val RegressionFunc = rf 
    member val Parameters = [a;b]
    member val Rss = rss
    member val Ess = ess
    member val Tss = ess + rss
    member val Rsquared = 1. - (ess / rss)
    member x.Item(i:obj) = i |> System.Convert.ToDouble |> x.RegressionFunc 

    override x.ToString() = sprintf "%A: %A + %A*%A" (x.Samples) a b rv
   
    new(eqn:ScalarEquation<real>, data1:obj seq, data2: obj seq) = SimpleLinearRegressionModel(as_var_map eqn, data1, data2)

module SimpleLinearRegression =
    let slrm (eqn:ScalarEquation<real>) (data:seq<_*_>) =
        let d1, d2 = data |> Seq.map (fst>>box), data |> Seq.map (snd>>box)
        SimpleLinearRegressionModel(eqn, d1, d2)

    let slreqn (m:SimpleLinearRegressionModel) = m.RegressionEquation