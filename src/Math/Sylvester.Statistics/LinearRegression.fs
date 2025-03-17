namespace Sylvester

open System
open FSharp.Quotations.Patterns
open MathNet.Numerics.LinearRegression
open MathNet.Numerics.Statistics
type LinearRegressionModel(eqn:ScalarVarMap<real>, samples: (real array*real) seq) =
    member val Samples = samples
    member val DependentVariable = eqn.Var
    member val IndependentVariables = eqn.Rhs |> get_real_vars

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
    let xsamples,ysamples = samples |> Seq.map fst, samples |> Seq.map snd
    let xmean,xvar = xsamples |> Statistics.MeanVariance
    let ymean,yvar = ysamples |> Statistics.MeanVariance
    let a, b = SimpleRegression.Fit samples
    let rf = fun x -> a + b * x
    let rss = samples |> Seq.sumBy(fun (x,y) -> (y - rf x) ** 2.)
    let ess = ysamples |> Seq.sumBy(fun y -> (y - ymean) ** 2.)
    member val Variables = [|rv; dv|]
    member val ParameterConsts = [b0; b1]
    member val RegressionEquation = b0 + b1 * rv 
    member val RegressionFunc = rf 
    member val Parameters = [a;b]
    member val Rss = rss
    member val Ess = ess
    member val Tss = ess + rss
    member val Rsquared = 1. - (ess / rss)
    member x.Item(i:obj) = i |> to_real |> rf

    override x.ToString() = sprintf "%A: %A + %A*%A" (x.Samples) a b rv
   
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
           |> List.map (function | [Constant (ValueWithName(_,_,n)); VariableWithOneOfNames rvn v] -> (get_var v).Name, ScalarConst<real> n | _ -> failwithf "Cannot determine the slope coefficient parameter symbol.")
           //|> List.sortBy const_name

    let a = MultipleRegression.Svd base.Samples
module SimpleLinearRegression =
    let slrm (eqn:ScalarEquation<real>) (data:seq<_*_>) =
        let d1, d2 = data |> Seq.map (fst>>box), data |> Seq.map (snd>>box)
        SimpleLinearRegressionModel(eqn, d1, d2)

    let slreqn (m:SimpleLinearRegressionModel) = m.RegressionEquation