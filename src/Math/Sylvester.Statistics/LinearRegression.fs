namespace Sylvester

open MathNet.Numerics.LinearRegression

type LinearRegressionModel(eqn:ScalarVarMap<real>, y: float seq, x: obj seq) =
    do if Seq.length y <> Seq.length x then failwithf "The size of the dependent variable sample must be the same as the size of the independent variable sample."
    member val DependentVariable = eqn.Var
    member val IndependentVariables = eqn.Rhs |> get_real_vars


type SimpleLinearRegressionModel(eqn:ScalarVarMap<real>, data:seq<float*float>) =
    inherit LinearRegressionModel(eqn, data |> Seq.map fst, data |> Seq.map (snd >> box))
    let a, b = SimpleRegression.Fit data
    member val Data = data