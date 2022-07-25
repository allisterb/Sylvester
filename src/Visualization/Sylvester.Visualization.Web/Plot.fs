module Plot

open System
open FSharp.Quotations
open XPlot.Plotly

open Sylvester

let plot2d<'t when 't:equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> width height label (xrange:seq<real>) (f:real->real) =
    let trace = Scatter(x=xrange, y=(xrange |> Seq.map f))
    [trace] |> Chart.Plot |> Chart.WithWidth width |> Chart.WithHeight height |> Chart.WithLabel label
