[<AutoOpen>]
module Plot

open Sylvester
open XPlot.Plotly

let plot2d width height title xaxis_label yaxis_label (traces:seq<#Trace>) = 
    let layout =
        Layout(
            title = title,
            xaxis =
                Xaxis(
                    title = xaxis_label,
                    showgrid = true
                ),
            yaxis =
                Yaxis(
                    title = yaxis_label,
                    showgrid = true
                )
        )

    traces |> Chart.Plot |> Chart.WithWidth width |> Chart.WithHeight height |> Chart.WithTitle title |> Chart.WithLayout layout

let trace2d name color min max step (f:real->real) =
    let xdat = seq {min..step..max}
    let ydat = xdat |> Seq.map f
    Scatter(x=xdat, y=ydat, line = Line(color=color), name = name, showlegend = true)
    
let plot2d_func width height title xaxis_label yaxis_label name color min max step (f:real->real) =
    let trace1 = trace2d name color min max step f
    plot2d width height title xaxis_label yaxis_label [trace1]

