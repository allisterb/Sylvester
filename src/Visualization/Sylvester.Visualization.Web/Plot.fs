[<AutoOpen>]
module Plot

open Sylvester
open XPlot.Plotly

type Plot2D(width:int, height:int, title:string, xaxis_label:string, yaxis_label:string, traces:seq<Trace>, ?layout:Layout) = 
    inherit PlotlyChart()
    member val Width = width with get,set
    member val Height = height with get,set
    member val Title = title with get,set
    member val Traces = new System.Collections.Generic.List<Trace>(traces)
    member val Layout = 
        defaultArg layout (
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
        ) with get, set
    
    member x.Chart() = x.Traces |> Chart.Plot |> Chart.WithWidth x.Width |> Chart.WithHeight x.Height |> Chart.WithTitle x.Title |> Chart.WithLayout x.Layout

    interface IHtmlDisplay with member x.Html() = x.Chart().GetInlineHtml()

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


let histogram name color (xbin_start:'t) (xbin_end:'t) (xbin_size:'t) (x:seq<'t>) = 
    Histogram (
        name = name,
        x = x,
        autobinx = false,
        xbins =
            Xbins(
                start = real xbin_start,
                ``end`` = real xbin_end,
                size = real xbin_size
            ),
        marker =
            Marker(
                color = color,
                line =
                    Line(
                        color = "grey",
                        width = 0
                    )
            )
    
    )

let with_histogram_opacity o (h:Histogram) = 
    h.marker.opacity <- o
    h

let with_plot_barmode_overlay (p:Plot2D) =
    p.Layout.barmode <- "overlay"
    p