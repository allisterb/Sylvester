namespace Sylvester

open FSharp.Quotations

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

[<AutoOpen>]
module Plot =
    
    let plot2d width height title xaxis_label yaxis_label (traces:seq<Trace>) = Plot2D(width, height, title, xaxis_label, yaxis_label, traces) 

    let lineplot name color min max step (f:real->real) =
        let xdat = seq {min..step..max} in
        let ydat = xdat |> Seq.map f in
        Scatter(x=xdat, y=ydat, line = Line(color=color), mode="lines", name = name, showlegend = true)

    let lineplot_as_func min max (expr:Expr<real>) =
        let f = as_func_of_single_var expr in
        lineplot (sprint' expr) "black" min max 0.1 f

    let with_lineplot_color c (s:Scatter) = s.line.color <- c; s

    let with_lineplot_markers (s:Scatter) = s.mode <- "lines+markers"; s

    let with_lineplot_fill f c (s:Scatter) = s.fill <- f; s.fillcolor <- c; s

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

    let with_histogram_opacity o (h:Histogram) = h.marker.opacity <- o; h

    let dotplot name color (x:seq<'t>) =
        let count = x |> Seq.countBy id
        let xdat,ydat = count |> Seq.map fst, count |> Seq.map snd
        Scatter (
            x = xdat,
            y = ydat,
            name = name,
            mode = "markers",
            marker = 
                Marker(
                    color = color,
                    line = 
                        Line (
                                color = color,
                                width = 1
                            ),
                    symbol = "circle",
                    size = 16
              )
        )

    let with_plot_barmode_overlay (p:Plot2D) = p.Layout.barmode <- "overlay"; p

    let with_plot_x_tick dt ticks (p:Plot2D) = p.Layout.xaxis.autotick <- false; p.Layout.xaxis.dtick <- dt; p.Layout.xaxis.ticks <- ticks; p 
    
    let with_plot_y_tick dt ticks (p:Plot2D) = p.Layout.yaxis.autotick <- false; p.Layout.yaxis.dtick <- dt; p.Layout.yaxis.ticks <- ticks; p