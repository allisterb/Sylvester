namespace Sylvester

open FSharp.Quotations

open XPlot.Plotly

type Plot2D(width:int, height:int, title:string, xaxis_label:string, yaxis_label:string, plots:seq<Plottable>, ?layout:Layout) = 
    inherit PlotlyChart()
    member val Width = width with get,set
    member val Height = height with get,set
    member val Title = title with get,set
    member val Plots = plots |> System.Collections.Generic.List with get, set
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
    member x.Traces() = x.Plots |> Seq.choose(function | Trace x -> Some x | _ -> None)
    member x.Shapes() = x.Plots |> Seq.choose(function | Shape x -> Some x | _ -> None)
    member x.Annotations() = x.Plots |> Seq.choose(function | Annotation x -> Some x | _ -> None)
    member x.Chart() = 
        x.Layout.shapes <- x.Shapes()
        x.Layout.annotations <- x.Annotations()
        x.Traces() |> Chart.Plot |> Chart.WithWidth x.Width |> Chart.WithHeight x.Height |> Chart.WithTitle x.Title |> Chart.WithLayout x.Layout

    interface IHtmlDisplay with member x.Html() = x.Chart().GetInlineHtml()

and Plottable = 
| Trace of Graph.Trace
| Shape of Graph.Shape
| Annotation of Graph.Annotation
    
[<AutoOpen>]
module Plot =
    
    let plot2d width height title xaxis_label yaxis_label (plots:seq<Plottable>) = Plot2D(width, height, title, xaxis_label, yaxis_label, plots) 

    let with_shapes shapes (p:Plot2D) =
        p.Layout.shapes <- shapes
        p

    let lineplot name color min max step (f:real->real) =
        let xdat = seq {min..step..max} in
        let ydat = xdat |> Seq.map f in
        Scatter(x=xdat, y=ydat, line = Line(color=color), mode="lines", name = name, showlegend = true) 
        :> Trace
        |> Plottable.Trace

    let lineplot_as_func min max (expr:Expr<real>) =
        let f = as_func_of_single_var expr in
        lineplot (sprinte expr) "black" min max 0.1 f

    let with_color c (p:Plottable) = 
        match p with
        | Trace t when (t :? Scatter) -> let s = t :?> Scatter in s.line.color <- c; s :> Trace |> Trace
        | Shape s -> s.line.color <- c; Shape s
        | Annotation a -> a.arrowcolor <- c; Annotation a


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
        :> Trace
        |> Plottable.Trace

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
        :> Trace
        |> Plottable.Trace

    let with_plot_barmode_overlay (p:Plot2D) = p.Layout.barmode <- "overlay"; p

    let with_plot_x_tick dt ticks (p:Plot2D) = p.Layout.xaxis.autotick <- false; p.Layout.xaxis.dtick <- dt; p.Layout.xaxis.ticks <- ticks; p 
    
    let with_plot_y_tick dt ticks (p:Plot2D) = p.Layout.yaxis.autotick <- false; p.Layout.yaxis.dtick <- dt; p.Layout.yaxis.ticks <- ticks; p

    let circle color x0 x1 y0 y1 = Plottable.Shape <| Graph.Shape(``type``="circle", x0=x0, x1=x1, y0=y0, y1=y1, fillcolor=color, line = Line(color=color, width=1), opacity=0.2)
                    
    let rect color x0 x1 y0 y1 = Plottable.Shape <| Graph.Shape(``type``="rect", x0=x0, x1=x1, y0=y0, y1=y1, fillcolor=color, line = Line(color=color, width=1), opacity=0.2)

    let line color x0 x1 y0 y1 = Plottable.Shape <| Graph.Shape(``type``="rect", x0=x0, x1=x1, y0=y0, y1=y1, fillcolor=color, line = Line(color=color, width=1), opacity=0.2)

    let annotation x y text = Plottable.Annotation <| Graph.Annotation(x=x, y=y, text=text, showarrow=true, arrowhead=1)