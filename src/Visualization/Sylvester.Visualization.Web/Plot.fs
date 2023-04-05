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
    member x.Traces() = x.Plots |> Seq.choose(function | Lineplot x -> x :> Trace |> Some | Histogram x -> x :> Trace |> Some | _ -> None)
    member x.Shapes() = x.Plots |> Seq.choose(function | Shape x -> Some x | _ -> None)
    member x.Annotations() = x.Plots |> Seq.choose(function | Annotation x -> Some x | _ -> None)
    member x.Chart() = 
        x.Layout.shapes <- x.Shapes()
        x.Layout.annotations <- x.Annotations()
        x.Traces() |> Chart.Plot |> Chart.WithWidth x.Width |> Chart.WithHeight x.Height |> Chart.WithTitle x.Title |> Chart.WithLayout x.Layout

    interface IHtmlDisplay with member x.Html() = x.Chart().GetInlineHtml()

and Plottable = 
| Lineplot of Lineplot
| Histogram of Graph.Histogram
| Shape of Graph.Shape
| Annotation of Graph.Annotation
    
and Lineplot(name, color, ?x, ?y, ?expr:Expr<real>) = 
    inherit Scatter(line = Line(color=color), mode="lines", name = name, showlegend = true, x = defaultArg x null, y = defaultArg y null)
    member x.Expr = match expr with | Some e -> sprinte e | _ -> ""
    
    new(name, color, min, max, step, expr:Expr<real>) =
        let f = as_func_of_single_var expr
        let xdat = seq {min..step..max} 
        let ydat = xdat |> Seq.map f
        Lineplot(name, color, xdat, ydat, expr) 
    
module Plot =
    let plot2d width height title xaxis_label yaxis_label (plots:seq<Plottable>) = Plot2D(width, height, title, xaxis_label, yaxis_label, plots) 

    let lineplot min max (expr:Expr<real>) = Lineplot((sprinte expr), "black", min, max, 0.1, expr) |> Plottable.Lineplot 

    let histogram name color (xbin_start:'t) (xbin_end:'t) (xbin_size:'t) (x:seq<'t>) = 
        Graph.Histogram (
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
        |> Plottable.Histogram

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
        //|> Plottable.Trace

    let with_color c (p:Plottable) = 
        match p with
        | Lineplot l -> l.line.color <- c; p
        | Shape s -> s.line.color <- c; p
        | Annotation a -> a.arrowcolor <- c; p
        | _ -> p

    let with_markers (p:Plottable) = 
        match p with
        | Lineplot l -> l.mode <- "lines+markers"; p
        | _ -> p
    
    let with_fill f c (p:Plottable) = 
        match p with
        | Lineplot l -> l.fill <- f; l.fillcolor <- c; p
        | _ -> p

    let with_plot_barmode_overlay (p:Plot2D) = p.Layout.barmode <- "overlay"; p

    let with_plot_x_tick dt ticks (p:Plot2D) = p.Layout.xaxis.autotick <- false; p.Layout.xaxis.dtick <- dt; p.Layout.xaxis.ticks <- ticks; p 
    
    let with_plot_y_tick dt ticks (p:Plot2D) = p.Layout.yaxis.autotick <- false; p.Layout.yaxis.dtick <- dt; p.Layout.yaxis.ticks <- ticks; p

    let circle color x0 x1 y0 y1 = Plottable.Shape <| Graph.Shape(``type``="circle", x0=x0, x1=x1, y0=y0, y1=y1, fillcolor=color, line = Line(color=color, width=1), opacity=0.2)
                    
    let rect color x0 x1 y0 y1 = Plottable.Shape <| Graph.Shape(``type``="rect", x0=x0, x1=x1, y0=y0, y1=y1, fillcolor=color, line = Line(color=color, width=1), opacity=0.2)

   
    let line color x0 x1 y0 y1 = Plottable.Shape <| Graph.Shape(``type``="line", x0=x0, x1=x1, y0=y0, y1=y1, fillcolor=color, line = Line(color=color, width=1))

    let vertical_line color x0 =
        let l = XPlot.Plotly.Graph.Shape(``type``="line", x0=x0, x1=x0, y0=0, y1=1, line = XPlot.Plotly.Graph.Line(color=color, width=0.5))
        l.x0 <- x0
        l.x1 <- x0
        l.y0 <- 0.
        l.y1 <- 1.
        l.yref <- "paper"
        l.xref <- "x"
        l |> Plottable.Shape

    let annotation x y text = Plottable.Annotation <| Graph.Annotation(x=x, y=y, text=text, showarrow=true, arrowhead=1)