namespace Sylvester

type IWebVisualization =
    abstract Draw:'a->Html

[<AutoOpenAttribute>]
module WebVisualization =
    let draw (attrs:'a) (v:IWebVisualization) = v.Draw attrs

