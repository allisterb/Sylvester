namespace Sylvester

type IWebVisualization =
    abstract Draw:obj->Html

[<AutoOpenAttribute>]
module WebVisualization =
    let draw (attrs:obj) (v:IWebVisualization) = v.Draw attrs

