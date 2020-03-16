namespace Sylvester

type IFunction<'t, 'u when 't : equality and 'u : equality> = 
    abstract Domain: Set<'t>
    abstract CoDomain: Set<'u>
    abstract Map: Map<'t, 'u>


type Fn<'t, 'u when 't: equality and 'u: equality>(domain:Set<'t>, codomain:Set<'u>, map:Map<'t, 'u>) = 
    interface IFunction<'t, 'u> with
        member val Domain = domain
        member val CoDomain = codomain
        member val Map = map

type IInjection<'t, 'u when 't: equality and 'u: equality> =
    inherit IFunction<'u, 't>

type ISurjection<'t, 'u when 't: equality and 'u: equality> =
    inherit IFunction<'u, 't>

type IBijection<'t, 'u when 't: equality and 'u: equality> =
    inherit IInjection<'u, 't>
    inherit ISurjection<'u, 't>