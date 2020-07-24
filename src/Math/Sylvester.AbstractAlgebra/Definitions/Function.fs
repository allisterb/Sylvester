﻿namespace Sylvester

type IFunction<'t, 'u when 't : equality and 'u : equality> = 
    abstract Domain: Set<'t>
    abstract CoDomain: Set<'u>
    abstract Map: Map<'t, 'u>

type IInjection<'t, 'u when 't: equality and 'u: equality> =
    inherit IFunction<'t, 'u>

type ISurjection<'t, 'u when 't: equality and 'u: equality> =
    inherit IFunction<'t, 'u>

type IBijection<'t, 'u when 't: equality and 'u: equality> =
    inherit IInjection<'t, 'u>
    inherit ISurjection<'t, 'u>

type Fn<'t, 'u when 't: equality and 'u: equality>(domain:Set<'t>, codomain:Set<'u>, map:Map<'t, 'u>) = 
    interface IFunction<'t, 'u> with
        member val Domain = domain
        member val CoDomain = codomain
        member val Map = map

type Injection<'t, 'u when 't: equality and 'u: equality>(domain:Set<'t>, codomain:Set<'u>, map:Map<'t, 'u>) =
    inherit Fn<'t, 'u>(domain, codomain, map)
    interface IInjection<'t, 'u>    