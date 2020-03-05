#if BEHAVIOURS
namespace FSketch.Behaviours
#else
namespace FSketch
#endif

module ColorSpaces =

    let HsalToArgb hsla =
        let h, s, l, a = hsla.H, hsla.S, hsla.L, hsla.Alpha

        let a = if a > 1. then 1. else a

        let v = if l <= 0.5 then l * (1. + s) else l + s - l * s

        let r, g, b =
            if v > 0. then
                let m = l + l - v
                let sv = (v - m) / v
                let h = h * 6.
                let sextant = System.Math.Floor h
                let fract = h - sextant
                let vsf = v * sv * fract
                let mid1 = m + vsf
                let mid2 = v - vsf
                match (int)sextant with
                | 0 -> v, mid1, m
                | 1 -> mid2, v, m
                | 2 -> m, v, mid1
                | 3 -> m, mid2, v
                | 4 -> mid1, m, v
                | _ -> v, m, mid2
            else
                l, l, l // default to gray

        { ArgbColor.Alpha = a; R = r; G= g; B = b }
