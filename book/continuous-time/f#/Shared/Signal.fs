namespace Shared

type Signal = { t0 : float; a : float; b : float; c : float }

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Signal =
    open Sodium.Frp

    let quantum = 0.000001

    let private isCloseTo x y = abs (x - y) < quantum

    let valueAt s t =
        let x = t - s.t0
        s.a * x * x + s.b * x + s.c

    let ``when`` s x =
        let c = s.c - x
        if isCloseTo s.a 0.0
        then
            let t = -c / s.b
            if t >= quantum then Some (t + s.t0) else None
        else
            let b24ac = sqrt (s.b * s.b - 4.0 * s.a * c)
            let t1 = (-s.b + b24ac) / (2.0 * s.a)
            let t2 = (-s.b - b24ac) / (2.0 * s.a)
            if t1 >= quantum
            then if t2 >= quantum then Some ((if t1 < t2 then t1 else t2) + s.t0) else Some (t1 + s.t0)
            else if t2 >= quantum then Some (t2 + s.t0) else None

    let integrate s initial =
        if not (isCloseTo s.a 0.0) then invalidOp "Signal can't handle x^3"
        { t0 = s.t0; a = s.b / 2.0; b = s.c; c = initial }

    let integrateCell s initial =
        let sSignal = s |> updatesC
        let integrate' n o = integrate n (valueAt o n.t0)
        sSignal |> accumS (integrate (s |> sampleC) initial) integrate'