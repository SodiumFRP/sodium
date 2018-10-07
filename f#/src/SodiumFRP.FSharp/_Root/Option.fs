module SodiumFRP.FSharp.Option

let private getValueLazy o = lazy Option.defaultWith (fun () -> invalidOp "This code should not be reachable.") o
let private hasValue (_, hasValue) = hasValue
let private getValue (valueLazy, _) = Lazy.value valueLazy

let whereSome (v : #seq<'T option>) =
    v |> Seq.map (fun i -> (getValueLazy i, Option.isSome i)) |> Seq.filter hasValue |> Seq.map getValue

let allSomeOrNone (v : #seq<'T option>) =
    let o = v |> Seq.map (fun i -> (getValueLazy i, Option.isSome i)) |> Seq.toArray
    if o |> Array.forall hasValue then Some (o |> Seq.map getValue) else None