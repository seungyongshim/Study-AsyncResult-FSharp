type Effect<'a> =
    | Log of string * (unit -> Effect<'a>)
    | Result of 'a

let handle effect =
    let rec loop log =
        function
        | Log (str, cont) ->
            let log' = str :: log
            loop log' (cont ())
        | Result res -> res, log

    let result, log = loop [] effect
    result, log |> List.rev

let rec bind f =
    function
    | Log (str, cont) -> Log(str, (fun () -> cont () |> bind f))
    | Result res -> f res

type EffectBuilder() =
    member _.Return(v) = Result v
    member _.Bind(eff, f) = bind f eff

let effect = EffectBuilder()

let log str = Log(str, (fun () -> Result()))
let logf fmt = Printf.ksprintf log fmt

let hypotenuse a b =
    effect {
        do! logf "Side a: %A" a
        do! logf "Side b: %A" b
        let c = (a * a + b * b) |> sqrt
        do! logf "Side c: %A" c
        return c
    }

let q = hypotenuse 1. 2.

let c, logs = q |> handle

printf "%A" logs
