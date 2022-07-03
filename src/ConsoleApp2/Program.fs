type Reader<'env, 'a> = Reader of action: ('env -> 'a)

module Reader =
    let run env (Reader action) = action env
    let ask = Reader id
    let map f reader = Reader(fun env -> f (run env reader))

    let bind f reader =
        let newAction env =
            let x = run env reader
            run env (f x)

        Reader newAction

type ReaderBuilder() =
    member __.Return(x) = Reader(fun _ -> x)
    member __.Bind(x, f) = Reader.bind f x

let compareTwoStrings str1 str2 =
    reader { fun (logger: ILogger) -> logger.LogDebug "" }


let a = [ 1 ] :> seq<int>
