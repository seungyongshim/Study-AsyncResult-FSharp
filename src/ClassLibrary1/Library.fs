namespace ClassLibrary1

type Result<'a> = Result<'a, exn>
type AsyncResult<'a> = Async<Result<'a>>



module Result =
    let ofChoice value =
        match value with
        | Choice1Of2 value -> Ok value
        | Choice2Of2 e -> Error e

module AsyncResult =
    let handler (operation:Async<'a>) : AsyncResult<'a> =
        async {
            let! result = Async.Catch operation
            return result |> Result.ofChoice
        }

    let retn (value:'a) : AsyncResult<'a> =
        value |> Ok |> async.Return

    let map (selector : 'a -> Async<'b>) (asyncResult : AsyncResult<'a> ) : AsyncResult<'b> =
        async {
            let! result = asyncResult
            match result with
            | Ok x -> return! selector x |> handler
            | Error err -> return Error err
        }

    let bind (selector : 'a -> AsyncResult<'b>) (asyncResult : AsyncResult<'a>) : AsyncResult<'b> =
        async {
            let! result = asyncResult
            match result with
            | Ok x -> return! selector x
            | Error err -> return Error err
        }

    let bimap success failure operation =
        async  {
            let! result = operation
            match result with
            | Ok v -> return! success v |> handler
            | Error err -> return! failure err |> handler
        }

    //let map (selector : 'a -> 'b) (asyncResult : AsyncResult<'a>) =
    //    asyncResult |> Async.map (Result.map selector)


type AsyncResultBuilder () =
    member _.Return m = AsyncResult.retn m
    member _.Bind (m, f) = AsyncResult.bind f m
    member _.ReturnFrom m = m

