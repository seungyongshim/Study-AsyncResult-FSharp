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



