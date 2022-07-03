namespace ConsoleApp3

open Domain

type DbInstruction<'a> =
    | QueryProfile of UserId * next: (Profile -> 'a)
    | UpdateProfile of Profile * next: (unit -> 'a)
    interface IInstruction<'a> with
        member this.Map f =
            match this with
            | QueryProfile (userId, next) -> QueryProfile(userId, next >> f)
            | UpdateProfile (userId, next) -> UpdateProfile(userId, next >> f)
            :> IInstruction<_>

[<AutoOpen>]
module DbCE =
    let queryProfile userId = Instruction(QueryProfile(userId, Stop))

    let updateProfile profile =
        Instruction(UpdateProfile(profile, Stop))
