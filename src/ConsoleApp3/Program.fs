open ConsoleApp3.Domain
open ConsoleApp3.Infrastructure
open ConsoleApp3.Program1
open ConsoleApp3

let program = ProgramBuilder()

type LoggerInstruction<'a> =
    | LogInfo of string * next: (unit -> 'a)
    | LogError of string * next: (unit -> 'a)
    interface IInstruction<'a> with
        member this.Map f =
            match this with
            | LogInfo (str, next) -> LogInfo(str, next >> f)
            | LogError (str, next) -> LogError(str, next >> f)
            :> IInstruction<_>

let logInfo str = Instruction(LogInfo(str, Stop))
let logError str = Instruction(LogError(str, Stop))

type DbInstruction<'a> =
    | QueryProfile of UserId * next: (Profile -> 'a)
    | UpdateProfile of Profile * next: (unit -> 'a)
    interface IInstruction<'a> with

type Decision =
    | NoAction
    | UpdateProfileOnly of Profile
    | UpdateProfileAndNotify of Profile * EmailMessage

let updateCustomerProfile (newProfile: Profile) (currentProfile: Profile) =
    if currentProfile <> newProfile then
        program { do! logInfo ("Updating Profile") }
    else
        program { return NoAction }
