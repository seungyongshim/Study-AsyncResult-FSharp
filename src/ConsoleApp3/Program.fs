module Domain =
    type UserId = UserId of int
    type UserName = string
    type EmailAddress = EmailAddress of string

    type Profile =
        { UserId: UserId
          Name: UserName
          EmailAddress: EmailAddress }

    type EmailMessage = { To: EmailAddress; Body: string }

module Infrastructure =
    open Domain

    type ILogger =
        abstract Info: string -> unit
        abstract Error: string -> unit

    type InfrastructureError =
        | DbError of string
        | SmtpError of string

    type DbConntection = DbConntection of unit

    type IDbService =
        abstract NewDbConnection: unit -> DbConntection
        abstract QueryProfile: DbConntection -> UserId -> Async<Result<Profile, InfrastructureError>>
        abstract UpdateProfile: DbConntection -> Profile -> Async<Result<unit, InfrastructureError>>

    type SmtpCredentials = SmtpCredentials of unit

    type IEmailService =
        abstract SendChangeNotification: SmtpCredentials -> EmailMessage -> Async<Result<unit, InfrastructureError>>

type IInstruction<'a> =
    abstract member Map: ('a -> 'b) -> IInstruction<'b>



type Program<'a> =
    | Instruction of IInstruction<Program<'a>>
    | Stop of 'a

module Program =
    let rec bind f program =
        match program with
        | Instruction inst -> inst.Map(bind f) |> Instruction
        | Stop x -> f x

type ProgramBuilder() =
    member _.Return(x) = Stop x
    member _.Bind(x, f) = Program.bind f x
    member _.Zero() = Stop()

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

open Domain

type DbInstruction<'a> =
    | QueryProfile of UserId * next:(Profile -> 'a)
    | UpdateProfile of Profile * next:(unit -> 'a)
    interface IInstruction<'a> with



type Decision =
    | NoAction
    | UpdateProfileOnly of Profile
    | UpdateProfileAndNotify of Profile * EmailMessage

let updateCustomerProfile (newProfile: Profile) (currentProfile: Profile) =
    if currentProfile <> newProfile then program {
        do! logInfo("Updating Profile")
    }
    else program {
        return NoAction
    }


