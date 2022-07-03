namespace ConsoleApp3

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
