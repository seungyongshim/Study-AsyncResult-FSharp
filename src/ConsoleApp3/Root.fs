// https://fsharpforfunandprofit.com/posts/dependencies-5/
// https://gist.github.com/swlaschin/ef1d180bfde18a9b876eb8f54913c49e

open ConsoleApp3.Domain
open ConsoleApp3.Infrastructure
open ConsoleApp3.Program1
open ConsoleApp3


type EmailInstruction<'a> =
    | SendChangeNotification of EmailMessage * next: (unit -> 'a)
    interface IInstruction<'a> with
        member this.Map f =
            match this with
            | SendChangeNotification (message, next) -> SendChangeNotification(message, next >> f)
            :> IInstruction<_>


let sendChangeNotification message =
    Instruction(SendChangeNotification(message, Stop))

type Decision =
    | NoAction
    | UpdateProfileOnly of Profile
    | UpdateProfileAndNotify of Profile * EmailMessage

let updateCustomerProfile (newProfile: Profile) (currentProfile: Profile) =
    if currentProfile <> newProfile then
        program { do! logInfo ("Updating Profile") }
    else
        program { return () }
