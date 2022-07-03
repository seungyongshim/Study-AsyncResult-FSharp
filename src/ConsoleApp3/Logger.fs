namespace ConsoleApp3

type LoggerInstruction<'a> =
    | LogInfo of string * next: (unit -> 'a)
    | LogError of string * next: (unit -> 'a)
    interface IInstruction<'a> with
        member this.Map f =
            match this with
            | LogInfo (str, next) -> LogInfo(str, next >> f)
            | LogError (str, next) -> LogError(str, next >> f)
            :> IInstruction<_>

[<AutoOpen>]
module LoggerCE = 
    let logInfo str = Instruction(LogInfo(str, Stop))
    let logError str = Instruction(LogError(str, Stop))
