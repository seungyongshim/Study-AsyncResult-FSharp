namespace ConsoleApp3

type Program<'a> =
    | Instruction of IInstruction<Program<'a>>
    | Stop of 'a

module Program1 =
    let rec bind f program =
        match program with
        | Instruction inst -> inst.Map(bind f) |> Instruction
        | Stop x -> f x


type ProgramBuilder() =
    member _.Return(x) = Stop x
    member _.Bind(x, f) = Program1.bind f x
    member _.Zero() = Stop()

[<AutoOpen>]
module ProgramCE =
    let program = ProgramBuilder()
