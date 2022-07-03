namespace ConsoleApp3

type IInstruction<'a> =
    abstract member Map: ('a -> 'b) -> IInstruction<'b>
