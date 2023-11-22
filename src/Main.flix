def main(): Unit \ IO = {
    let input = "let f = fun x -> asdf x in (f f) f ";
    Lexer.lex(input, Lexer.Examples.LetAbsApp.matchers()) |> println
}
