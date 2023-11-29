def main(): Unit \ IO = {
    // let input = "let f = fun x -> asdf x in (f f) f ";
    let input = "fun x -> asdf x";
    Lexer.lex(input, Lexer.Examples.Lc.matchers()) |> Result.map(parse) |> println
}
