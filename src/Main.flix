//

enum Token with ToString, Eq {
    case Fun
    case Arrow
    case Var(String)
    case Let
    case Eq
    case In
    case LParen
    case RParen
    case EOF
}

enum LexError with ToString, Eq {
    case LexError
}

type alias LexToken[token] = (Regex, String -> Option[token])

def lambdaTokens(): List[LexToken[Token]] = List#{
    (regex"fun\\b", _ -> Some(Token.Fun)),
    (regex"->", _ -> Some(Token.Arrow)),
    (regex"let\\b", _ -> Some(Token.Let)),
    (regex"=", _ -> Some(Token.Eq)),
    (regex"in\\b", _ -> Some(Token.In)),
    (regex"\\(", _ -> Some(Token.LParen)),
    (regex"\\)", _ -> Some(Token.RParen)),
    (regex"\\w+", s -> Some(Token.Var(s))),
    (regex"\\s", _ -> None),
    (regex"\\z", _ -> Some(Token.EOF))
}

def lexOne(input: String, lexTokens: List[LexToken[token]]): Result[LexError, (Option[token], String)] = {
    let matcher = match (r, tokenizer) -> {
        for (
            (m, suffix) <- matchFirst(input, r)
        ) yield (tokenizer(m), suffix)
    };
    lexTokens |> List.findMap(matcher) |> Option.toOk(LexError.LexError)
}

def lex(input: String, lexTokens: List[LexToken[token]], eof: token): Result[LexError, List[token]] with Eq[token] = {
    lexAux(input, List.append(lexTokens, List#{(regex"\\z", _ -> Some(eof))}), eof)
}

def lexAux(input: String, lexTokens: List[LexToken[token]], eof: token): Result[LexError, List[token]] with Eq[token] = {
    for (
        (token, suffix) <- lexOne(input, lexTokens);
        tokens <- if (token == Some(eof)) Ok(Nil) else lex(suffix, lexTokens, eof)
    ) yield Option.toList(token) ::: tokens
}


/// Returns (match, suffix) if matched
def matchFirst(input: String, r: Regex): Option[(String, String)] = {
    for (
        prefix <- Regex.getPrefix(substr = r, input);
        suffix <- Regex.stripPrefix(substr = r, input)
    ) yield (prefix, suffix)
}

def main(): Unit \ IO = {
    let input = "let f = fun x -> asdf x in (f f) f";
    lex(input, lambdaTokens(), Token.EOF) |> println
}
