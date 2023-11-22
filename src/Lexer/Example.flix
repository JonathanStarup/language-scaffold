mod Lexer.Example {
    pub enum Token with ToString, Eq {
        case Fun
        case Arrow
        case Var(String)
        case Let
        case Eq
        case In
        case LParen
        case RParen
    }

    pub def letAbsAppTokens(): List[Lexer.TokenMatcher[Token]] = List#{
        (regex"fun\\b", _ -> Some(Token.Fun)),
        (regex"->", _ -> Some(Token.Arrow)),
        (regex"let\\b", _ -> Some(Token.Let)),
        (regex"=", _ -> Some(Token.Eq)),
        (regex"in\\b", _ -> Some(Token.In)),
        (regex"\\(", _ -> Some(Token.LParen)),
        (regex"\\)", _ -> Some(Token.RParen)),
        (regex"\\w+", s -> Some(Token.Var(s))),
        (regex"\\s", _ -> None)
    }
}
