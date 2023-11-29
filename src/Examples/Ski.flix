mod Examples.Ski {

    pub enum Token with Eq, ToString {
        case S
        case K
        case I
        case LParen
        case RParen
    }

    pub def matchers(): List[Lexer.TokenMatcher[Token]] = List#{
        (regex"S", _ -> Some(Token.S)),
        (regex"K", _ -> Some(Token.K)),
        (regex"I", _ -> Some(Token.I)),
        (regex"\\(", _ -> Some(Token.LParen)),
        (regex"\\)", _ -> Some(Token.RParen)),
        (regex"\\s", _ -> None)
    }

}
