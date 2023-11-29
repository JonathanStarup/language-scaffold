mod Lexer.Examples {

    mod LetAbsApp {
        pub enum Token with Eq, ToString {
            case Fun
            case Arrow
            case Var(String)
            case Let
            case Eq
            case In
            case LParen
            case RParen
        }

        pub def matchers(): List[Lexer.TokenMatcher[Token]] = List#{
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

    mod Ski {
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
            (regex"\\)", _ -> Some(Token.RParen))
        }
    }

    mod Lc {
        pub enum Token with Eq, ToString {
            case Fun
            case Arrow
            case Var(String)
            case LParen
            case RParen
        }

        pub def matchers(): List[Lexer.TokenMatcher[Token]] = List#{
            (regex"fun\\b", _ -> Some(Token.Fun)),
            (regex"->", _ -> Some(Token.Arrow)),
            (regex"\\(", _ -> Some(Token.LParen)),
            (regex"\\)", _ -> Some(Token.RParen)),
            (regex"\\w+", s -> Some(Token.Var(s))),
            (regex"\\s", _ -> None)
        }
    }

}
