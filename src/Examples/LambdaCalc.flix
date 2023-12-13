mod Examples.LambdaCalc {

    enum Ast with ToString {
        case Lam(String, Ast)
        case App(Ast, Ast)
        case Var(String)
    }

    pub enum Token with Eq, ToString {
        case Fun
        case Arrow
        case Var(String)
        case LParen
        case RParen
    }

    pub def lex(s: String): Result[String, List[Token]] = Lexer.lex(s, Token.matchers())

    pub def parse(s: String): Result[String, Ast] = {
        for (
            tokens <- lex(s);
            ast <- Parser.RecursiveDecent.parse(tokens, Ast.parseAst)
        ) yield ast
    }

    mod Token {
        pub def matchers(): List[Lexer.TokenMatcher[Token]] = List#{
            (regex"fun\\b", _ -> Some(Fun)),
            (regex"->", _ -> Some(Arrow)),
            (regex"\\(", _ -> Some(LParen)),
            (regex"\\)", _ -> Some(RParen)),
            (regex"\\w+", s -> Some(Var(s))),
            (regex"\\s", _ -> None)
        }
    }

    mod Ast {
        use Examples.LambdaCalc.Token
        use Parser.RecursiveDecent.Parser
        use Parser.RecursiveDecent.{<++, eof, |||, +++, ++>, require, map}

        pub def parseAst(l: List[Token]): Option[(Ast, List[Token])] = l |> {
            parseFun ||| parseApp
        }

        def parseFun(l: List[Token]): Option[(Ast, List[Token])] = l |> {
            (((require(Token.Fun) ++> parseVarName) <++ require(Token.Arrow)) +++ parseAst) >> map(match (var, expr) -> Lam(var, expr))
        }

        // TODO: Broken, left-recursion
        def parseApp(l: List[Token]): Option[(Ast, List[Token])] = l |> {
            parseElem ||| ((parseApp +++ parseElem) >> map(match (e1, e2) -> App(e1, e2)))
        }

        def parseElem(l: List[Token]): Option[(Ast, List[Token])] = l |> {
            parseVar ||| ((require(Token.LParen) ++> parseAst) <++ require(Token.RParen))
        }

        def parseVar(l: List[Token]): Option[(Ast, List[Token])] = match l {
            case Token.Var(var) :: tl => Some((Var(var), tl))
            case _ => None
        }

        def parseVarName(l: List[Token]): Option[(String, List[Token])] = match l {
            case Token.Var(var) :: tl => Some((var, tl))
            case _ => None
        }

    }

}
