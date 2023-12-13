mod Examples.Arithmetic {

    pub enum Token with Eq, ToString {
        case Plus
        case Minus
        case Multiplication
        case Division
        case Exponentiation
        case LParen
        case RParen
        case Var(String)
    }

    pub enum Ast with ToString {
        case Plus(Ast, Ast)
        case Minus(Ast, Ast)
        case Neg(Ast)
        case Multiplication(Ast, Ast)
        case Division(Ast, Ast)
        case Exponentiation(Ast, Ast)
        case Var(String)
    }

    pub def lex(s: String): Result[String, List[Token]] = Lexer.lex(s, Token.matchers())

    pub def parse(s: String): Result[String, Ast] = {
        for (
            tokens <- lex(s);
            ast <- Parser.RecursiveDecent.parse(tokens, Ast.parseE)
        ) yield ast
    }

    mod Token {
        pub def matchers(): List[Lexer.TokenMatcher[Token]] = List#{
            (regex"\\+", _ -> Some(Plus)),
            (regex"-", _ -> Some(Minus)),
            (regex"\\*", _ -> Some(Multiplication)),
            (regex"/", _ -> Some(Division)),
            (regex"\\^", _ -> Some(Exponentiation)),
            (regex"\\(", _ -> Some(LParen)),
            (regex"\\)", _ -> Some(RParen)),
            (regex"\\w+", id -> Some(Var(id))),
            (regex"\\s", _ -> None)
        }
    }

    mod Ast {
        use Parser.RecursiveDecent.{Parser, ParserStream, ParserOutput}
        use Parser.RecursiveDecent.{<++, eof, |||, +++, ++>, require, map, parseOne, zeroOrMore, optional, requireWith}
        use Examples.Arithmetic.Token

        pub type alias Stream = ParserStream[Token]
        pub type alias Output[a] = ParserOutput[Token, a]

        // https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm

        // Grammer: Naive
        // E --> E "+" E
        //     | E "-" E
        //     | "-" E
        //     | E "*" E
        //     | E "/" E
        //     | E "^" E
        //     | "(" E ")"
        //     | v

        // Grammer: No left recursion
        // {...} means zero or more times
        // [...] means zero or one time
        // E --> T {( "+" | "-" ) T}
        // T --> F {( "*" | "/" ) F}
        // F --> P ["^" F]
        // P --> v | "(" E ")" | "-" T


        pub def parseE(s: Stream): Output[Ast] = s |> {
            parseT +++ zeroOrMore((parsePlus ||| parseMinus) +++ parseT >> map(t -> t ||> appRight)) >> map(t -> t ||> applyRighthands)
        }

        def parseT(s: Stream): Output[Ast] = s |> {
            parseF +++ zeroOrMore((parseMultiplication ||| parseDivision) +++ parseF >> map(t -> t ||> appRight)) >> map(t -> t ||> applyRighthands)
        }

        def parseF(s: Stream): Output[Ast] = s |> {
            parseP +++ (optional(parseExponentiation +++ parseF >> map(t -> t ||> appRight)) >> map(Option.toList)) >> map(t -> t ||> applyRighthands)
        }

        def parseP(s: Stream): Output[Ast] = s |> {
            parseV |||
            (require(Token.LParen) ++> parseE <++ require(Token.RParen)) |||
            (parseNeg +++ parseP >> map(match (op, right) -> op(right)))
        }

        def parseV(s: Stream): Output[Ast] = s |> {
            parseOne(t -> match t {
                case Token.Var(x) => Some(Var(x))
                case _ => None
            })
        }

        // operation parsing

        def parsePlus(s: Stream): Output[(Ast, Ast) -> Ast] = s |> {
            requireWith(Token.Plus, curry2(Plus))
        }

        def parseMinus(s: Stream): Output[(Ast, Ast) -> Ast] = s |> {
            requireWith(Token.Minus, curry2(Minus))
        }

        def parseMultiplication(s: Stream): Output[(Ast, Ast) -> Ast] = s |> {
            requireWith(Token.Multiplication, curry2(Multiplication))
        }

        def parseDivision(s: Stream): Output[(Ast, Ast) -> Ast] = s |> {
            requireWith(Token.Division, curry2(Division))
        }

        def parseExponentiation(s: Stream): Output[(Ast, Ast) -> Ast] = s |> {
            requireWith(Token.Exponentiation, curry2(Exponentiation))
        }

        def parseNeg(s: Stream): Output[Ast -> Ast] = s |> {
            requireWith(Token.Minus, Neg)
        }

        // Helpers to build Ast terms

        def applyRighthands(left: Ast, rights: List[Ast -> Ast]): Ast =
            (left, rights) ||> List.foldLeft((acc, righthand) -> righthand(acc))

        def appRight(cons: (Ast, Ast) -> Ast, right: Ast): Ast -> Ast =
            left -> cons(left, right)

        def curry2(f: ((a, b)) -> c): a -> b -> c = x -> y -> f((x, y))

    }

}
