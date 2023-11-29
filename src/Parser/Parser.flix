mod Parser {
    pub type alias ParserStream[token] = List[token]
    pub type alias ParserOutput[token, a] = Option[(a, ParserStream[token])]
    pub type alias Parser[token, a] = ParserStream[token] -> ParserOutput[token, a]

    pub def parse(s: ParserStream[token], p: Parser[token, a]): Result[String, a] = {
        p(s) |> Option.map(fst) |> Option.toOk("Failed Parsing")
    }

    pub def require(t: token, l: ParserStream[token]): ParserOutput[token, Unit] with Eq[token] = l |> {
        parseOne(Eq.eq(t) >> (b -> if (b) Some(()) else None))
    }

    pub def requireWith(t: token, x: a, l: ParserStream[token]): ParserOutput[token, a] with Eq[token] = l |> {
        require(t) >> map(_ -> x)
    }

    pub def parseOne(f: token -> Option[a], l: ParserStream[token]): ParserOutput[token, a] = match l {
        case hd :: tl => match f(hd) {
            case Some(v) => Some(v, tl)
            case None => None
        }
        case Nil => None
    }

    pub def eof(l: ParserStream[token]): ParserOutput[token, Unit] = match l {
        case Nil => Some((), Nil)
        case _ :: _ => None
    }

    pub def orElse(p2: Parser[token, a], p1: Parser[token, a]): Parser[token, a] = {
        l -> match p1(l) {
            case None => p2(l)
            case Some(v) => Some(v)
        }
    }

    pub def |||(p1: Parser[token, a], p2: Parser[token, a]): Parser[token, a] = orElse(p2, p1)

    pub def andThen(p2: Parser[token, b], p1: Parser[token, a], l: ParserStream[token]): ParserOutput[token, (a, b)] =
        match p1(l) {
            case None => None
            case Some((e, l2)) => match p2(l2) {
                case None => None
                case Some((e2, l3)) => Some(((e, e2), l3))
            }
        }

    pub def +++(p1: Parser[token, a], p2: Parser[token, b], l: ParserStream[token]): ParserOutput[token, (a, b)] =
        andThen(p2, p1, l)

    pub def ++>(p1: Parser[token, a], p2: Parser[token, b], l: ParserStream[token]): ParserOutput[token, b] =
        l |> ((p1 +++ p2) >> map(match (_, x) -> x))

    pub def <++(p1: Parser[token, a], p2: Parser[token, b], l: ParserStream[token]): ParserOutput[token, a] =
        l |> ((p1 +++ p2) >> map(match (x, _) -> x))

    pub def map(f: a -> b, o: Option[(a, ParserStream[token])]): ParserOutput[token, b] = {
        for (
            (x, l) <- o
        ) yield (f(x), l)
    }

    pub def success(x: a, l: ParserStream[token]): ParserOutput[token, a] = Some((x, l))

    pub def zeroOrMore(p: Parser[token, a], l: ParserStream[token]): ParserOutput[token, List[a]] =
        zeroOrMoreAux(Nil, p, l)

    def zeroOrMoreAux(acc: List[a], p: Parser[token, a], l: ParserStream[token]): ParserOutput[token, List[a]] = match p(l) {
        case None => Some(acc |> List.reverse, l)
        case Some(x, tl) => zeroOrMoreAux(x :: acc, p, tl)
    }

    pub def optional(p: Parser[token, a], l: ParserStream[token]): ParserOutput[token, Option[a]] = l |> {
        (p >> map(x -> Some(x))) ||| success(None)
    }
}
