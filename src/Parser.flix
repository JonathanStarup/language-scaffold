use Token.{Fun, Arrow, Var, LParen, RParen}

enum Expr {
    case Lam(String, Expr)
    case App(Expr, Expr)
    case Var(String)
}

pub enum Token with Eq, ToString {
    case Fun
    case Arrow
    case Var(String)
    // case Let
    // case Eq
    // case In
    case LParen
    case RParen
}
enum Error {
    case Error
}

def parse(l: List[Token]): Option[Expr] = {
    l |> (parseExpr() <++ requireEmpty()) |> Option.map(match (expr, _) -> expr)
}

def parseExpr(): Parser[Expr] = {
    parseFun() ||| parseApp()
}

def parseFun(): Parser[Expr] = 
    require(Fun) ++> parseVarName() <++ require(Arrow) +++ parseExpr() >> map(match (var, expr) -> Expr.Lam(var, expr))

def parseApp(): Parser[Expr] = {
    parseElem() ||| ((parseApp() +++ parseExpr()) >> map(match (e1, e2) -> Expr.App(e1, e2)))
}

def parseElem(): Parser[Expr] = {
    (require(LParen) ++> parseExpr() <++ require(RParen)) ||| parseVar()
}

def parseVar(): Parser[Expr] = l -> match l {
    case Var(var) :: tl => Some((Expr.Var(var), tl))
    case _ => None
}

def parseVarName(): Parser[String] = l -> match l {
    case Var(var) :: tl => Some((var, tl))
    case _ => None
}



def require(t: Token): Parser[Unit] = l -> match l {
    case hd :: tl if hd == t => Some(((), tl))
    case _ => None
}

def requireEmpty(): Parser[Unit] = l -> match l {
    case Nil => Some((), Nil)
    case _ :: _ => None
}

def orElse(p2: Parser[a], p1: Parser[a]): Parser[a] = {
    l -> match p1(l) {
        case None => p2(l)
        case Some(v) => Some(v)
    }
}

def |||(p1: Parser[a], p2: Parser[a]): Parser[a] = orElse(p2, p1)

type alias Parser[a] = List[Token] -> Option[(a, List[Token])]

def andThen(p2: Parser[b], p1: Parser[a]): Parser[(a, b)] =
    l -> match p1(l) {
        case None => None
        case Some((e, l2)) => match p2(l2) {
            case None => None
            case Some((e2, l3)) => Some(((e, e2), l3))
        }
    }

def +++(p1: Parser[a], p2: Parser[b]): Parser[(a, b)] = andThen(p2, p1)

def ++>(p1: Parser[a], p2: Parser[b]): Parser[b] = (p1 +++ p2) >> map(match (_, x) -> x)
def <++(p1: Parser[a], p2: Parser[b]): Parser[a] = (p1 +++ p2) >> map(match (x, _) -> x)

def map(f: a -> b, o: Option[(a, List[Token])]): Option[(b, List[Token])] = {
    for (
        (x, l) <- o
    ) yield (f(x), l)
}