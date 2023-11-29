mod Test.TestLexer {
    use Lexer.lex;
    use Examples;

    @test
    pub def skiExample(): Bool = {
        use Examples.Ski.Token.{S, K, I, LParen => L, RParen => R};
        let term = "(S(KI)SKI(SKIS))KSI";
        let expected = Ok(List#{L, S, L, K, I, R, S, K, I, L, S, K, I, S, R, R, K, S, I});
        let actual = lex(term, Examples.Ski.matchers());
        Assert.eq(expected, actual)
    }

    @test
    pub def letAbsAppExample(): Bool = {
        use Examples.LetAbsApp.Token.{Fun, Arrow, Var, Let, Eq, In, LParen => L, RParen => R};
        let term = "let twice = fun f x -> f (f x) in twice (add one) one";
        let expected = Ok(List#{
            Let, Var("twice"), Eq, Fun, Var("f"), Var("x"), Arrow, Var("f"), L,
            Var("f"), Var("x"), R, In, Var("twice"), L, Var("add"), Var("one"),
            R, Var("one")
        });
        let actual = lex(term, Examples.LetAbsApp.matchers());
        Assert.eq(expected, actual)
    }

}
