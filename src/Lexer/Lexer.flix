mod Lexer {

    /// A pair of a regex pattern, describing the token syntactically, and
    /// a function that receives the string matched, and optionally returns a
    /// token that should be produced.
    pub type alias TokenMatcher[token] = (Regex, String -> Option[token])

    /// Returns a list of tokens according to `matchers`.
    /// Returns `Err` if a running suffix cannot be matched by any token matcher.
    /// Note that EOF is handled by internally and cannot be produced by a matcher.
    pub def lex(input: String, matchers: List[TokenMatcher[token]]): Result[String, List[token]] with Eq[token] = {
        if (Regex.isMatch(regex"\\z", input)) Ok(Nil) else
        for (
            (token, suffix) <- lexOne(input, matchers);
            tokens <- lex(suffix, matchers)
        ) yield Option.toList(token) ::: tokens
    }

    /// Returns the output of the first matcher that can match a prefix of
    /// `input` along with the respective suffix.
    /// Returns `Err` if no matcher can accept a prefix.
    def lexOne(input: String, matchers: List[TokenMatcher[token]]): Result[String, (Option[token], String)] = {
        let matcher = match (r, tokenizer) -> {
            for (
                (m, suffix) <- matchFirst(input, r)
            ) yield (tokenizer(m), suffix)
        };
        matchers |> List.findMap(matcher) |> Option.toOk(input)
    }

    /// Returns (prefix, suffix) if `r` matches some prefix of `input`.
    def matchFirst(input: String, r: Regex): Option[(String, String)] = {
        for (
            prefix <- Regex.getPrefix(substr = r, input);
            suffix <- Regex.stripPrefix(substr = r, input)
        ) yield (prefix, suffix)
    }
}
