use std::collections::HashMap;

use chumsky::prelude::*;
use thiserror::Error;
use unescape::unescape;

use crate::rule::{Atom, Expr, Predicate, RepeatRange, RepeatRangeEnd, Var};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    /// `<-`
    LeftArrow,
    /// `:`
    Colon,
    /// `;`
    Semicolon,
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `/`
    Slash,
    /// `'hello'`
    String(String),
    /// `hello`
    Ident(String),
    /// - e.g.: `0..9`
    /// - e.g.: `0..`
    Range(RepeatRange),
    /// `.`
    Dot,
    /// `?`
    Question,
    /// `+`
    Plus,
    /// `*`
    Asterisk,
    /// `$`
    Dollar,
    /// `!`
    Not,
    /// `&`
    And,
    End,
}

impl Token {
    pub fn into_ident(self) -> Option<String> {
        match self {
            Token::Ident(s) => Some(s),
            _ => None,
        }
    }

    pub fn into_string(self) -> Option<String> {
        match self {
            Token::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn into_range(self) -> Option<RepeatRange> {
        match self {
            Token::Range(r) => Some(r),
            _ => None,
        }
    }
}

fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let left_arrow = just("<-").map(|_| Token::LeftArrow);
    let colon = just(':').map(|_| Token::Colon);
    let semicolon = just(';').map(|_| Token::Semicolon);
    let l_paren = just('(').map(|_| Token::LParen);
    let r_paren = just(')').map(|_| Token::RParen);
    let slash = just('/').map(|_| Token::Slash);

    // String
    let not_quote_1 = just('\'').not();
    let not_quote_2 = just('"').not();
    // escape_one <- '\' .
    let escape_one = just('\\')
        .then(any())
        .map(|(_, c): (char, char)| format!("\\{}", c));
    // escaped_1 <- ( escape_one / not_quote_1 )*
    let escaped_1 = choice((escape_one, not_quote_1.map(|c| c.to_string())))
        .repeated()
        .map(|strings: Vec<String>| {
            let str = strings.iter().fold(String::new(), |acc, x| acc + x);
            unescape(&str).unwrap()
        });
    // escaped_2 <- ( escape_one / not_quote_2 )*
    let escaped_2 = choice((escape_one, not_quote_2.map(|c| c.to_string())))
        .repeated()
        .map(|strings: Vec<String>| {
            let str = strings.iter().fold(String::new(), |acc, x| acc + x);
            unescape(&str).unwrap()
        });
    // string_1 <- '\'' escaped_1 '\''
    let string_1 = escaped_1
        .delimited_by(just('\''), just('\''))
        .map(Token::String);
    // string_2 <- '"' escaped_2 '"'
    let string_2 = escaped_2
        .delimited_by(just('"'), just('"'))
        .map(Token::String);
    // string <- string_1 / string_2
    let string = choice((string_1, string_2));

    let ident = text::ident().map(Token::Ident);
    let range = text::digits(10)
        .then(just(".."))
        .then(text::digits(10).or_not())
        .map(|((start, _), end): ((String, &str), Option<String>)| {
            let start: usize = start.parse().unwrap();
            let end = match end {
                Some(end) => RepeatRangeEnd::Finite(end.parse().unwrap()),
                None => RepeatRangeEnd::Infinite,
            };
            Token::Range(RepeatRange::new(start, end))
        });
    let dot = just('.').map(|_| Token::Dot);
    let question = just('?').map(|_| Token::Question);
    let plus = just('+').map(|_| Token::Plus);
    let asterisk = just('*').map(|_| Token::Asterisk);
    let dollar = just('$').map(|_| Token::Dollar);
    let not = just('!').map(|_| Token::Not);
    let and = just('&').map(|_| Token::And);
    let token_end = end().map(|_| Token::End);

    // Padding
    let comment_head = choice((just('#').ignored(), just("//").ignored()));
    let comment = comment_head
        .then_ignore(
            choice((just('\n').ignored(), end().ignored()))
                .not()
                .repeated(),
        )
        .then_ignore(choice((just('\n').ignored(), end().ignored())));
    let whitespace = filter(|c: &char| c.is_whitespace()).ignored();
    // pad <- (whitespace / comment)*
    let pad = choice((whitespace, comment)).repeated();

    // token <- left_arrow/ colon / semicolon / l_paren / r_paren / slash / string / ident / range / dot / question / plus / asterisk / dollar / not / and
    let token = choice((
        left_arrow, colon, semicolon, l_paren, r_paren, slash, string, ident, range, dot, question,
        plus, asterisk, dollar, not, and,
    ))
    .padded_by(pad.clone());

    // tokens <- token* end
    token.repeated().then(token_end.padded_by(pad.clone())).map(
        |(mut tokens, end): (Vec<Token>, Token)| {
            tokens.push(end);
            tokens
        },
    )
}

fn parser() -> impl Parser<Token, HashMap<Var, Expr>, Error = Simple<Token>> {
    // empty <- "empty"
    let empty = just(Token::Ident("empty".to_string())).map(|_| Expr::Sequence(vec![]));
    // var <- ident
    let var = filter(|t: &Token| matches!(t, Token::Ident(_)))
        .map(|t: Token| Var(t.into_ident().unwrap()));

    let expr = recursive(|expr| {
        // atom <- var / string / dollar
        let atom = choice((
            var.map(Atom::Var),
            filter(|t: &Token| matches!(t, Token::String(_)))
                .map(|t: Token| Atom::Literal(t.into_string().unwrap())),
            just(Token::Dollar).map(|_| Atom::End),
        ))
        .map(|atom: Atom| Expr::Atom {
            inner: atom,
            index: 0,
        });
        // expr_atom <- empty / atom / l_paren expr r_paren
        let expr_atom = choice((
            empty,
            atom,
            expr.delimited_by(just(Token::LParen), just(Token::RParen)),
        ));
        // repeat <- expr_atom (question / plus / asterisk / range)?
        let repeat = expr_atom
            .then(
                choice((
                    just(Token::Question).map(|_| RepeatRange::new(0, RepeatRangeEnd::Finite(1))),
                    just(Token::Plus).map(|_| RepeatRange::new(1, RepeatRangeEnd::Infinite)),
                    just(Token::Asterisk).map(|_| RepeatRange::new(0, RepeatRangeEnd::Infinite)),
                    filter(|t: &Token| matches!(t, Token::Range(_)))
                        .map(|t: Token| t.into_range().unwrap()),
                ))
                .or_not(),
            )
            .map(|(atom, range)| match range {
                Some(range) => Expr::Repeat {
                    inner: atom.into(),
                    range,
                },
                None => atom,
            });
        // predicate <- (not / and)? repeat
        let predicate = choice((
            just(Token::Not).map(|_| Predicate::Not),
            just(Token::And).map(|_| Predicate::Lookahead),
        ))
        .or_not()
        .then(repeat)
        .map(|(predicate, repeat)| match predicate {
            Some(predicate) => Expr::Predicate {
                inner: repeat.into(),
                predicate,
            },
            None => repeat,
        });
        // sequence <- predicate+
        let sequence = predicate.repeated().at_least(1).map(|predicates| {
            if predicates.len() == 1 {
                predicates.into_iter().next().unwrap()
            } else {
                Expr::Sequence(predicates)
            }
        });
        // choice <- (sequence slash)* sequence
        let choice = sequence
            .clone()
            .then_ignore(just(Token::Slash))
            .repeated()
            .then(sequence.clone())
            .map(|(mut exprs, last): (Vec<Expr>, Expr)| {
                if exprs.is_empty() {
                    return last;
                }
                exprs.push(last);
                Expr::Choice(exprs)
            });
        // expr <- choice
        #[allow(clippy::let_and_return)]
        choice
    });
    // rule <- var (left_arrow / colon) expr semicolon
    let rule = var
        .then_ignore(choice((just(Token::LeftArrow), just(Token::Colon))))
        .then(expr)
        .then_ignore(just(Token::Semicolon));
    // rules <- rule* $
    let rules = rule.repeated().then_ignore(just(Token::End)).map(|rules| {
        let mut map = HashMap::new();
        for (var, mut expr) in rules {
            expr.set_atom_indices(0);
            map.insert(var, expr);
        }
        map
    });
    #[allow(clippy::let_and_return)]
    rules
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum Error {
    #[error("parse error")]
    Parser(Vec<Simple<Token>>),
    #[error("lexer error")]
    Lexer(Vec<Simple<char>>),
}

pub fn parse(src: &str) -> Result<HashMap<Var, Expr>, Error> {
    let tokens = match lexer().parse(src) {
        Ok(tokens) => tokens,
        Err(e) => return Err(Error::Lexer(e)),
    };
    match parser().parse(tokens) {
        Ok(rules) => Ok(rules),
        Err(e) => Err(Error::Parser(e)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string() {
        let input = "'hello \\'world\\''
        \"hello \\\"world\\\"\"
        ";
        let tokens = lexer().parse(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::String("hello 'world'".into()),
                Token::String("hello \"world\"".into()),
                Token::End
            ]
        );
    }

    #[test]
    fn ident() {
        let input = "hello_world
        hello_world123
        _hello_world
        ";
        let tokens = lexer().parse(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Ident("hello_world".into()),
                Token::Ident("hello_world123".into()),
                Token::Ident("_hello_world".into()),
                Token::End
            ]
        );
    }

    #[test]
    fn range() {
        let input = "1..2
        1..
        ";
        let tokens = lexer().parse(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Range(RepeatRange::new(1, RepeatRangeEnd::Finite(2))),
                Token::Range(RepeatRange::new(1, RepeatRangeEnd::Infinite)),
                Token::End
            ]
        );
    }

    #[test]
    fn empty() {
        let input = "";
        let tokens = lexer().parse(input).unwrap();
        assert_eq!(tokens, vec![Token::End]);
    }

    #[test]
    fn comment() {
        let input = "# hello world
        // hello world
        ";
        let tokens = lexer().parse(input).unwrap();
        assert_eq!(tokens, vec![Token::End]);
    }

    #[test]
    fn rule() {
        let input = "
        S <- A / B / C / D / E ;
        A <- 'a' $ ;
        B <- 'b' ;
        C <- 'c_' 'em' empty 'pty' ;
        D <- 'd' '_repeat'+ ;
        E <- 'e_' !'foobar' 'bar' ;
        ";
        let rules = parse(input).unwrap();
        let mut rules_expected = HashMap::new();
        rules_expected.insert(
            Var("S".into()),
            Expr::Choice(vec![
                Expr::Atom {
                    inner: Atom::Var(Var("A".into())),
                    index: 0,
                },
                Expr::Atom {
                    inner: Atom::Var(Var("B".into())),
                    index: 1,
                },
                Expr::Atom {
                    inner: Atom::Var(Var("C".into())),
                    index: 2,
                },
                Expr::Atom {
                    inner: Atom::Var(Var("D".into())),
                    index: 3,
                },
                Expr::Atom {
                    inner: Atom::Var(Var("E".into())),
                    index: 4,
                },
            ]),
        );
        rules_expected.insert(
            Var("A".into()),
            Expr::Sequence(vec![
                Expr::Atom {
                    inner: Atom::Literal("a".into()),
                    index: 0,
                },
                Expr::Atom {
                    inner: Atom::End,
                    index: 1,
                },
            ]),
        );
        rules_expected.insert(
            Var("B".into()),
            Expr::Atom {
                inner: Atom::Literal("b".into()),
                index: 0,
            },
        );
        rules_expected.insert(
            Var("C".into()),
            Expr::Sequence(vec![
                Expr::Atom {
                    inner: Atom::Literal("c_".into()),
                    index: 0,
                },
                Expr::Atom {
                    inner: Atom::Literal("em".into()),
                    index: 1,
                },
                Expr::Sequence(vec![]),
                Expr::Atom {
                    inner: Atom::Literal("pty".into()),
                    index: 2,
                },
            ]),
        );
        rules_expected.insert(
            Var("D".into()),
            Expr::Sequence(vec![
                Expr::Atom {
                    inner: Atom::Literal("d".into()),
                    index: 0,
                },
                Expr::Repeat {
                    inner: Expr::Atom {
                        inner: Atom::Literal("_repeat".into()),
                        index: 1,
                    }
                    .into(),
                    range: RepeatRange::new(1, RepeatRangeEnd::Infinite),
                },
            ]),
        );
        rules_expected.insert(
            Var("E".into()),
            Expr::Sequence(vec![
                Expr::Atom {
                    inner: Atom::Literal("e_".into()),
                    index: 0,
                },
                Expr::Predicate {
                    inner: Expr::Atom {
                        inner: Atom::Literal("foobar".into()),
                        index: 1,
                    }
                    .into(),
                    predicate: Predicate::Not,
                },
                Expr::Atom {
                    inner: Atom::Literal("bar".into()),
                    index: 2,
                },
            ]),
        );
        let var = Var("S".into());
        assert_eq!(rules.get(&var), rules_expected.get(&var));
        let var = Var("A".into());
        assert_eq!(rules.get(&var), rules_expected.get(&var));
        let var = Var("B".into());
        assert_eq!(rules.get(&var), rules_expected.get(&var));
        let var = Var("C".into());
        assert_eq!(rules.get(&var), rules_expected.get(&var));
        let var = Var("D".into());
        assert_eq!(rules.get(&var), rules_expected.get(&var));
        assert_eq!(rules, rules_expected);
    }
}
