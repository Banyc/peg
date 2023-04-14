use std::collections::HashMap;

use tracing::trace;

/// Non-terminal
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var(pub String);

pub struct RuleSet {
    rules: HashMap<Var, Rule>,
    start: Var,
}

impl RuleSet {
    pub fn new(rules: HashMap<Var, Rule>, start: Var) -> Self {
        Self { rules, start }
    }

    pub fn rule(&self, var: &Var) -> Option<&Rule> {
        self.rules.get(var)
    }

    pub fn start(&self) -> &Var {
        &self.start
    }

    pub fn eval(&self, src: &[char]) -> (usize, EvalResult, Vec<Match>) {
        let rule = self.rule(&self.start).unwrap();
        let mut ctx = EvalContext {
            src_pos: 0,
            src,
            rule_set: self,
            rule: self.start.clone(),
            matches: Default::default(),
        };
        let eval = rule.eval(&mut ctx);
        (ctx.src_pos, eval, ctx.matches)
    }
}

pub struct Rule(pub Expr);

impl Rule {
    pub fn eval(&self, ctx: &mut EvalContext) -> EvalResult {
        self.0.eval(ctx)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RepeatRange {
    start: usize,
    end: RepeatRangeEnd,
}

impl RepeatRange {
    pub fn new(start: usize, end: RepeatRangeEnd) -> Self {
        if let RepeatRangeEnd::Finite(end) = end {
            assert!(start <= end);
        }
        Self { start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RepeatRangeEnd {
    Finite(usize),
    Infinite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Predicate {
    /// And-predicate
    Lookahead,
    /// Not-predicate
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Choice(Vec<Expr>),
    Atom {
        index: usize,
        inner: Atom,
    },
    Sequence(Vec<Expr>),
    Predicate {
        predicate: Predicate,
        inner: Box<Expr>,
    },
    Repeat {
        range: RepeatRange,
        inner: Box<Expr>,
    },
}

impl Expr {
    pub fn eval(&self, ctx: &mut EvalContext) -> EvalResult {
        match self {
            Expr::Atom { index, inner } => {
                let src_pos = ctx.src_pos;
                let eval = inner.eval(ctx);

                if eval == EvalResult::Matched {
                    // Save the match
                    ctx.matches.push(Match {
                        rule_pos: RulePosition {
                            var: ctx.rule.clone(),
                            index: *index,
                        },
                        src_pos: src_pos..ctx.src_pos,
                    });
                }

                eval
            }
            Expr::Choice(choice) => {
                let src_pos = ctx.src_pos;
                for expr in choice {
                    let eval = expr.eval(ctx);
                    if eval == EvalResult::Matched {
                        return EvalResult::Matched;
                    }
                    ctx.src_pos = src_pos;
                }
                EvalResult::NotMatched
            }
            Expr::Sequence(sequence) => {
                for expr in sequence {
                    let eval = expr.eval(ctx);
                    if eval == EvalResult::NotMatched {
                        return EvalResult::NotMatched;
                    }
                }
                EvalResult::Matched
            }
            Expr::Predicate { predicate, inner } => {
                let src_pos = ctx.src_pos;
                let eval = inner.eval(ctx);
                ctx.src_pos = src_pos;
                match predicate {
                    Predicate::Lookahead => eval,
                    Predicate::Not => match eval {
                        EvalResult::Matched => EvalResult::NotMatched,
                        EvalResult::NotMatched => EvalResult::Matched,
                    },
                }
            }
            Expr::Repeat { range, inner } => {
                trace!("eval: {:?}", self);
                let mut repeat = 0;
                let mut eval = loop {
                    trace!("repeat: {}", repeat);
                    if let RepeatRangeEnd::Finite(end) = range.end {
                        if repeat == end {
                            trace!("repeat == end");
                            break EvalResult::Matched;
                        }
                        assert!(repeat < end);
                    }
                    let eval = inner.eval(ctx);
                    if eval == EvalResult::NotMatched {
                        match range.end {
                            RepeatRangeEnd::Finite(_) => {
                                trace!("not matched before finite");
                                break EvalResult::NotMatched;
                            }
                            RepeatRangeEnd::Infinite => {
                                trace!("repeat ends before infinite");
                                break EvalResult::Matched;
                            }
                        }
                    }
                    repeat += 1;
                };
                if repeat < range.start {
                    trace!("repeat < start");
                    eval = EvalResult::NotMatched;
                }
                eval
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Atom {
    Var(Var),
    /// Terminal
    Literal(String),
    /// `$`
    End,
}

impl Atom {
    pub fn eval(&self, ctx: &mut EvalContext) -> EvalResult {
        match self {
            Atom::Var(name) => {
                let rule_set = ctx.rule_set.rule(name).unwrap();
                let rule = ctx.rule.clone();
                ctx.rule = name.clone();
                let eval = rule_set.eval(ctx);

                // Restore the rule position
                ctx.rule = rule;

                eval
            }
            Atom::Literal(value) => {
                if ctx.src[ctx.src_pos..].starts_with(value.chars().collect::<Vec<_>>().as_slice())
                {
                    // Consume the input
                    let end = ctx.src_pos + value.len();
                    ctx.src_pos = end;

                    EvalResult::Matched
                } else {
                    EvalResult::NotMatched
                }
            }
            Atom::End => {
                if ctx.src.len() == ctx.src_pos {
                    EvalResult::Matched
                } else {
                    EvalResult::NotMatched
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalResult {
    Matched,
    NotMatched,
}

pub struct EvalContext<'src, 'rule> {
    pub src: &'src [char],
    pub src_pos: usize,
    pub rule_set: &'rule RuleSet,

    pub rule: Var,
    pub matches: Vec<Match>,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RulePosition {
    pub var: Var,
    pub index: usize,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Match {
    pub rule_pos: RulePosition,
    pub src_pos: std::ops::Range<usize>,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// S <- A / B / C / D / E
    /// A <- "a" $
    /// B <- "b"
    /// C <- "c_" "em" empty "pty"
    /// D <- "d" "_repeat"+
    /// E <- "e_" !"foobar" "bar"
    fn rule() -> RuleSet {
        let mut rules = HashMap::new();
        rules.insert(
            Var("S".into()),
            Rule(Expr::Sequence(vec![Expr::Choice(vec![
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
            ])])),
        );
        rules.insert(
            Var("A".into()),
            Rule(Expr::Sequence(vec![
                Expr::Atom {
                    inner: Atom::Literal("a".into()),
                    index: 0,
                },
                Expr::Atom {
                    inner: Atom::End,
                    index: 1,
                },
            ])),
        );
        rules.insert(
            Var("B".into()),
            Rule(Expr::Sequence(vec![Expr::Atom {
                inner: Atom::Literal("b".into()),
                index: 0,
            }])),
        );
        rules.insert(
            Var("C".into()),
            Rule(Expr::Sequence(vec![
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
            ])),
        );
        rules.insert(
            Var("D".into()),
            Rule(Expr::Sequence(vec![
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
            ])),
        );
        rules.insert(
            Var("E".into()),
            Rule(Expr::Sequence(vec![
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
            ])),
        );
        RuleSet::new(rules, Var("S".into()))
    }

    #[test]
    fn var() {
        let rule = rule();
        let src = "a".to_string();
        let src = src.chars().collect::<Vec<_>>();
        let (src_read, eval, matches) = rule.eval(&src);
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                Match {
                    rule_pos: RulePosition {
                        var: Var("A".into()),
                        index: 0,
                    },
                    src_pos: 0..1,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("A".into()),
                        index: 1,
                    },
                    src_pos: 1..1,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("S".into()),
                        index: 0
                    },
                    src_pos: 0..1
                }
            ]
        );
    }

    #[test]
    fn choice() {
        let rule = rule();
        let src = "b".to_string();
        let src = src.chars().collect::<Vec<_>>();
        let (src_read, eval, matches) = rule.eval(&src);
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                Match {
                    rule_pos: RulePosition {
                        var: Var("B".into()),
                        index: 0,
                    },
                    src_pos: 0..1,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("S".into()),
                        index: 1
                    },
                    src_pos: 0..1
                }
            ]
        );
    }

    #[test]
    fn empty() {
        let rule = rule();
        let src = "c_empty".to_string();
        let src = src.chars().collect::<Vec<_>>();
        let (src_read, eval, matches) = rule.eval(&src);
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                Match {
                    rule_pos: RulePosition {
                        var: Var("C".into()),
                        index: 0,
                    },
                    src_pos: 0..2,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("C".into()),
                        index: 1,
                    },
                    src_pos: 2..4,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("C".into()),
                        index: 2,
                    },
                    src_pos: 4..7,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("S".into()),
                        index: 2
                    },
                    src_pos: 0..7
                }
            ]
        );
    }

    #[test]
    fn not_matched() {
        let rule = rule();
        let src = "no matches".to_string();
        let src = src.chars().collect::<Vec<_>>();
        let (src_read, eval, matches) = rule.eval(&src);
        assert_eq!(src_read, 0);
        assert_eq!(eval, EvalResult::NotMatched);
        assert_eq!(matches, vec![]);
    }

    #[test]
    fn repeat_1() {
        let rule = rule();
        let src = "d_repeat".to_string();
        let src = src.chars().collect::<Vec<_>>();
        let (src_read, eval, matches) = rule.eval(&src);
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                Match {
                    rule_pos: RulePosition {
                        var: Var("D".into()),
                        index: 0,
                    },
                    src_pos: 0..1,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("D".into()),
                        index: 1,
                    },
                    src_pos: 1..8,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("S".into()),
                        index: 3
                    },
                    src_pos: 0..8
                }
            ]
        );
    }

    #[test]
    fn repeat_2() {
        let rule = rule();
        let src = "d_repeat_repeat".to_string();
        let src = src.chars().collect::<Vec<_>>();
        let (src_read, eval, matches) = rule.eval(&src);
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                Match {
                    rule_pos: RulePosition {
                        var: Var("D".into()),
                        index: 0,
                    },
                    src_pos: 0..1,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("D".into()),
                        index: 1,
                    },
                    src_pos: 1..8,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("D".into()),
                        index: 1,
                    },
                    src_pos: 8..15,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("S".into()),
                        index: 3
                    },
                    src_pos: 0..15
                }
            ]
        );
    }

    #[test]
    fn not() {
        let rule = rule();
        let src = "e_bar".to_string();
        let src = src.chars().collect::<Vec<_>>();
        let (src_read, eval, matches) = rule.eval(&src);
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                Match {
                    rule_pos: RulePosition {
                        var: Var("E".into()),
                        index: 0,
                    },
                    src_pos: 0..2,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("E".into()),
                        index: 2,
                    },
                    src_pos: 2..5,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("S".into()),
                        index: 4
                    },
                    src_pos: 0..5
                }
            ]
        );
    }
}
