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
            rule: self,
            rule_pos: self.start.clone(),
            matches: Default::default(),
        };
        let eval = rule.eval(&mut ctx);
        (ctx.src_pos, eval, ctx.matches)
    }
}

pub struct Rule(pub Atom);

impl Rule {
    pub fn eval(&self, ctx: &mut EvalContext) -> EvalResult {
        self.0.eval(ctx)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Atom {
    pub expr: Expr,
    pub repeat: RepeatRange,
    pub predicate: Option<Predicate>,
}

impl Atom {
    pub fn eval(&self, ctx: &mut EvalContext) -> EvalResult {
        trace!("eval: {:?}", self);
        let src_pos = ctx.src_pos;
        let mut repeat = 0;
        let mut eval = loop {
            trace!("repeat: {}", repeat);
            if let RepeatRangeEnd::Finite(end) = self.repeat.end {
                if repeat == end {
                    trace!("repeat == end");
                    break EvalResult::Matched;
                }
                assert!(repeat < end);
            }
            let eval = self.expr.eval(ctx);
            if eval == EvalResult::NotMatched {
                match self.repeat.end {
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
        if repeat < self.repeat.start {
            trace!("repeat < start");
            eval = EvalResult::NotMatched;
        }
        match &self.predicate {
            Some(p) => {
                trace!("predicate: {:?}", p);
                ctx.src_pos = src_pos;
                match p {
                    Predicate::Lookahead => eval,
                    Predicate::Not => match eval {
                        EvalResult::Matched => EvalResult::NotMatched,
                        EvalResult::NotMatched => EvalResult::Matched,
                    },
                }
            }
            None => eval,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
    Var {
        name: Var,
        index: usize,
    },
    Choice(Vec<Atom>),
    /// Terminal
    LiteralString {
        value: String,
        index: usize,
    },
    /// Sequence delimited by parentheses
    Sequence(Vec<Atom>),
}

impl Expr {
    pub fn eval(&self, ctx: &mut EvalContext) -> EvalResult {
        match self {
            Expr::Var { name, index } => {
                let src_pos = ctx.src_pos;
                let rule = ctx.rule.rule(name).unwrap();
                let rule_pos = ctx.rule_pos.clone();
                ctx.rule_pos = name.clone();
                let eval = rule.eval(ctx);

                // Restore the rule position
                ctx.rule_pos = rule_pos;

                if eval == EvalResult::Matched {
                    // Save the match
                    ctx.matches.push(Match {
                        rule_pos: RulePosition {
                            var: ctx.rule_pos.clone(),
                            index: *index,
                        },
                        src_pos: src_pos..ctx.src_pos,
                    });
                }

                eval
            }
            Expr::Choice(choice) => {
                let src_pos = ctx.src_pos;
                for atom in choice {
                    let eval = atom.eval(ctx);
                    if eval == EvalResult::Matched {
                        return EvalResult::Matched;
                    }
                    ctx.src_pos = src_pos;
                }
                EvalResult::NotMatched
            }
            Expr::LiteralString { value, index } => {
                if ctx.src[ctx.src_pos..].starts_with(value.chars().collect::<Vec<_>>().as_slice())
                {
                    let end = ctx.src_pos + value.len();

                    // Save the match
                    ctx.matches.push(Match {
                        rule_pos: RulePosition {
                            var: ctx.rule_pos.clone(),
                            index: *index,
                        },
                        src_pos: ctx.src_pos..end,
                    });

                    // Consume the input
                    ctx.src_pos = end;

                    EvalResult::Matched
                } else {
                    EvalResult::NotMatched
                }
            }
            Expr::Sequence(sequence) => {
                for atom in sequence {
                    let eval = atom.eval(ctx);
                    if eval == EvalResult::NotMatched {
                        return EvalResult::NotMatched;
                    }
                }
                EvalResult::Matched
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
    pub rule: &'rule RuleSet,

    pub rule_pos: Var,
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

    /// S <- A / B / C / D
    /// A <- "a"
    /// B <- "b"
    /// C <- "em" empty "pty"
    /// D <- "d" "repeat"+
    fn rule() -> RuleSet {
        let mut rules = HashMap::new();
        rules.insert(
            Var("S".into()),
            Rule(Atom {
                expr: Expr::Sequence(vec![Atom {
                    expr: Expr::Choice(vec![
                        Atom {
                            expr: Expr::Var {
                                name: Var("A".into()),
                                index: 0,
                            },
                            repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                            predicate: None,
                        },
                        Atom {
                            expr: Expr::Var {
                                name: Var("B".into()),
                                index: 1,
                            },
                            repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                            predicate: None,
                        },
                        Atom {
                            expr: Expr::Var {
                                name: Var("C".into()),
                                index: 2,
                            },
                            repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                            predicate: None,
                        },
                        Atom {
                            expr: Expr::Var {
                                name: Var("D".into()),
                                index: 3,
                            },
                            repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                            predicate: None,
                        },
                    ]),
                    repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                    predicate: None,
                }]),
                repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                predicate: None,
            }),
        );
        rules.insert(
            Var("A".into()),
            Rule(Atom {
                expr: Expr::Sequence(vec![Atom {
                    expr: Expr::LiteralString {
                        value: "a".into(),
                        index: 0,
                    },
                    repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                    predicate: None,
                }]),
                repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                predicate: None,
            }),
        );
        rules.insert(
            Var("B".into()),
            Rule(Atom {
                expr: Expr::Sequence(vec![Atom {
                    expr: Expr::LiteralString {
                        value: "b".into(),
                        index: 0,
                    },
                    repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                    predicate: None,
                }]),
                repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                predicate: None,
            }),
        );
        rules.insert(
            Var("C".into()),
            Rule(Atom {
                expr: Expr::Sequence(vec![
                    Atom {
                        expr: Expr::LiteralString {
                            value: "em".into(),
                            index: 0,
                        },
                        repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                        predicate: None,
                    },
                    Atom {
                        expr: Expr::Sequence(vec![]),
                        repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                        predicate: None,
                    },
                    Atom {
                        expr: Expr::LiteralString {
                            value: "pty".into(),
                            index: 1,
                        },
                        repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                        predicate: None,
                    },
                ]),
                repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                predicate: None,
            }),
        );
        rules.insert(
            Var("D".into()),
            Rule(Atom {
                expr: Expr::Sequence(vec![
                    Atom {
                        expr: Expr::LiteralString {
                            value: "d".into(),
                            index: 0,
                        },
                        repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                        predicate: None,
                    },
                    Atom {
                        expr: Expr::LiteralString {
                            value: "_repeat".into(),
                            index: 1,
                        },
                        repeat: RepeatRange::new(1, RepeatRangeEnd::Infinite),
                        predicate: None,
                    },
                ]),
                repeat: RepeatRange::new(1, RepeatRangeEnd::Finite(1)),
                predicate: None,
            }),
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
        let src = "empty".to_string();
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
                    src_pos: 2..5,
                },
                Match {
                    rule_pos: RulePosition {
                        var: Var("S".into()),
                        index: 2
                    },
                    src_pos: 0..5
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
}
