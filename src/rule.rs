use std::{borrow::Cow, collections::HashMap};

use thiserror::Error;
use tracing::trace;

/// Non-terminal
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub args: Vec<Var>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Def {
    Rule(Expr),
    Func(Func),
}

impl Def {
    pub fn set_atom_indices(&mut self) {
        match self {
            Def::Rule(rule) => {
                rule.set_atom_indices(0);
            }
            Def::Func(func) => {
                func.body.set_atom_indices(0);
            }
        }
    }
}

pub type Rules = HashMap<Var, Def>;

pub struct RuleSet {
    rules: Rules,
    start: Var,
}

impl RuleSet {
    pub fn new(rules: Rules, start: Var) -> Self {
        Self { rules, start }
    }

    pub fn def(&self, var: &Var) -> Option<&Def> {
        self.rules.get(var)
    }

    pub fn start(&self) -> &Var {
        &self.start
    }

    pub fn eval(&self, src: &[char]) -> Result<(usize, EvalResult, Vec<AtomMatch>), EvalError> {
        let rule = self.def(&self.start).unwrap();
        let rule = match rule {
            Def::Rule(rule) => rule,
            Def::Func(_) => {
                panic!("start rule must be a rule, not a function");
            }
        };
        let mut ctx = EvalContext {
            src_pos: 0,
            src,
            rules: &self.rules,
            params: Default::default(),
            rule: self.start.clone(),
            matches: Default::default(),
        };
        let eval = rule.eval(&mut ctx)?;
        Ok((ctx.src_pos, eval, ctx.matches))
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
    pub fn eval(&self, ctx: &mut EvalContext) -> Result<EvalResult, EvalError> {
        let eval = match self {
            Expr::Atom { index, inner } => {
                let src_pos = ctx.src_pos;
                let eval = inner.eval(ctx)?;

                if eval == EvalResult::Matched {
                    // Save the match
                    ctx.matches.push(AtomMatch {
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
                    let eval = expr.eval(ctx)?;
                    if eval == EvalResult::Matched {
                        return Ok(EvalResult::Matched);
                    }
                    ctx.src_pos = src_pos;
                }
                EvalResult::NotMatched
            }
            Expr::Sequence(sequence) => {
                for expr in sequence {
                    let eval = expr.eval(ctx)?;
                    if eval == EvalResult::NotMatched {
                        return Ok(EvalResult::NotMatched);
                    }
                }
                EvalResult::Matched
            }
            Expr::Predicate { predicate, inner } => {
                let src_pos = ctx.src_pos;
                let eval = inner.eval(ctx)?;
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
                    let eval = inner.eval(ctx)?;
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
        };
        Ok(eval)
    }

    pub fn set_atom_indices(&mut self, next: usize) -> usize {
        match self {
            Expr::Atom { index, inner } => {
                *index = next;
                let next = next + 1;
                match inner {
                    Atom::Call { params, .. } => {
                        let mut next = next;
                        for param in params {
                            next = param.set_atom_indices(next);
                        }
                        next
                    }
                    _ => next,
                }
            }
            Expr::Choice(choice) => {
                let mut next = next;
                for expr in choice {
                    next = expr.set_atom_indices(next);
                }
                next
            }
            Expr::Sequence(sequence) => {
                let mut next = next;
                for expr in sequence {
                    next = expr.set_atom_indices(next);
                }
                next
            }
            Expr::Predicate { inner, .. } => inner.set_atom_indices(next),
            Expr::Repeat { inner, .. } => inner.set_atom_indices(next),
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
    /// `.`
    Any,
    /// e.g.: `foo[x, y]`
    Call {
        func: Var,
        params: Vec<Expr>,
    },
}

impl Atom {
    pub fn eval(&self, ctx: &mut EvalContext) -> Result<EvalResult, EvalError> {
        let eval = match self {
            Atom::Var(name) => {
                let rule = ctx.rule.clone();

                let expr = match ctx.params.get(name) {
                    Some((rule, expr)) => {
                        // Set the rule position
                        ctx.rule = rule.clone();

                        Cow::Owned(expr.clone())
                    }
                    None => match ctx.rules.get(name) {
                        Some(Def::Rule(expr)) => {
                            // Set the rule position
                            ctx.rule = name.clone();

                            Cow::Borrowed(expr)
                        }
                        _ => return Err(EvalError::UndefinedRule(name.clone())),
                    },
                };

                let eval = expr.eval(ctx)?;

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
            Atom::Any => {
                if ctx.src.len() == ctx.src_pos {
                    EvalResult::NotMatched
                } else {
                    // Consume the input
                    ctx.src_pos += 1;

                    EvalResult::Matched
                }
            }
            Atom::Call { func, params } => {
                let ctx_params = ctx.params.clone();
                let name = func.clone();
                let rule = ctx.rule.clone();
                ctx.rule = name.clone();
                let func = match ctx.rules.get(&name) {
                    Some(Def::Func(func)) => func,
                    _ => return Err(EvalError::UndefinedFunc(name.clone())),
                };

                // Check the number of arguments
                if params.len() != func.args.len() {
                    return Err(EvalError::UnmatchedArgs {
                        func: name,
                        expected: func.args.len(),
                        actual: params.len(),
                    });
                }

                // Put the params to the context
                for (param, arg) in params.iter().zip(func.args.iter()) {
                    ctx.params
                        .insert(arg.clone(), (rule.clone(), param.clone()));
                }

                // Evaluate the function
                let eval = func.body.eval(ctx)?;

                // Restore the params
                ctx.params = ctx_params;

                // Restore the rule position
                ctx.rule = rule;

                eval
            }
        };
        Ok(eval)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalResult {
    Matched,
    NotMatched,
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum EvalError {
    /// The rule is not defined
    #[error("undefined rule: {0:?}")]
    UndefinedRule(Var),
    /// The function is not defined
    #[error("undefined function: {0:?}")]
    UndefinedFunc(Var),
    /// The function is called with the wrong number of arguments
    #[error("unmatched arguments: {func:?} expected {expected} but got {actual}")]
    UnmatchedArgs {
        func: Var,
        expected: usize,
        actual: usize,
    },
}

pub struct EvalContext<'src, 'rule> {
    pub src: &'src [char],
    pub src_pos: usize,
    pub rules: &'rule Rules,

    /// param name -> (rule name, expr)
    pub params: HashMap<Var, (Var, Expr)>,

    pub rule: Var,
    pub matches: Vec<AtomMatch>,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RulePosition {
    pub var: Var,
    pub index: usize,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AtomMatch {
    pub rule_pos: RulePosition,
    pub src_pos: std::ops::Range<usize>,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// S <- A / B / C / D / E / F
    /// A <- "a" $
    /// B <- "b"
    /// C <- "c_" "em" empty "pty"
    /// D <- "d" "_repeat"+
    /// E <- "e_" !"foobar" "bar"
    /// F <- "f" ..
    fn rule() -> RuleSet {
        let mut rules = HashMap::new();
        rules.insert(
            Var("S".into()),
            Def::Rule(Expr::Sequence(vec![Expr::Choice(vec![
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
                Expr::Atom {
                    inner: Atom::Var(Var("F".into())),
                    index: 5,
                },
            ])])),
        );
        rules.insert(
            Var("A".into()),
            Def::Rule(Expr::Sequence(vec![
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
            Def::Rule(Expr::Sequence(vec![Expr::Atom {
                inner: Atom::Literal("b".into()),
                index: 0,
            }])),
        );
        rules.insert(
            Var("C".into()),
            Def::Rule(Expr::Sequence(vec![
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
            Def::Rule(Expr::Sequence(vec![
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
            Def::Rule(Expr::Sequence(vec![
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
        rules.insert(
            Var("F".into()),
            Def::Rule(Expr::Sequence(vec![
                Expr::Atom {
                    inner: Atom::Literal("f".into()),
                    index: 0,
                },
                Expr::Atom {
                    inner: Atom::Any,
                    index: 1,
                },
                Expr::Atom {
                    inner: Atom::Any,
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
        let (src_read, eval, matches) = rule.eval(&src).unwrap();
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("A".into()),
                        index: 0,
                    },
                    src_pos: 0..1,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("A".into()),
                        index: 1,
                    },
                    src_pos: 1..1,
                },
                AtomMatch {
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
        let (src_read, eval, matches) = rule.eval(&src).unwrap();
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("B".into()),
                        index: 0,
                    },
                    src_pos: 0..1,
                },
                AtomMatch {
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
        let (src_read, eval, matches) = rule.eval(&src).unwrap();
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("C".into()),
                        index: 0,
                    },
                    src_pos: 0..2,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("C".into()),
                        index: 1,
                    },
                    src_pos: 2..4,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("C".into()),
                        index: 2,
                    },
                    src_pos: 4..7,
                },
                AtomMatch {
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
        let (src_read, eval, matches) = rule.eval(&src).unwrap();
        assert_eq!(src_read, 0);
        assert_eq!(eval, EvalResult::NotMatched);
        assert_eq!(matches, vec![]);
    }

    #[test]
    fn repeat_1() {
        let rule = rule();
        let src = "d_repeat".to_string();
        let src = src.chars().collect::<Vec<_>>();
        let (src_read, eval, matches) = rule.eval(&src).unwrap();
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("D".into()),
                        index: 0,
                    },
                    src_pos: 0..1,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("D".into()),
                        index: 1,
                    },
                    src_pos: 1..8,
                },
                AtomMatch {
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
        let (src_read, eval, matches) = rule.eval(&src).unwrap();
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("D".into()),
                        index: 0,
                    },
                    src_pos: 0..1,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("D".into()),
                        index: 1,
                    },
                    src_pos: 1..8,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("D".into()),
                        index: 1,
                    },
                    src_pos: 8..15,
                },
                AtomMatch {
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
        let (src_read, eval, matches) = rule.eval(&src).unwrap();
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("E".into()),
                        index: 0,
                    },
                    src_pos: 0..2,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("E".into()),
                        index: 2,
                    },
                    src_pos: 2..5,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("S".into()),
                        index: 4
                    },
                    src_pos: 0..5
                }
            ]
        );
    }

    #[test]
    fn any() {
        let rule = rule();
        let src = "f_f".to_string();
        let src = src.chars().collect::<Vec<_>>();
        let (src_read, eval, matches) = rule.eval(&src).unwrap();
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("F".into()),
                        index: 0,
                    },
                    src_pos: 0..1,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("F".into()),
                        index: 1,
                    },
                    src_pos: 1..2,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("F".into()),
                        index: 2,
                    },
                    src_pos: 2..3,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("S".into()),
                        index: 5
                    },
                    src_pos: 0..3
                }
            ]
        );
    }

    #[test]
    fn set_atom_indices() {
        let mut expr = Expr::Choice(vec![
            Expr::Atom {
                inner: Atom::Var(Var("A".into())),
                index: 0,
            },
            Expr::Atom {
                inner: Atom::Var(Var("B".into())),
                index: 0,
            },
            Expr::Atom {
                inner: Atom::Var(Var("C".into())),
                index: 0,
            },
            Expr::Atom {
                inner: Atom::Var(Var("D".into())),
                index: 0,
            },
            Expr::Atom {
                inner: Atom::Var(Var("E".into())),
                index: 0,
            },
            Expr::Atom {
                inner: Atom::Call {
                    func: Var("F".into()),
                    params: vec![
                        Expr::Atom {
                            inner: Atom::Var(Var("G".into())),
                            index: 0,
                        },
                        Expr::Atom {
                            inner: Atom::Var(Var("H".into())),
                            index: 0,
                        },
                    ],
                },
                index: 0,
            },
            Expr::Atom {
                inner: Atom::Var(Var("I".into())),
                index: 0,
            },
        ]);
        expr.set_atom_indices(0);
        assert_eq!(
            expr,
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
                Expr::Atom {
                    inner: Atom::Call {
                        func: Var("F".into()),
                        params: vec![
                            Expr::Atom {
                                inner: Atom::Var(Var("G".into())),
                                index: 6,
                            },
                            Expr::Atom {
                                inner: Atom::Var(Var("H".into())),
                                index: 7,
                            },
                        ],
                    },
                    index: 5,
                },
                Expr::Atom {
                    inner: Atom::Var(Var("I".into())),
                    index: 8,
                },
            ])
        );
    }

    #[test]
    fn call() {
        // S           <- three['f']
        // three[char] <- char 3..3
        let mut rules = HashMap::new();
        rules.insert(
            Var("S".into()),
            Def::Rule(Expr::Atom {
                index: 0,
                inner: Atom::Call {
                    func: Var("three".into()),
                    params: vec![Expr::Atom {
                        index: 1,
                        inner: Atom::Literal("f".into()),
                    }],
                },
            }),
        );
        rules.insert(
            Var("three".into()),
            Def::Func(Func {
                args: vec![Var("char".into())],
                body: Expr::Repeat {
                    range: RepeatRange {
                        start: 3,
                        end: RepeatRangeEnd::Finite(3),
                    },
                    inner: Expr::Atom {
                        index: 0,
                        inner: Atom::Var(Var("char".into())),
                    }
                    .into(),
                },
            }),
        );
        let rule = RuleSet::new(rules, Var("S".into()));
        let src = "fff".to_string();
        let src = src.chars().collect::<Vec<_>>();
        let (src_read, eval, matches) = rule.eval(&src).unwrap();
        assert_eq!(src_read, src.len());
        assert_eq!(eval, EvalResult::Matched);
        assert_eq!(
            matches,
            vec![
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("S".into()),
                        index: 1,
                    },
                    src_pos: 0..1,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("three".into()),
                        index: 0,
                    },
                    src_pos: 0..1,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("S".into()),
                        index: 1,
                    },
                    src_pos: 1..2,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("three".into()),
                        index: 0,
                    },
                    src_pos: 1..2,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("S".into()),
                        index: 1,
                    },
                    src_pos: 2..3,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("three".into()),
                        index: 0,
                    },
                    src_pos: 2..3,
                },
                AtomMatch {
                    rule_pos: RulePosition {
                        var: Var("S".into()),
                        index: 0
                    },
                    src_pos: 0..3
                }
            ]
        );
    }
}
