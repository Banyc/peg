use std::{
    collections::{HashMap, HashSet},
    ops::Range,
};

use crate::{
    parser::{Error, PegParser},
    rule::{AtomMatch, EvalError, EvalResult, Expr, RulePosition, RuleSet, Var},
};

pub struct Matcher {
    rule: RuleSet,
}

impl Matcher {
    pub fn new(patterns: &[String], start: String) -> Result<Self, Error> {
        let (rules, _) = parse(patterns)?;
        let rule = RuleSet::new(rules, Var(start));
        Ok(Self { rule })
    }

    pub fn new_top(patterns: &[String], top_level_expr: &str) -> Result<Self, Error> {
        let (mut rules, parser) = parse(patterns)?;
        let top_expr = parser.parse_expr(top_level_expr)?;
        let start = Var("".into());
        rules.insert(start.clone(), top_expr);
        let rule = RuleSet::new(rules, start);
        Ok(Self { rule })
    }

    pub fn match_(
        &self,
        src: &str,
        filter: &Filter,
    ) -> Result<(Vec<Range<usize>>, Vec<AtomMatch>), EvalError> {
        let mut global_full_matches = Vec::new();
        let mut global_atom_matches = Vec::new();

        let src = src.chars().collect::<Vec<_>>();

        let mut pos = 0;
        while pos <= src.len() {
            let (read, eval, atom_matches) = self.rule.eval(&src[pos..])?;
            match eval {
                EvalResult::Matched => {
                    if filter.full() {
                        global_full_matches.push(pos..pos + read);
                    }
                    let atom_matches = atom_matches
                        .into_iter()
                        .filter(|m| filter.atoms().contains(&m.rule_pos));
                    global_atom_matches.extend(atom_matches);
                    pos += read.max(1);
                }
                EvalResult::NotMatched => {
                    pos += 1;
                }
            }
        }

        Ok((global_full_matches, global_atom_matches))
    }
}

fn parse(patterns: &[String]) -> Result<(HashMap<Var, Expr>, PegParser), Error> {
    let parser = PegParser::new();
    let mut rules = HashMap::new();
    for pattern in patterns {
        let ast = parser.parse(pattern)?;
        for (var, rule) in ast {
            rules.insert(var, rule);
        }
    }
    Ok((rules, parser))
}

pub struct Filter {
    atoms: HashSet<RulePosition>,
    full: bool,
}

impl Filter {
    pub fn new(full: bool) -> Self {
        Self {
            atoms: Default::default(),
            full,
        }
    }

    pub fn add(&mut self, var: Var, index: usize) {
        self.atoms.insert(RulePosition { var, index });
    }

    pub fn atoms(&self) -> &HashSet<RulePosition> {
        &self.atoms
    }

    pub fn full(&self) -> bool {
        self.full
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn many() {
        let top_level_expr = "
            '(' . ')'
        ";
        let src = "a (1) b (2) c (3d) e (4)";

        let filter = Filter::new(true);
        let matcher = Matcher::new_top(&[], top_level_expr).unwrap();
        let (full_matches, _) = matcher.match_(src, &filter).unwrap();
        let expected = vec!["(1)", "(2)", "(4)"];
        let matched_str = full_matches
            .into_iter()
            .map(|r| &src[r])
            .map(|s| s.to_string())
            .collect::<Vec<_>>();
        assert_eq!(matched_str, expected);
    }
}
