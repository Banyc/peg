use std::io::BufRead;

use clap::{arg, Parser};
use peg::matcher::{Filter, Matcher};

/// A simple grep-like tool for PEG grammars
#[derive(Parser, Debug)]
struct Args {
    /// Grammar files
    #[arg(short, long)]
    grammars: Vec<String>,

    /// Matching pattern
    pattern: String,

    /// Input file
    filename: Option<String>,
}

fn main() {
    let args = Args::parse();

    let top_level_expr = args.pattern;
    let lines = match args.filename {
        Some(filename) => {
            let file = std::fs::File::open(filename).unwrap();
            let mut file = std::io::BufReader::new(file);
            let mut lines = vec![];
            loop {
                let mut line = String::new();
                file.read_line(&mut line).unwrap();
                if line.is_empty() {
                    break;
                }
                lines.push(line);
            }
            lines
        }
        None => {
            let stdin = std::io::stdin();
            stdin.lines().map(|l| l.unwrap()).collect::<Vec<_>>()
        }
    };

    let mut grammars = vec![];
    for grammar in args.grammars {
        let grammar = std::fs::read_to_string(grammar).unwrap();
        grammars.push(grammar);
    }

    let filter = Filter::new(true);
    let matcher = Matcher::new_top(&grammars, &top_level_expr).unwrap();
    for line in lines {
        let (full_matches, _) = matcher.match_(&line, &filter);
        if full_matches.is_empty() {
            continue;
        }
        print!("{}", line);
    }
}
