use std::io::BufRead;

use clap::Parser;
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

    // Read all grammars
    let mut grammars = vec![];
    for grammar in args.grammars {
        let grammar = std::fs::read_to_string(grammar).unwrap();
        grammars.push(grammar);
    }

    // Build matcher
    let filter = Filter::new(true);
    let matcher = Matcher::new_top(&grammars, &top_level_expr).unwrap();

    match args.filename {
        Some(filename) => {
            let file = std::fs::File::open(filename).unwrap();
            let mut file = std::io::BufReader::new(file);
            print_lines_if_match(&mut file, &matcher, &filter);
        }
        None => {
            let stdin = std::io::stdin();
            let mut stdin = stdin.lock();
            print_lines_if_match(&mut stdin, &matcher, &filter);
        }
    };
}

fn print_lines_if_match<R>(read_buf: &mut R, matcher: &Matcher, filter: &Filter)
where
    R: BufRead,
{
    let mut line = String::new();
    loop {
        read_buf.read_line(&mut line).unwrap();
        if line.is_empty() {
            break;
        }
        print_line_if_match(&line, matcher, filter);
        line.clear();
    }
}

fn print_line_if_match(line: &str, matcher: &Matcher, filter: &Filter) {
    let (full_matches, _) = matcher.match_(line, filter).unwrap();
    if full_matches.is_empty() {
        return;
    }
    print!("{}", line);
}
