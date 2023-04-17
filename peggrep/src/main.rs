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

    /// Input files
    filenames: Option<Vec<String>>,

    /// Select non-matching lines
    #[arg(short('v'), long)]
    invert_match: bool,

    /// Match at the start of a line
    #[arg(short('^'), long)]
    start_of_line: bool,
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

    // Match
    let match_args = MatchArgs {
        invert_match: args.invert_match,
        start_of_line: args.start_of_line,
    };

    match args.filenames {
        Some(filenames) => {
            assert!(!filenames.is_empty(), "No filenames given");
            let show_filename = filenames.len() > 1;
            for filename in filenames {
                let file = std::fs::File::open(filename.clone()).unwrap();
                let mut file = std::io::BufReader::new(file);
                let line_prefix = if show_filename {
                    Some(filename.as_ref())
                } else {
                    None
                };
                print_lines_if_match(&mut file, &matcher, &filter, line_prefix, &match_args);
            }
        }
        None => {
            let stdin = std::io::stdin();
            let mut stdin = stdin.lock();
            print_lines_if_match(&mut stdin, &matcher, &filter, None, &match_args);
        }
    };
}

fn print_lines_if_match<R>(
    read_buf: &mut R,
    matcher: &Matcher,
    filter: &Filter,
    line_prefix: Option<&str>,
    args: &MatchArgs,
) where
    R: BufRead,
{
    let mut line = String::new();
    loop {
        read_buf.read_line(&mut line).unwrap();
        if line.is_empty() {
            break;
        }
        print_line_if_match(&line, matcher, filter, line_prefix, args);
        line.clear();
    }
}

struct MatchArgs {
    invert_match: bool,
    start_of_line: bool,
}

fn print_line_if_match(
    line: &str,
    matcher: &Matcher,
    filter: &Filter,
    line_prefix: Option<&str>,
    args: &MatchArgs,
) {
    let (full_matches, _) = matcher.match_(line, filter).unwrap();
    let mut matched = false;
    if !full_matches.is_empty() {
        matched = true;
    }
    if args.start_of_line && matched {
        let first_match = &full_matches[0];
        if first_match.start != 0 {
            matched = false;
        }
    }

    if !matched != args.invert_match {
        return;
    }
    match line_prefix {
        Some(line_prefix) => print!("{}:{}", line_prefix, line),
        None => print!("{}", line),
    }
}
