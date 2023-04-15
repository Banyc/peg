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

    match args.filenames {
        Some(filenames) => {
            assert!(!filenames.is_empty(), "No filenames given");
            let show_filename = filenames.len() > 1;
            for filename in filenames {
                let file = std::fs::File::open(filename.clone()).unwrap();
                let mut file = std::io::BufReader::new(file);
                let line_prefix = if show_filename { &filename } else { "" };
                print_lines_if_match(&mut file, &matcher, &filter, line_prefix);
            }
        }
        None => {
            let stdin = std::io::stdin();
            let mut stdin = stdin.lock();
            print_lines_if_match(&mut stdin, &matcher, &filter, "");
        }
    };
}

fn print_lines_if_match<R>(read_buf: &mut R, matcher: &Matcher, filter: &Filter, line_prefix: &str)
where
    R: BufRead,
{
    let mut line = String::new();
    loop {
        read_buf.read_line(&mut line).unwrap();
        if line.is_empty() {
            break;
        }
        print_line_if_match(&line, matcher, filter, line_prefix);
        line.clear();
    }
}

fn print_line_if_match(line: &str, matcher: &Matcher, filter: &Filter, line_prefix: &str) {
    let (full_matches, _) = matcher.match_(line, filter).unwrap();
    if full_matches.is_empty() {
        return;
    }
    print!("{}:{}", line_prefix, line);
}
