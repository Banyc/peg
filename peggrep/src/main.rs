use std::io::BufRead;

use peg::matcher::{Filter, Matcher};

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let top_level_expr = args.get(1).unwrap();
    let lines = match args.get(2) {
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
    let filter = Filter::new(true);
    let matcher = Matcher::new_top(&[], top_level_expr).unwrap();
    for line in lines {
        let (full_matches, _) = matcher.match_(&line, &filter);
        if full_matches.is_empty() {
            continue;
        }
        print!("{}", line);
    }
}
