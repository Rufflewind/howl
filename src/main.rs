extern crate howl;
use howl::parser;

fn main() {
    use std::io::{Write, stderr};
    use std::process::exit;
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        writeln!(stderr(), "need 1 command-line argument").unwrap();
        exit(1);
    }
    let path = &args[1];
    let s = parser::load_file(path);
    let (nodes, errs) = parser::Node::parse(&s, path);
    for err in &errs {
        writeln!(stderr(), "{}", err).unwrap();
    }
    print!("{}", String::from_utf8_lossy(&parser::render_doc(&nodes)));
    if !errs.is_empty() {
        exit(1);
    }
}
