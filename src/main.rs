#[derive(Clone, Copy)]
struct DebugUtf8<'a>(pub &'a [u8]);

impl<'a> ::std::fmt::Debug for DebugUtf8<'a> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match ::std::str::from_utf8(self.0) {
            Ok(s) => write!(f, "b{:?}", s),
            Err(_) => self.0.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DelimType { Bracket, Parenthesis, Brace }

#[derive(Debug, Clone, Copy)]
pub enum DelimDirection { Opening, Closing }

impl ::std::ops::Not for DelimDirection {
    type Output = Self;
    fn not(self) -> Self::Output {
        use DelimDirection::*;
        match self {
            Opening => Closing,
            Closing => Opening,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Delim(DelimType, DelimDirection);

impl Delim {
    fn from_u8(d: u8) -> Option<Self> {
        use DelimType::*;
        use DelimDirection::*;
        match d {
            b'[' => Some(Delim(Bracket, Opening)),
            b']' => Some(Delim(Bracket, Closing)),
            b'(' => Some(Delim(Parenthesis, Opening)),
            b')' => Some(Delim(Parenthesis, Closing)),
            b'{' => Some(Delim(Brace, Opening)),
            b'}' => Some(Delim(Brace, Closing)),
            _ => None,
        }
    }

    fn to_u8(&self) -> u8 {
        use DelimType::*;
        use DelimDirection::*;
        match self {
            &Delim(Bracket, Opening) => b'[',
            &Delim(Bracket, Closing) => b']',
            &Delim(Parenthesis, Opening) => b'(',
            &Delim(Parenthesis, Closing) => b')',
            &Delim(Brace, Opening) => b'{',
            &Delim(Brace, Closing) => b'}',
        }
    }
}

impl ::std::ops::Not for Delim {
    type Output = Self;
    fn not(self) -> Self::Output {
        let Delim(d, dir) = self;
        Delim(d, !dir)
    }
}

fn is_ascii_space(c: u8) -> bool {
    match c {
        b' ' => true,
        _ if c >= 0x9 && c < 0xe => true,
        _ => false,
    }
}

#[derive(Clone, Copy, Debug)]
struct Loc<'a> {
    name: &'a str,
    row: usize,
    col: usize,
}

impl<'a> Loc<'a> {
    fn new(name: &'a str) -> Self {
        Loc { name: name, row: 0, col: 0 }
    }
}

#[derive(Clone, Copy)]
enum Token<'a>{
    Chunk(Loc<'a>, &'a [u8]),
    Tag(Loc<'a>, &'a [u8], Delim),
}

impl<'a> ::std::fmt::Debug for Token<'a> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            &Token::Chunk(ref loc, ref s) => {
                f.debug_tuple("Chunk")
                    .field(loc)
                    .field(&DebugUtf8(s))
                    .finish()
            }
            &Token::Tag(ref loc, ref s, ref d) => {
                f.debug_tuple("Tag")
                    .field(loc)
                    .field(&DebugUtf8(s))
                    .field(&DebugUtf8(&[d.to_u8()]))
                    .finish()
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Lexer<'a> {
    input: &'a [u8],
    loc: Loc<'a>,
    tag: Option<(&'a [u8], Delim)>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a [u8], loc: Loc<'a>) -> Self {
        Lexer { input: input, loc: loc, tag: None }
    }
}

/// Like splitn, but specialized to return at most two items.
fn slice_split2<T, R, F>(s: &[T], mut pred: F) -> Option<(&[T], R, &[T])>
    where F: FnMut(&T) -> Option<R> {
    for i in 0 .. s.len() {
        if let Some(r) = pred(&s[i]) {
            return Some((s.split_at(i).0, r, s.split_at(i + 1).1));
        }
    }
    None
}

/// The second element is the longest suffix of elements that fail to satisfy
/// the predicate.  The first element is the remaining part.
fn slice_rbreak<T, F>(s: &[T], mut pred: F) -> (&[T], &[T])
    where F: FnMut(&T) -> bool {
    let mut j = 0;
    for i in (0 .. s.len()).rev() {
        j = i;
        if pred(&s[i]) {
            break;
        }
    }
    s.split_at(j)
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        // we still have a tag from the previous iteration
        let mut tag = None;
        ::std::mem::swap(&mut tag, &mut self.tag);
        if let Some((word, delim)) = tag {
            let loc = self.loc;
            self.loc.row += word.len() + 1;
            return Some(Token::Tag(loc, word, delim));
        }
        // end of input
        if self.input.len() == 0 {
            return None;
        }
        // find the next delimiter
        let loc = self.loc;
        match slice_split2(self.input, |c| {
            match Delim::from_u8(*c) {
                None => {
                    self.loc.col += 1;
                    if *c == b'\n' {
                        self.loc.col = 0;
                        self.loc.row += 1;
                    }
                    None
                }
                Some(d) => Some(d),
            }
        }) {
            None => { // no delimiter
                let chunk = self.input;
                self.input = &[];
                Some(Token::Chunk(loc, chunk))
            }
            Some((before, delim, rest)) => { // found delimiter
                self.input = rest;
                let (chunk, word) = slice_rbreak(before,
                                                 |c| is_ascii_space(*c));
                self.loc.row -= word.len();
                self.tag = Some((word, delim));
                Some(Token::Chunk(loc, chunk))
            }
        }
    }
}

// class Node:
//     def __init__(self, name, children):
//         self.name = name
//         self.children = children

//     def __repr__(self):
//         return "{}({!r}, {!r})".format(
//             type(self).__name__,
//             self.name,
//             self.children
//         )

// def parse(tokens):
//     ctxs = [{
//         "name": "",
//         "delim": "",
//         "children": [],
//     }]
//     for chunk, name, delim in tokens:
//         if delim in "])}":
//             if delim != ctxs[-1]:
//                 raise ValueError()
//         if delim in "[({":
//             _
//         print(repr(chunk), name, delim)

#[derive(Clone, Debug)]
struct Elem<T> {
    name: T,
    delim: DelimType,
    children: Vec<Node<T>>,
}

#[derive(Clone, Debug)]
enum Node<T> {
    Text(T),
    Elem(Elem<T>),
}

impl<'a> Node<&'a str> {
    fn parse<I: Iterator<Item=Token<'a>>>(tokens: I) -> Vec<Self> {
        struct Ctx<'a> {
            name: &'a str,
            delim: Option<DelimType>,
            children: Vec<Node<&'a str>>,
        };
        let mut stack = vec![Ctx {
            name: &"",
            delim: None,
            children: Vec::new(),
        }];
        for token in tokens {
            println!("{:?}", token);
        }
        panic!()
    }
}

fn load_file(path: &str) -> Vec<u8> {
    use std::io::Read;
    let mut f = std::fs::File::open(path).unwrap();
    let mut s = Vec::new();
    let _ = f.read_to_end(&mut s).unwrap();
    s
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let path = &args[1];
    let s = load_file(path);
    let lex = Lexer::new(&s, Loc::new(path));
    let nodes = Node::parse(lex);
}
