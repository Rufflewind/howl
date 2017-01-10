pub mod debug;

use debug::debug_utf8;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DelimType { Bracket, Parenthesis, Brace }

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DelimSide { Open, Close }

impl ::std::ops::Not for DelimSide {
    type Output = Self;
    fn not(self) -> Self::Output {
        use DelimSide::*;
        match self {
            Open => Close,
            Close => Open,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Delim(DelimType, DelimSide);

impl Delim {
    fn from_u8(d: u8) -> Option<Self> {
        use DelimType::*;
        use DelimSide::*;
        match d {
            b'(' => Some(Delim(Parenthesis, Open)),
            b')' => Some(Delim(Parenthesis, Close)),
            b'[' => Some(Delim(Bracket, Open)),
            b']' => Some(Delim(Bracket, Close)),
            b'{' => Some(Delim(Brace, Open)),
            b'}' => Some(Delim(Brace, Close)),
            _ => None,
        }
    }

    fn to_u8(&self) -> u8 {
        self.to_u8s()[0]
    }

    fn to_u8s(&self) -> &'static [u8] {
        use DelimType::*;
        use DelimSide::*;
        match self {
            &Delim(Parenthesis, Open) => b"(",
            &Delim(Parenthesis, Close) => b")",
            &Delim(Bracket, Open) => b"[",
            &Delim(Bracket, Close) => b"]",
            &Delim(Brace, Open) => b"{",
            &Delim(Brace, Close) => b"}",
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

impl<'a> Default for Loc<'a> {
    fn default() -> Self {
        Loc::new("")
    }
}

impl<'a> ::std::fmt::Display for Loc<'a> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        if self.name.is_empty() {
            write!(f, "<unknown>")
        } else {
            write!(f, "{}:{}:{}", self.name, self.row + 1, self.col + 1)
        }
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
                    .field(&debug_utf8(s))
                    .finish()
            }
            &Token::Tag(ref loc, ref w, ref d) => {
                f.debug_tuple("Tag")
                    .field(loc)
                    .field(&debug_utf8(w))
                    .field(&debug_utf8(&[d.to_u8()]))
                    .finish()
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Lexer<'a> {
    input: &'a [u8],
    loc: Loc<'a>,
    state: Option<(&'a [u8], Delim)>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a [u8], loc: Loc<'a>) -> Self {
        Lexer { input: input, loc: loc, state: None }
    }
}

/// Find the element for which the predicate is true, and then make a split
/// immediately afterwards.  If not found, returns `None`.
fn slice_split2<T, R, F>(s: &[T], mut pred: F) -> Option<(&[T], R, &[T])>
    where F: FnMut(&T) -> Option<R> {
    for (i, c) in s.into_iter().enumerate() {
        if let Some(r) = pred(c) {
            return Some((s.split_at(i).0, r, s.split_at(i + 1).1));
        }
    }
    None
}

/// The first element is the longest suffix of elements that satisfies the
/// predicate.  The second element is the remaining part.
fn slice_rspan<T, F>(s: &[T], mut pred: F) -> (&[T], &[T])
    where F: FnMut(&T) -> bool {
    let mut j = s.len();
    for (i, c) in s.into_iter().enumerate().rev() {
        if !pred(c) {
            break;
        }
        j = i;
    }
    let (suffix, rest) = s.split_at(j);
    (rest, suffix)
}

static DIVIDER: u8 = b'/';

fn is_divider(c: u8) -> bool {
    c == DIVIDER
}

fn is_word_char(c: u8) -> bool {
    !(is_ascii_space(c) || is_divider(c))
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        match ::std::mem::replace(&mut self.state, None) {
            None => {
                // end of input
                if self.input.len() == 0 {
                    return None;
                }
                // find the next delimiter
                let loc = self.loc;
                match slice_split2(self.input, |c| {
                    let delim = Delim::from_u8(*c);
                    if delim.is_none() {
                        self.loc.col += 1;
                        if *c == b'\n' {
                            self.loc.col = 0;
                            self.loc.row += 1;
                        }
                    }
                    delim
                }) {
                    None => { // no delimiter
                        let chunk = self.input;
                        self.input = &[];
                        Some(Token::Chunk(loc, chunk))
                    }
                    Some((pre, delim, input)) => { // found delimiter
                        self.input = input;
                        let (word, chunk) = slice_rspan(
                            pre, |&c| is_word_char(c));
                        let chunk = match chunk.split_last() {
                            Some((&c, rest)) if is_divider(c) => rest,
                            _ => chunk,
                        };
                        // assuming words can never contain newlines
                        self.loc.col -= word.len();
                        self.state = Some((word, delim));
                        Some(Token::Chunk(loc, chunk))
                    }
                }
            }
            // we still have a tag from the previous iteration
            Some((word, delim)) => {
                let loc = self.loc;
                // assuming delimiters are never newlines
                self.loc.col += word.len() + 1;
                return Some(Token::Tag(loc, word, delim));
            }
        }
    }
}

#[derive(Clone, Debug)]
struct Elem<T, L> {
    name: T,
    delim: DelimType,
    children: Vec<Node<T, L>>,
    loc: L,
}

type IntoTextNodesIter<T> =
    ::std::iter::Chain<::std::iter::Chain<::std::iter::Once<T>,
                                          ::std::iter::Once<T>>,
                       ::std::vec::IntoIter<T>>;

fn escape_delim<'a, L: Default>(delim: Delim) -> Node<&'a [u8], L> {
    Node::Elem(Elem {
        name: b"\\" as &[u8],
        delim: DelimType::Parenthesis,
        children: vec![Node::Text(delim.to_u8s())],
        loc: L::default(),
    })
}

impl<'a, L> Elem<&'a [u8], L> {
    /// Melt the node into a mix of text nodes and child nodes.
    /// The closing delimiter is not included.
    fn into_text_nodes(self) -> IntoTextNodesIter<Node<&'a [u8], L>>
        where L: Default {
        let delim = Delim(self.delim, DelimSide::Open);
        std::iter::once(Node::Text(self.name))
            .chain(std::iter::once(escape_delim(delim)))
            .chain(self.children.into_iter())
    }
}

trait WriteTo {
    type State;
    fn write_to<W>(&self, f: &mut W, s: &mut Self::State)
                   -> ::std::io::Result<()> where W: ::std::io::Write;
}

fn write_to_vec<T: ?Sized>(x: &T, s: &mut T::State)
                           -> Vec<u8> where T: WriteTo {
    let mut v = Vec::new();
    x.write_to(&mut v, s).unwrap();
    v
}

impl<'a> WriteTo for [u8] {
    type State = ();
    fn write_to<W>(&self, f: &mut W, _: &mut Self::State)
                   -> ::std::io::Result<()> where W: ::std::io::Write {
        f.write_all(self)
    }
}

enum NodeWriteState { Clean, Sticky }

impl<'a, L> WriteTo for [Node<&'a [u8], L>] {
    type State = NodeWriteState;
    fn write_to<W>(&self, f: &mut W, s: &mut Self::State)
                   -> ::std::io::Result<()> where W: ::std::io::Write {
        for x in self {
            try!(x.write_to(f, s))
        }
        Ok(())
    }
}

impl<'a, L> WriteTo for Node<&'a [u8], L> {
    type State = NodeWriteState;
    fn write_to<W>(&self, f: &mut W, s: &mut Self::State)
                   -> ::std::io::Result<()> where W: ::std::io::Write {
        match self {
            &Node::Text(t) => {
                try!(t.write_to(f, &mut ()));
                if is_word_char(*t.last().unwrap_or(&b' ')) {
                    *s = NodeWriteState::Sticky;
                } else {
                    *s = NodeWriteState::Clean;
                }
            }
            &Node::Elem(ref elem) => {
                if let &mut NodeWriteState::Sticky = s {
                    try!([DIVIDER].write_to(f, &mut ()));
                }
                try!(elem.name.write_to(f, &mut ()));
                try!(Delim(elem.delim, DelimSide::Open).to_u8s()
                     .write_to(f, &mut ()));
                try!(elem.children.write_to(f, s));
                if is_escape(elem.name) {
                    try!(elem.name.write_to(f, &mut ()));
                }
                try!(Delim(elem.delim, DelimSide::Close).to_u8s()
                     .write_to(f, &mut ()));
                *s = NodeWriteState::Clean;
            },
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
enum Node<T, L> {
    Text(T),
    Elem(Elem<T, L>),
}

fn is_escape(name: &[u8]) -> bool {
    match name.first() {
        Some(&b'\\') => true,
        _ => false,
    }
}

impl<'a> Node<&'a [u8], Loc<'a>> {
    fn parse<I: Iterator<Item=Token<'a>>>(tokens: I)
                                          -> (Vec<Self>, Vec<String>) {
        let mut errs = Vec::new();
        let mut stack = Vec::new();
        let mut top = Elem {
            name: &[] as &[u8],
            delim: DelimType::Parenthesis,
            children: Vec::new(),
            loc: Default::default(),
        };
        for token in tokens {
            let esc = is_escape(top.name);
            match token {
                Token::Chunk(_, s) => {
                    top.children.push(Node::Text(s));
                }
                Token::Tag(loc, word, delim) => match delim {
                    _ if esc && top.name != word => {
                        top.children.push(Node::Text(word));
                        top.children.push(Node::Text(delim.to_u8s()));
                    }
                    Delim(dtype, DelimSide::Open) => {
                        stack.push(top);
                        top = Elem {
                            name: word,
                            delim: dtype,
                            children: Vec::new(),
                            loc: loc,
                        };
                    }
                    Delim(dtype, DelimSide::Close) => {
                        if !esc {
                            top.children.push(Node::Text(word));
                        }
                        if top.delim != dtype {
                            let d = Delim(top.delim, DelimSide::Open);
                            errs.push(format!(
                                "{}: ‘{}’ doesn’t close ‘{}{}’ at {}",
                                loc,
                                String::from_utf8_lossy(delim.to_u8s()),
                                debug_utf8(top.name),
                                String::from_utf8_lossy(d.to_u8s()),
                                top.loc));
                            top.children.push(escape_delim(d));
                        } else {
                            match stack.pop() {
                                None => {
                                    // we're at root level (which is never
                                    // an escaping context), so there's
                                    // nothing to close
                                    let d = delim.to_u8s();
                                    errs.push(format!(
                                        "{}: ‘{}’ doesn’t close anything",
                                        loc, String::from_utf8_lossy(d)));
                                    top.children.push(Node::Text(d));
                                }
                                Some(mut new_top) => {
                                    new_top.children.push(Node::Elem(Elem {
                                        name: top.name,
                                        delim: top.delim,
                                        children: top.children,
                                        loc: top.loc,
                                    }));
                                    top = new_top;
                                }
                            }
                        }
                    }
                }
            }
        }
        let mut nodes = ::std::mem::replace(match stack.first_mut() {
            Some(root) => {
                let d = Delim(top.delim, DelimSide::Open).to_u8s();
                errs.push(format!(
                    "{}: ‘{}{}’ was never closed",
                    top.loc,
                    String::from_utf8_lossy(top.name),
                    String::from_utf8_lossy(d)));
                &mut root.children
            }
            None => &mut top.children,
        }, Vec::new());
        // flatten the unclosed elements into text
        for elem in stack.into_iter().chain(::std::iter::once(top)).skip(1) {
            nodes.extend(elem.into_text_nodes());
        }
        (nodes, errs)
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
    use std::io::{Write, stderr};
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        writeln!(stderr(), "need 1 command-line argument").unwrap();
        ::std::process::exit(1);
    }
    let path = &args[1];
    let s = load_file(path);
    let lexer = Lexer::new(&s, Loc::new(path));
    let (nodes, errs) = Node::parse(lexer);
    for err in &errs {
        writeln!(stderr(), "{}", err).unwrap();
    }
    print!("{}", String::from_utf8_lossy(
        &write_to_vec(&nodes as &[_], &mut NodeWriteState::Clean)));
    if !errs.is_empty() {
        ::std::process::exit(1);
    }
}
