use askama::Template;

#[derive(Template)]
#[template(ext = "txt", source = "{{ a == b != c }}")]
struct EqNe {
    a: usize,
    b: usize,
    c: usize,
}

#[derive(Template)]
#[template(ext = "txt", source = "{{ a <= b < c }}")]
struct Between {
    a: usize,
    b: usize,
    c: usize,
}

#[derive(Template)]
#[template(ext = "txt", source = "{{ ((a == b) == c) == d == e }}")]
struct ThreeTimesOk {
    a: usize,
    b: usize,
    c: usize,
    d: usize,
    e: usize,
}

#[derive(Template)]
#[template(ext = "txt", source = "{{ a == (b == (c == d == e)) }}")]
struct ThreeTimesOk2 {
    a: usize,
    b: usize,
    c: usize,
    d: usize,
    e: usize,
}

fn main() {}
