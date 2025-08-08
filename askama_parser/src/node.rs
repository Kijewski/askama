use std::str::{self, FromStr};

use winnow::combinator::{
    alt, cut_err, delimited, eof, fail, not, opt, peek, preceded, repeat, separated,
    separated_pair, terminated,
};
use winnow::error::ErrMode;
use winnow::stream::{Location, Stream};
use winnow::token::{any, rest, take, take_until};
use winnow::{ModalParser, Parser};

use crate::{
    ErrorContext, Expr, Filter, HashSet, InputStream, ParseResult, Span, State, Target, WithSpan,
    cut_error, filter, identifier, is_rust_keyword, keyword, skip_ws0, str_lit_without_prefix, ws,
};

#[derive(Debug, PartialEq)]
pub enum Node<'a> {
    Lit(WithSpan<Lit<'a>>),
    Comment(WithSpan<Comment<'a>>),
    Expr(Ws, WithSpan<Box<Expr<'a>>>),
    Call(WithSpan<Call<'a>>),
    Let(WithSpan<Let<'a>>),
    If(WithSpan<If<'a>>),
    Match(WithSpan<Match<'a>>),
    Loop(WithSpan<Loop<'a>>),
    Extends(WithSpan<Extends<'a>>),
    BlockDef(WithSpan<BlockDef<'a>>),
    Include(WithSpan<Include<'a>>),
    Import(WithSpan<Import<'a>>),
    Macro(WithSpan<Macro<'a>>),
    Raw(WithSpan<Raw<'a>>),
    Break(WithSpan<Ws>),
    Continue(WithSpan<Ws>),
    FilterBlock(WithSpan<FilterBlock<'a>>),
}

impl<'a> Node<'a> {
    pub(super) fn parse_template(
        i: &mut InputStream<'a>,
        s: &State<'_, '_>,
    ) -> ParseResult<'a, Vec<Box<Self>>> {
        let nodes = parse_with_unexpected_fallback(
            |i: &mut _| Self::many(i, s),
            |i: &mut _| unexpected_tag(i, s),
        )
        .parse_next(i)?;

        opt(|i: &mut _| unexpected_tag(i, s)).parse_next(i)?;
        if !i.is_empty() {
            return cut_error!(
                "cannot parse entire template\n\
                you should never encounter this error\n\
                please report this error to <https://github.com/askama-rs/askama/issues>",
                *i,
            );
        }
        Ok(nodes)
    }

    fn many(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Vec<Box<Self>>> {
        repeat(
            0..,
            alt((
                |i: &mut _| Lit::parse(i, s),
                |i: &mut _| Comment::parse(i, s),
                |i: &mut _| Self::expr(i, s),
                |i: &mut _| Self::parse(i, s),
            )),
        )
        .map(|v: Vec<_>| v)
        .parse_next(i)
    }

    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Self>> {
        let start = i.checkpoint();
        let (span, tag) = (
            s.syntax.block_start,
            peek(preceded((opt(Whitespace::parse), skip_ws0), identifier)),
        )
            .parse_next(i)?;

        let func = match tag {
            "call" => Call::parse,
            "let" | "set" => Let::parse,
            "if" => If::parse,
            "for" => Loop::parse,
            "match" => Match::parse,
            "extends" => |i: &mut InputStream<'a>, _s: &State<'_, '_>| Extends::parse(i),
            "include" => |i: &mut InputStream<'a>, _s: &State<'_, '_>| Include::parse(i),
            "import" => |i: &mut InputStream<'a>, _s: &State<'_, '_>| Import::parse(i),
            "block" => BlockDef::parse,
            "macro" => Macro::parse,
            "raw" => Raw::parse,
            "break" => Self::r#break,
            "continue" => Self::r#continue,
            "filter" => FilterBlock::parse,
            _ => {
                i.reset(&start);
                return fail.parse_next(i);
            }
        };

        let _level_guard = s.level.nest(i)?;
        let node = func(i, s)?;
        let closed = cut_node(
            None,
            alt((
                ws(eof).value(false),
                (|i: &mut _| s.tag_block_end(i)).value(true),
            )),
        )
        .parse_next(i)?;
        match closed {
            true => Ok(node),
            false => Err(ErrorContext::unclosed("block", s.syntax.block_end, span).cut()),
        }
    }

    fn r#break(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Node<'a>>> {
        let mut p = (
            opt(Whitespace::parse),
            ws(keyword("break")),
            opt(Whitespace::parse),
        );

        let start = ***i;
        let (pws, _, nws) = p.parse_next(i)?;
        if !s.is_in_loop() {
            return cut_error!("you can only `break` inside a `for` loop", start);
        }
        Ok(Box::new(Self::Break(WithSpan::new(Ws(pws, nws), start, i))))
    }

    fn r#continue(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Node<'a>>> {
        let mut p = (
            opt(Whitespace::parse),
            ws(keyword("continue")),
            opt(Whitespace::parse),
        );

        let start = ***i;
        let (pws, _, nws) = p.parse_next(i)?;
        if !s.is_in_loop() {
            return cut_error!("you can only `continue` inside a `for` loop", start);
        }
        Ok(Box::new(Self::Continue(WithSpan::new(
            Ws(pws, nws),
            start,
            i,
        ))))
    }

    fn expr(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Self>> {
        let start = ***i;
        let level = s.level;
        let (pws, expr) = preceded(
            |i: &mut _| s.tag_expr_start(i),
            cut_node(
                None,
                (
                    opt(Whitespace::parse),
                    ws(|i: &mut _| Expr::parse(i, level, false)),
                ),
            ),
        )
        .parse_next(i)?;

        let (nws, closed) = cut_node(
            None,
            (
                opt(Whitespace::parse),
                alt((
                    (|i: &mut _| s.tag_expr_end(i)).value(true),
                    ws(eof).value(false),
                )),
            ),
        )
        .parse_next(i)?;
        match closed {
            true => Ok(Box::new(Self::Expr(Ws(pws, nws), expr))),
            false => Err(ErrorContext::unclosed("expression", s.syntax.expr_end, start).cut()),
        }
    }

    #[must_use]
    pub fn span(&self) -> Span {
        match self {
            Self::Lit(span) => span.span,
            Self::Comment(span) => span.span,
            Self::Expr(_, span) => span.span,
            Self::Call(span) => span.span,
            Self::Let(span) => span.span,
            Self::If(span) => span.span,
            Self::Match(span) => span.span,
            Self::Loop(span) => span.span,
            Self::Extends(span) => span.span,
            Self::BlockDef(span) => span.span,
            Self::Include(span) => span.span,
            Self::Import(span) => span.span,
            Self::Macro(span) => span.span,
            Self::Raw(span) => span.span,
            Self::Break(span) => span.span,
            Self::Continue(span) => span.span,
            Self::FilterBlock(span) => span.span,
        }
    }
}

#[inline]
fn parse_with_unexpected_fallback<'a, O>(
    mut parser: impl ModalParser<InputStream<'a>, O, ErrorContext>,
    mut unexpected_parser: impl FnMut(&mut InputStream<'a>) -> ParseResult<'a, ()>,
) -> impl ModalParser<InputStream<'a>, O, ErrorContext> {
    #[cold]
    #[inline(never)]
    fn try_assign_fallback_error<'a>(
        i: &mut InputStream<'a>,
        unexpected_parser: &mut dyn FnMut(&mut InputStream<'a>) -> ParseResult<'a, ()>,
        err: &mut ErrMode<ErrorContext>,
    ) {
        let (ErrMode::Backtrack(err_ctx) | ErrMode::Cut(err_ctx)) = &err else {
            return;
        };
        if err_ctx.message.is_none() {
            return;
        }

        let checkpoint = i.checkpoint();
        i.input.reset_to_start();
        if take::<_, _, ()>(err_ctx.span.start).parse_next(i).is_ok()
            && let Err(better_err) = opt(unexpected_parser).parse_next(i)
            && let ErrMode::Backtrack(better_ctx) | ErrMode::Cut(better_ctx) = &better_err
            && better_ctx.message.is_some()
        {
            *err = better_err;
        }
        i.reset(&checkpoint);
    }

    move |i: &mut InputStream<'a>| {
        let mut result = parser.parse_next(i);
        if let Err(err) = &mut result {
            try_assign_fallback_error(i, &mut unexpected_parser, err);
        }
        result
    }
}

#[inline]
fn cut_node<'a, O>(
    kind: Option<&'static str>,
    inner: impl ModalParser<InputStream<'a>, O, ErrorContext>,
) -> impl ModalParser<InputStream<'a>, O, ErrorContext> {
    parse_with_unexpected_fallback(cut_err(inner), move |i: &mut _| unexpected_raw_tag(kind, i))
}

fn unexpected_tag<'a>(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, ()> {
    (
        |i: &mut _| s.tag_block_start(i),
        opt(Whitespace::parse),
        |i: &mut _| unexpected_raw_tag(None, i),
    )
        .void()
        .parse_next(i)
}

fn unexpected_raw_tag<'a>(
    kind: Option<&'static str>,
    i: &mut InputStream<'a>,
) -> ParseResult<'a, ()> {
    let (tag, span) = peek(ws(identifier.with_span())).parse_next(i)?;
    let msg = match tag {
        "end" | "elif" | "else" | "when" => match kind {
            Some(kind) => {
                format!("node `{tag}` was not expected in the current context: `{kind}` block")
            }
            None => format!("node `{tag}` was not expected in the current context"),
        },
        tag if tag.starts_with("end") => format!("unexpected closing tag `{tag}`"),
        tag => format!("unknown node `{tag}`"),
    };
    cut_error!(msg, span, i)
}

#[derive(Debug, PartialEq)]
pub struct When<'a> {
    pub ws: Ws,
    pub target: Vec<Target<'a>>,
    pub nodes: Vec<Box<Node<'a>>>,
}

impl<'a> When<'a> {
    fn r#else(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, WithSpan<Self>> {
        let mut p = (
            |i: &mut _| s.tag_block_start(i),
            opt(Whitespace::parse),
            ws(keyword("else")),
            cut_node(
                Some("match-else"),
                (
                    opt(Whitespace::parse),
                    |i: &mut _| s.tag_block_end(i),
                    cut_node(Some("match-else"), |i: &mut _| Node::many(i, s)),
                ),
            ),
        );

        let start = ***i;
        let (_, pws, _, (nws, _, nodes)) = p.parse_next(i)?;
        Ok(WithSpan::new(
            Self {
                ws: Ws(pws, nws),
                target: vec![Target::Placeholder(WithSpan::new((), start, i))],
                nodes,
            },
            start,
            i,
        ))
    }

    #[allow(clippy::self_named_constructors)]
    fn when(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, WithSpan<Self>> {
        let start = ***i;
        let endwhen = ws(terminated(
            delimited(
                |i: &mut _| s.tag_block_start(i),
                opt(Whitespace::parse),
                ws(keyword("endwhen")),
            ),
            cut_node(
                Some("match-endwhen"),
                (
                    opt(Whitespace::parse),
                    |i: &mut _| s.tag_block_end(i),
                    repeat(0.., ws(|i: &mut _| Comment::parse(i, s))).map(|()| ()),
                ),
            ),
        )
        .with_taken())
        .map(|(pws, span)| {
            // A comment node is used to pass the whitespace suppressing information to the
            // generator. This way we don't have to fix up the next `when` node or the closing
            // `endmatch`. Any whitespaces after `endwhen` are to be suppressed. Actually, they
            // don't wind up in the AST anyway.
            Box::new(Node::Comment(WithSpan::new_with_full(
                Comment {
                    ws: Ws(pws, Some(Whitespace::Suppress)),
                    content: "",
                },
                span,
            )))
        });
        let mut p = (
            |i: &mut _| s.tag_block_start(i),
            opt(Whitespace::parse),
            ws(keyword("when")),
            cut_node(
                Some("match-when"),
                (
                    separated(1.., ws(|i: &mut _| Target::parse(i, s)), '|'),
                    opt(Whitespace::parse),
                    |i: &mut _| s.tag_block_end(i),
                    cut_node(Some("match-when"), |i: &mut _| Node::many(i, s)),
                    opt(endwhen),
                ),
            ),
        );
        let (_, pws, _, (target, nws, _, mut nodes, endwhen)) = p.parse_next(i)?;
        if let Some(endwhen) = endwhen {
            nodes.push(endwhen);
        }
        Ok(WithSpan::new(
            Self {
                ws: Ws(pws, nws),
                target,
                nodes,
            },
            start,
            i,
        ))
    }
}

#[derive(Debug, PartialEq)]
pub struct Cond<'a> {
    pub ws: Ws,
    pub cond: Option<CondTest<'a>>,
    pub nodes: Vec<Box<Node<'a>>>,
}

impl<'a> Cond<'a> {
    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, WithSpan<Self>> {
        let (_, pws, (span, cond), nws, _, nodes) = (
            |i: &mut _| s.tag_block_start(i),
            opt(Whitespace::parse),
            alt((
                (ws(keyword("else").span()), opt(|i: &mut _| CondTest::parse(i, s))),
                (
                    ws(keyword("elif").span()),
                    cut_node(Some("if-elif"), |i: &mut _| {
                        CondTest::parse_cond(i, s).map(Some)
                    }),
                ),
            )),
            opt(Whitespace::parse),
            cut_node(Some("if"), |i: &mut _| s.tag_block_end(i)),
            cut_node(Some("if"), |i: &mut _| Node::many(i, s)),
        )
            .parse_next(i)?;
        Ok(WithSpan::new(
            Self {
                ws: Ws(pws, nws),
                cond,
                nodes,
            },
            span,
            i,
        ))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CondTest<'a> {
    pub target: Option<Target<'a>>,
    pub expr: WithSpan<Box<Expr<'a>>>,
    pub contains_bool_lit_or_is_defined: bool,
}

impl<'a> CondTest<'a> {
    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Self> {
        preceded(
            ws(keyword("if")),
            cut_node(Some("if"), |i: &mut _| Self::parse_cond(i, s)),
        )
        .parse_next(i)
    }

    fn parse_cond(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Self> {
        let (target, expr) = (
            opt(delimited(
                ws(alt((keyword("let"), keyword("set")))),
                ws(|i: &mut _| Target::parse(i, s)),
                ws('='),
            )),
            ws(|i: &mut InputStream<'a>| {
                let checkpoint = i.checkpoint();
                let start = ***i;

                let mut expr = Expr::parse(i, s.level, false)?;
                if let Expr::BinOp(v) = &mut *expr.inner
                    && matches!(*v.rhs.inner, Expr::Var("set" | "let"))
                {
                    let _level_guard = s.level.nest(i)?;

                    i.reset(&checkpoint);
                    i.next_slice(
                        v.rhs
                            .span
                            .offset_from(start)
                            .expect("rhs offset cannot be invalid"),
                    );

                    let start_span = ***i;
                    let new_right = Self::parse_cond(i, s)?;
                    v.rhs.inner = Box::new(Expr::LetCond(WithSpan::new(new_right, start_span, i)));
                }
                Ok(expr)
            }),
        )
            .parse_next(i)?;
        let contains_bool_lit_or_is_defined = expr.contains_bool_lit_or_is_defined();
        Ok(Self {
            target,
            expr,
            contains_bool_lit_or_is_defined,
        })
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq, Debug, Hash)]
#[cfg_attr(feature = "config", derive(serde_derive::Deserialize))]
#[cfg_attr(feature = "config", serde(field_identifier, rename_all = "lowercase"))]
pub enum Whitespace {
    #[default]
    Preserve,
    Suppress,
    Minimize,
}

impl Whitespace {
    fn parse<'i>(i: &mut InputStream<'i>) -> ParseResult<'i, Self> {
        any.verify_map(Self::parse_char).parse_next(i)
    }

    fn parse_char(c: char) -> Option<Self> {
        u8::try_from(c).ok().and_then(Self::parse_byte)
    }

    fn parse_byte(b: u8) -> Option<Whitespace> {
        match b {
            b'+' => Some(Self::Preserve),
            b'-' => Some(Self::Suppress),
            b'~' => Some(Self::Minimize),
            _ => None,
        }
    }
}

impl FromStr for Whitespace {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" | "preserve" => Ok(Whitespace::Preserve),
            "-" | "suppress" => Ok(Whitespace::Suppress),
            "~" | "minimize" => Ok(Whitespace::Minimize),
            s => Err(format!("invalid value for `whitespace`: {s:?}")),
        }
    }
}

fn check_block_start<'a>(
    i: &mut InputStream<'a>,
    start: &'a str,
    s: &State<'_, '_>,
    node: &str,
    expected: &str,
) -> ParseResult<'a, ()> {
    if i.is_empty() {
        return cut_error!(
            format!("expected `{expected}` to terminate `{node}` node, found nothing"),
            start,
        );
    }
    (|i: &mut _| s.tag_block_start(i)).parse_next(i)
}

#[derive(Debug, PartialEq)]
pub struct Loop<'a> {
    pub ws1: Ws,
    pub var: Target<'a>,
    pub iter: WithSpan<Box<Expr<'a>>>,
    pub cond: Option<WithSpan<Box<Expr<'a>>>>,
    pub body: Vec<Box<Node<'a>>>,
    pub ws2: Ws,
    pub else_nodes: Vec<Box<Node<'a>>>,
    pub ws3: Ws,
}

impl<'a> Loop<'a> {
    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Node<'a>>> {
        fn content<'a>(
            i: &mut InputStream<'a>,
            s: &State<'_, '_>,
        ) -> ParseResult<'a, Vec<Box<Node<'a>>>> {
            s.enter_loop();
            let result = (|i: &mut _| Node::many(i, s)).parse_next(i);
            s.leave_loop();
            result
        }

        let start = ***i;
        let if_cond = preceded(
            ws(keyword("if")),
            cut_node(
                Some("for-if"),
                ws(|i: &mut _| Expr::parse(i, s.level, true)),
            ),
        );

        let else_block = |i: &mut _| {
            let mut p = preceded(
                ws(keyword("else")),
                cut_node(
                    Some("for-else"),
                    (
                        opt(Whitespace::parse),
                        delimited(
                            |i: &mut _| s.tag_block_end(i),
                            |i: &mut _| Node::many(i, s),
                            |i: &mut _| s.tag_block_start(i),
                        ),
                        opt(Whitespace::parse),
                    ),
                ),
            );
            let (pws, nodes, nws) = p.parse_next(i)?;
            Ok((pws, nodes, nws))
        };

        let body_and_end = |i: &mut _| {
            let (body, (_, pws, else_block, _, nws)) = cut_node(
                Some("for"),
                (
                    |i: &mut _| content(i, s),
                    cut_node(
                        Some("for"),
                        (
                            |i: &mut _| check_block_start(i, start, s, "for", "endfor"),
                            opt(Whitespace::parse),
                            opt(else_block),
                            end_node("for", "endfor"),
                            opt(Whitespace::parse),
                        ),
                    ),
                ),
            )
            .parse_next(i)?;
            Ok((body, pws, else_block, nws))
        };

        let mut p = (
            opt(Whitespace::parse),
            ws(keyword("for").span()),
            cut_node(
                Some("for"),
                (
                    ws(|i: &mut _| Target::parse(i, s)),
                    ws(keyword("in")),
                    cut_node(
                        Some("for"),
                        (
                            ws(|i: &mut _| Expr::parse(i, s.level, true)),
                            opt(if_cond),
                            opt(Whitespace::parse),
                            |i: &mut _| s.tag_block_end(i),
                            body_and_end,
                        ),
                    ),
                ),
            ),
        );
        let (pws1, span, (var, _, (iter, cond, nws1, _, (body, pws2, else_block, nws2)))) =
            p.parse_next(i)?;
        let (nws3, else_nodes, pws3) = else_block.unwrap_or_default();
        Ok(Box::new(Node::Loop(WithSpan::new(
            Self {
                ws1: Ws(pws1, nws1),
                var,
                iter,
                cond,
                body,
                ws2: Ws(pws2, nws3),
                else_nodes,
                ws3: Ws(pws3, nws2),
            },
            span,
            i,
        ))))
    }
}

#[derive(Debug, PartialEq)]
pub struct Macro<'a> {
    pub ws1: Ws,
    pub name: &'a str,
    pub args: Vec<(&'a str, Option<WithSpan<Box<Expr<'a>>>>)>,
    pub nodes: Vec<Box<Node<'a>>>,
    pub ws2: Ws,
}

fn check_duplicated_name<'a>(
    names: &mut HashSet<&'a str>,
    arg_name: &'a str,
    i: &'a str,
) -> Result<(), crate::ParseErr<'a>> {
    if !names.insert(arg_name) {
        return cut_error!(format!("duplicated argument `{arg_name}`"), i);
    }
    Ok(())
}

impl<'a> Macro<'a> {
    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Node<'a>>> {
        let start_s = ***i;

        let (pws1, _, (name, span)) = (
            opt(Whitespace::parse),
            ws(keyword("macro")),
            cut_node(Some("macro"), ws(identifier.with_span())),
        )
            .parse_next(i)?;
        if is_rust_keyword(name) {
            return cut_error!(format!("'{name}' is not a valid name for a macro"), span, i);
        }

        let parameters_span = span.clone();
        #[allow(clippy::type_complexity)]
        let parameters =
            |i: &mut _| -> ParseResult<'_, Option<Vec<(&str, Option<WithSpan<Box<Expr<'_>>>>)>>> {
                let args = opt(preceded(
                    '(',
                    (
                        opt(terminated(
                            separated(
                                1..,
                                (
                                    ws(identifier),
                                    opt(preceded(
                                        '=',
                                        ws(|i: &mut _| Expr::parse(i, s.level, false)),
                                    )),
                                ),
                                ',',
                            ),
                            opt(','),
                        )),
                        ws(opt(')')),
                    ),
                ))
                .parse_next(i)?;
                match args {
                    Some((args, Some(_))) => Ok(args),
                    Some((_, None)) => cut_error!(
                        "expected `)` to close macro argument list",
                        parameters_span,
                        i
                    ),
                    None => Ok(None),
                }
            };

        let (params, nws1, _) = cut_node(
            Some("macro"),
            (parameters, opt(Whitespace::parse), s.syntax.block_end),
        )
        .parse_next(i)?;

        if let Some(ref params) = params {
            let mut names = HashSet::default();

            let mut iter = params.iter();
            while let Some((arg_name, default_value)) = iter.next() {
                check_duplicated_name(&mut names, arg_name, start_s)?;
                if default_value.is_some() {
                    for (new_arg_name, default_value) in iter.by_ref() {
                        check_duplicated_name(&mut names, new_arg_name, start_s)?;
                        if default_value.is_none() {
                            return cut_error!(
                                format!(
                                    "all arguments following `{arg_name}` should have a default \
                                         value, `{new_arg_name}` doesn't have a default value"
                                ),
                                start_s,
                            );
                        }
                    }
                }
            }
        }

        let mut end = cut_node(
            Some("macro"),
            (
                |i: &mut _| Node::many(i, s),
                cut_node(
                    Some("macro"),
                    (
                        |i: &mut _| check_block_start(i, start_s, s, "macro", "endmacro"),
                        opt(Whitespace::parse),
                        end_node("macro", "endmacro"),
                        cut_node(
                            Some("macro"),
                            preceded(
                                opt(|i: &mut InputStream<'a>| {
                                    let before = ***i;
                                    let end_name = ws(identifier).parse_next(i)?;
                                    check_end_name(before, name, end_name, "macro")
                                }),
                                opt(Whitespace::parse),
                            ),
                        ),
                    ),
                ),
            ),
        );
        let (contents, (_, pws2, _, nws2)) = end.parse_next(i)?;

        Ok(Box::new(Node::Macro(WithSpan::new(
            Self {
                ws1: Ws(pws1, nws1),
                name,
                args: params.unwrap_or_default(),
                nodes: contents,
                ws2: Ws(pws2, nws2),
            },
            span,
            i,
        ))))
    }
}

#[derive(Debug, PartialEq)]
pub struct FilterBlock<'a> {
    pub ws1: Ws,
    pub filters: Filter<'a>,
    pub nodes: Vec<Box<Node<'a>>>,
    pub ws2: Ws,
}

impl<'a> FilterBlock<'a> {
    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Node<'a>>> {
        let start = ***i;
        let (pws1, _, span) =
            (opt(Whitespace::parse), skip_ws0, keyword("filter").span()).parse_next(i)?;

        fn filters<'a>(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Filter<'a>> {
            let mut filter = |i: &mut InputStream<'a>| {
                preceded(
                    skip_ws0,
                    (|i: &mut InputStream<'a>| filter(i, s.level)).with_span(),
                )
                .parse_next(i)
            };

            let (mut res, span) = filter.parse_next(i)?;
            res.arguments
                .insert(0, WithSpan::new(Box::new(Expr::FilterSource), span, &*i));

            let mut level_guard = s.level.guard();
            let mut i_before = i.clone();
            while let Some((mut filter, span)) = opt(filter).parse_next(i)? {
                level_guard.nest(&i_before)?;
                filter
                    .arguments
                    .insert(0, WithSpan::new(Box::new(Expr::Filter(res)), span, &*i));
                res = filter;
                i_before = i.clone();
            }
            Ok(res)
        }

        let ((filters, nws1, _), nodes, (_, pws2, _, nws2)) = (
            cut_node(
                Some("filter"),
                (
                    ws(|i: &mut _| filters(i, s)),
                    opt(Whitespace::parse),
                    |i: &mut _| s.tag_block_end(i),
                ),
            ),
            cut_node(Some("filter"), |i: &mut _| Node::many(i, s)),
            cut_node(
                Some("filter"),
                (
                    |i: &mut _| check_block_start(i, start, s, "filter", "endfilter"),
                    opt(Whitespace::parse),
                    end_node("filter", "endfilter"),
                    opt(Whitespace::parse),
                ),
            ),
        )
            .parse_next(i)?;

        Ok(Box::new(Node::FilterBlock(WithSpan::new(
            Self {
                ws1: Ws(pws1, nws1),
                filters,
                nodes,
                ws2: Ws(pws2, nws2),
            },
            span,
            i,
        ))))
    }
}

#[derive(Debug, PartialEq)]
pub struct Import<'a> {
    pub ws: Ws,
    pub path: &'a str,
    pub scope: &'a str,
}

impl<'a> Import<'a> {
    fn parse(i: &mut InputStream<'a>) -> ParseResult<'a, Box<Node<'a>>> {
        let mut p = (
            opt(Whitespace::parse),
            ws(keyword("import").span()),
            cut_node(
                Some("import"),
                (
                    ws(str_lit_without_prefix),
                    ws(keyword("as")),
                    cut_node(Some("import"), (ws(identifier), opt(Whitespace::parse))),
                ),
            ),
        );
        let (pws, span, (path, _, (scope, nws))) = p.parse_next(i)?;
        Ok(Box::new(Node::Import(WithSpan::new(
            Self {
                ws: Ws(pws, nws),
                path,
                scope,
            },
            span,
            i,
        ))))
    }
}

#[derive(Debug, PartialEq)]
pub struct Call<'a> {
    pub ws1: Ws,
    pub caller_args: Vec<&'a str>,
    pub scope: Option<&'a str>,
    pub name: &'a str,
    pub args: Vec<WithSpan<Box<Expr<'a>>>>,
    pub nodes: Vec<Box<Node<'a>>>,
    pub ws2: Ws,
}

impl<'a> Call<'a> {
    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Node<'a>>> {
        let start_s = ***i;

        let (pws, span) = (opt(Whitespace::parse), ws(keyword("call").span())).parse_next(i)?;

        let span_parameters = span.clone();
        let parameters = |i: &mut _| -> ParseResult<'_, Option<Vec<&str>>> {
            let args = opt(preceded(
                '(',
                (
                    opt(terminated(separated(0.., ws(identifier), ','), opt(','))),
                    ws(opt(')')),
                ),
            ))
            .parse_next(i)?;

            match args {
                Some((args, Some(_))) => Ok(args),
                Some((_, None)) => cut_error!(
                    "expected `)` to close call argument list",
                    span_parameters.clone(),
                    i
                ),
                None => Ok(None),
            }
        };
        let mut p = (
            parameters,
            cut_node(
                Some("call"),
                (
                    opt((ws(identifier), ws("::"))),
                    ws(identifier),
                    opt(ws(|nested: &mut _| Expr::arguments(nested, s.level))),
                    opt(Whitespace::parse),
                    |i: &mut _| s.tag_block_end(i),
                ),
            ),
        );

        let (call_args, (scope, name, args, nws, _)) = p.parse_next(i)?;
        let scope = scope.map(|(scope, _)| scope);
        let args = args.unwrap_or_default();
        let mut end = cut_node(
            Some("call"),
            (
                |i: &mut _| Node::many(i, s),
                cut_node(
                    Some("call"),
                    (
                        |i: &mut _| check_block_start(i, start_s, s, "call", "endcall"),
                        opt(Whitespace::parse),
                        end_node("call", "endcall"),
                        opt(Whitespace::parse),
                    ),
                ),
            ),
        );
        let (nodes, (_, pws2, _, nws2)) = end.parse_next(i)?;

        Ok(Box::new(Node::Call(WithSpan::new(
            Self {
                ws1: Ws(pws, nws),
                caller_args: call_args.unwrap_or_default(),
                scope,
                name,
                args,
                nodes,
                ws2: Ws(pws2, nws2),
            },
            span,
            i,
        ))))
    }
}

#[derive(Debug, PartialEq)]
pub struct Match<'a> {
    pub ws1: Ws,
    pub expr: WithSpan<Box<Expr<'a>>>,
    pub arms: Vec<WithSpan<When<'a>>>,
    pub ws2: Ws,
}

impl<'a> Match<'a> {
    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Node<'a>>> {
        let start = ***i;
        let mut p = (
            opt(Whitespace::parse),
            ws(keyword("match").span()),
            cut_node(
                Some("match"),
                (
                    ws(|i: &mut _| Expr::parse(i, s.level, false)),
                    opt(Whitespace::parse),
                    |i: &mut _| s.tag_block_end(i),
                    cut_node(
                        Some("match"),
                        (
                            ws(repeat(0.., ws(|i: &mut _| Comment::parse(i, s)))).map(|()| ()),
                            repeat(0.., |i: &mut _| When::when(i, s)).map(|v: Vec<_>| v),
                            cut_node(
                                Some("match"),
                                (
                                    opt(|i: &mut _| When::r#else(i, s)),
                                    cut_node(
                                        Some("match"),
                                        (
                                            ws(|i: &mut _| {
                                                check_block_start(i, start, s, "match", "endmatch")
                                            }),
                                            opt(Whitespace::parse),
                                            end_node("match", "endmatch"),
                                            opt(Whitespace::parse),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        );
        let (pws1, span, (expr, nws1, _, (_, mut arms, (else_arm, (_, pws2, _, nws2))))) =
            p.parse_next(i)?;

        if let Some(arm) = else_arm {
            arms.push(arm);
        }
        if arms.is_empty() {
            return cut_error!(
                "`match` nodes must contain at least one `when` node and/or an `else` case",
                span,
                i,
            );
        }

        Ok(Box::new(Node::Match(WithSpan::new(
            Self {
                ws1: Ws(pws1, nws1),
                expr,
                arms,
                ws2: Ws(pws2, nws2),
            },
            span,
            i,
        ))))
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockDef<'a> {
    pub ws1: Ws,
    pub name: &'a str,
    pub nodes: Vec<Box<Node<'a>>>,
    pub ws2: Ws,
}

impl<'a> BlockDef<'a> {
    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Node<'a>>> {
        let start_s = ***i;
        let mut start = (
            opt(Whitespace::parse),
            ws(keyword("block")),
            cut_node(
                Some("block"),
                (
                    ws(identifier.with_span()),
                    opt(Whitespace::parse),
                    |i: &mut _| s.tag_block_end(i),
                ),
            ),
        );
        let (pws1, _, ((name, span), nws1, _)) = start.parse_next(i)?;

        let mut end = cut_node(
            Some("block"),
            (
                |i: &mut _| Node::many(i, s),
                cut_node(
                    Some("block"),
                    (
                        |i: &mut _| check_block_start(i, start_s, s, "block", "endblock"),
                        opt(Whitespace::parse),
                        end_node("block", "endblock"),
                        cut_node(
                            Some("block"),
                            (
                                opt(|i: &mut InputStream<'a>| {
                                    let before = ***i;
                                    let end_name = ws(identifier).parse_next(i)?;
                                    check_end_name(before, name, end_name, "block")
                                }),
                                opt(Whitespace::parse),
                            ),
                        ),
                    ),
                ),
            ),
        );
        let (nodes, (_, pws2, _, (_, nws2))) = end.parse_next(i)?;

        Ok(Box::new(Node::BlockDef(WithSpan::new(
            BlockDef {
                ws1: Ws(pws1, nws1),
                name,
                nodes,
                ws2: Ws(pws2, nws2),
            },
            span,
            i,
        ))))
    }
}

fn check_end_name<'a>(
    before: &'a str,
    name: &'a str,
    end_name: &'a str,
    kind: &str,
) -> ParseResult<'a> {
    if name == end_name {
        return Ok(end_name);
    }

    cut_error!(
        match name.is_empty() && !end_name.is_empty() {
            true => format!("unexpected name `{end_name}` in `end{kind}` tag for unnamed `{kind}`"),
            false => format!("expected name `{name}` in `end{kind}` tag, found `{end_name}`"),
        },
        before,
    )
}

#[derive(Debug, PartialEq)]
pub struct Lit<'a> {
    pub lws: &'a str,
    pub val: &'a str,
    pub rws: &'a str,
}

impl<'a> Lit<'a> {
    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Node<'a>>> {
        let content = take_until(
            ..,
            (
                s.syntax.block_start,
                s.syntax.comment_start,
                s.syntax.expr_start,
            ),
        );
        let (content, span) = preceded(not(eof), alt((content, rest)))
            .verify(|s: &str| !s.is_empty())
            .with_span()
            .parse_next(i)?;
        Ok(Box::new(Node::Lit(WithSpan::new(
            Self::split_ws_parts(content),
            span,
            i,
        ))))
    }

    pub(crate) fn split_ws_parts(s: &'a str) -> Self {
        let trimmed_start = s.trim_ascii_start();
        let len_start = s.len() - trimmed_start.len();
        let trimmed = trimmed_start.trim_ascii_end();
        Self {
            lws: &s[..len_start],
            val: trimmed,
            rws: &trimmed_start[trimmed.len()..],
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Raw<'a> {
    pub ws1: Ws,
    pub lit: WithSpan<Lit<'a>>,
    pub ws2: Ws,
}

impl<'a> Raw<'a> {
    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Node<'a>>> {
        fn endraw<'a>(
            i: &mut InputStream<'a>,
            s: &State<'_, '_>,
        ) -> ParseResult<'a, (Ws, WithSpan<Lit<'a>>)> {
            let start = i.current_token_start();
            loop {
                // find the string "endraw", strip any spaces before it, and look if there is a `{%`
                let inner = terminated(take_until(.., "endraw"), "endraw").parse_next(i)?;

                let mut inner = inner.trim_ascii_end();
                let pws = Whitespace::parse_char(inner.chars().next_back().unwrap_or_default());
                if pws.is_some() {
                    inner = &inner[..inner.len() - 1];
                }
                let Some(inner) = inner.strip_suffix(s.syntax.block_start) else {
                    continue;
                };
                let span = start..start + inner.len();

                // We found `{% endraw`. Do we find `%}`, too?
                skip_ws0(i)?;
                let i_before_nws = *i;
                let nws = opt(Whitespace::parse).parse_next(i)?;
                if opt(peek(s.syntax.block_end)).parse_next(i)?.is_none() {
                    *i = i_before_nws; // `block_start` might start with the `nws` character
                    continue;
                }

                return Ok((
                    Ws(pws, nws),
                    WithSpan::new(Lit::split_ws_parts(inner), span, i),
                ));
            }
        }

        let mut p = (
            opt(Whitespace::parse),
            ws(keyword("raw").span()),
            cut_node(
                Some("raw"),
                separated_pair(
                    opt(Whitespace::parse),
                    |i: &mut _| s.tag_block_end(i),
                    |i: &mut _| endraw(i, s),
                ),
            ),
        );
        let (pws, span, (nws, (ws2, lit))) = p.parse_next(i)?;
        let ws1 = Ws(pws, nws);
        Ok(Box::new(Node::Raw(WithSpan::new(
            Self { ws1, lit, ws2 },
            span,
            i,
        ))))
    }
}

#[derive(Debug, PartialEq)]
pub struct Let<'a> {
    pub ws: Ws,
    pub var: Target<'a>,
    pub val: Option<WithSpan<Box<Expr<'a>>>>,
    pub is_mutable: bool,
}

impl<'a> Let<'a> {
    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Node<'a>>> {
        let mut p = (
            opt(Whitespace::parse),
            ws(alt((keyword("let"), keyword("set"))).span()),
            ws(opt(keyword("mut").span())),
            cut_node(
                Some("let"),
                (
                    ws((|i: &mut _| Target::parse(i, s)).with_span()),
                    opt(preceded(
                        ws('='),
                        ws(|i: &mut _| Expr::parse(i, s.level, false)),
                    )),
                    opt(Whitespace::parse),
                ),
            ),
        );
        let (pws, span, is_mut, ((var, var_span), val, nws)) = p.parse_next(i)?;

        if val.is_none()
            && let Some(kind) = match &var {
                Target::Name(_) => None,
                Target::Tuple(..) => Some("a tuple"),
                Target::Array(..) => Some("an array"),
                Target::Struct(..) => Some("a struct"),
                Target::NumLit(..)
                | Target::StrLit(..)
                | Target::CharLit(..)
                | Target::BoolLit(..) => Some("a literal"),
                Target::Path(..) => Some("a path or enum variant"),
                Target::OrChain(..) | Target::Placeholder(..) | Target::Rest(..) => {
                    Some("a pattern")
                }
            }
        {
            return cut_error!(
                format!(
                    "when you forward-define a variable, you cannot use {kind} in place of \
                         a variable name"
                ),
                var_span,
                i,
            );
        }

        if let Some(mut_span) = &is_mut
            && !matches!(var, Target::Name(_))
        {
            return cut_error!(
                "you can only use the `mut` keyword with a variable name",
                mut_span.clone(),
                i,
            );
        }

        Ok(Box::new(Node::Let(WithSpan::new(
            Let {
                ws: Ws(pws, nws),
                var,
                val,
                is_mutable: is_mut.is_some(),
            },
            span,
            i,
        ))))
    }
}

#[derive(Debug, PartialEq)]
pub struct If<'a> {
    pub ws: Ws,
    pub branches: Vec<WithSpan<Cond<'a>>>,
}

impl<'a> If<'a> {
    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Node<'a>>> {
        let start = ***i;

        let end_if = cut_node(
            Some("if"),
            (
                |i: &mut _| check_block_start(i, start, s, "if", "endif"),
                opt(Whitespace::parse),
                end_node("if", "endif"),
                opt(Whitespace::parse),
            ),
        );
        let p = (
            opt(Whitespace::parse),
            (|i: &mut _| CondTest::parse(i, s)).with_span(),
            cut_node(
                Some("if"),
                (
                    opt(Whitespace::parse),
                    |i: &mut _| s.tag_block_end(i),
                    cut_node(
                        Some("if"),
                        (
                            |i: &mut _| Node::many(i, s),
                            repeat(0.., |i: &mut _| Cond::parse(i, s)).map(|v: Vec<_>| v),
                            end_if,
                        ),
                    ),
                ),
            ),
        );

        let ((pws1, (cond, cond_span), (nws1, _, (nodes, elifs, (_, pws2, _, nws2)))), span) =
            p.with_span().parse_next(i)?;
        let mut branches = vec![WithSpan::new(
            Cond {
                ws: Ws(pws1, nws1),
                cond: Some(cond),
                nodes,
            },
            cond_span,
            &*i,
        )];
        branches.extend(elifs);

        Ok(Box::new(Node::If(WithSpan::new(
            Self {
                ws: Ws(pws2, nws2),
                branches,
            },
            span,
            i,
        ))))
    }
}

#[derive(Debug, PartialEq)]
pub struct Include<'a> {
    pub ws: Ws,
    pub path: &'a str,
}

impl<'a> Include<'a> {
    fn parse(i: &mut InputStream<'a>) -> ParseResult<'a, Box<Node<'a>>> {
        let p = (
            opt(Whitespace::parse),
            ws(keyword("include")),
            cut_node(
                Some("include"),
                (ws(str_lit_without_prefix), opt(Whitespace::parse)),
            ),
        );
        let ((pws, _, (path, nws)), span) = p.with_span().parse_next(i)?;
        Ok(Box::new(Node::Include(WithSpan::new(
            Self {
                ws: Ws(pws, nws),
                path,
            },
            span,
            i,
        ))))
    }
}

#[derive(Debug, PartialEq)]
pub struct Extends<'a> {
    pub path: &'a str,
}

impl<'a> Extends<'a> {
    fn parse(i: &mut InputStream<'a>) -> ParseResult<'a, Box<Node<'a>>> {
        let p = preceded(
            (opt(Whitespace::parse), ws(keyword("extends"))),
            cut_node(
                Some("extends"),
                terminated(ws(str_lit_without_prefix), opt(Whitespace::parse)),
            ),
        );
        let (path, span) = p.with_span().parse_next(i)?;
        Ok(Box::new(Node::Extends(WithSpan::new(
            Self { path },
            span,
            i,
        ))))
    }
}

#[derive(Debug, PartialEq)]
pub struct Comment<'a> {
    pub ws: Ws,
    pub content: &'a str,
}

impl<'a> Comment<'a> {
    fn parse(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, Box<Node<'a>>> {
        fn content<'a>(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a, ()> {
            let mut depth = 0usize;
            loop {
                take_until(.., (s.syntax.comment_start, s.syntax.comment_end)).parse_next(i)?;
                let is_open = opt(s.syntax.comment_start).parse_next(i)?.is_some();
                if is_open {
                    // cannot overflow: `i` cannot be longer than `isize::MAX`, cf. [std::alloc::Layout]
                    depth += 1;
                } else if let Some(new_depth) = depth.checked_sub(1) {
                    s.tag_comment_end(i)?;
                    depth = new_depth;
                } else {
                    return Ok(());
                }
            }
        }

        fn comment<'a>(i: &mut InputStream<'a>, s: &State<'_, '_>) -> ParseResult<'a> {
            let start = s.syntax.comment_start.span().parse_next(i)?;
            let mut content = opt(terminated(
                (|i: &mut _| content(i, s)).take(),
                |i: &mut _| s.tag_comment_end(i),
            ));
            let Some(content) = content.parse_next(i)? else {
                return Err(ErrorContext::unclosed(
                    "comment",
                    s.syntax.comment_end,
                    Span::new(start, &*i),
                )
                .cut());
            };
            Ok(content)
        }

        let (content, span) = (|i: &mut _| comment(i, s)).with_span().parse_next(i)?;
        let ws = match content.as_bytes() {
            &[b'-' | b'+' | b'~'] => {
                return cut_error!(
                    format!(
                        "ambiguous whitespace stripping\n\
                     use `{}{content} {content}{}` to apply the same whitespace stripping on both \
                     sides",
                        s.syntax.comment_start, s.syntax.comment_end,
                    ),
                    span,
                    i,
                );
            }
            &[pws, .., nws] => Ws(Whitespace::parse_byte(pws), Whitespace::parse_byte(nws)),
            _ => Ws(None, None),
        };
        Ok(Box::new(Node::Comment(WithSpan::new(
            Self { ws, content },
            span,
            i,
        ))))
    }
}

/// First field is "minus/plus sign was used on the left part of the item".
///
/// Second field is "minus/plus sign was used on the right part of the item".
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Ws(pub Option<Whitespace>, pub Option<Whitespace>);

fn end_node<'a, 'g: 'a>(
    node: &'g str,
    expected: &'g str,
) -> impl ModalParser<InputStream<'a>, &'a str, ErrorContext> + 'g {
    move |i: &mut InputStream<'a>| {
        let start = i.checkpoint();
        let (actual, span) = ws(identifier.with_span()).parse_next(i)?;
        if actual == expected {
            Ok(actual)
        } else if actual.starts_with("end") {
            i.reset(&start);
            cut_error!(
                format!("expected `{expected}` to terminate `{node}` node, found `{actual}`"),
                span,
                i,
            )
        } else {
            i.reset(&start);
            fail.parse_next(i)
        }
    }
}
