///|
test "parse literals and primitives" {
  assert_eq(parse("True"), Ok(Expr::BoolLit(true)))
  assert_eq(parse("False"), Ok(Expr::BoolLit(false)))
  assert_eq(parse("42"), Ok(Expr::NaturalLit(42)))
  assert_eq(parse("+42"), Ok(Expr::IntegerLit(42)))
  assert_eq(parse("-42"), Ok(Expr::IntegerLit(-42)))
  assert_eq(parse("\"hello world\""), Ok(Expr::text("hello world")))
  assert_eq(parse("Type"), Ok(Expr::Const(Const::Type)))
  assert_eq(parse("Natural/fold"), Ok(Expr::Builtin(Builtin::NaturalFold)))
}

///|
test "parse record and list" {
  let rec = parse("{ a = 1, b = \"hi\" }")
  let expected_rec = Expr::RecordLit([
    ("a", Expr::NaturalLit(1)),
    ("b", Expr::text("hi")),
  ])
  assert_eq(rec, Ok(expected_rec))

  assert_eq(parse("{=}"), Ok(Expr::RecordLit([])))

  let list = parse("[1, 2, 3]")
  let expected_list = Expr::ListLit((None : Expr?), [
    Expr::NaturalLit(1),
    Expr::NaturalLit(2),
    Expr::NaturalLit(3),
  ])
  assert_eq(list, Ok(expected_list))
}

///|
test "parse functions and let" {
  let lam = parse("\\(x : Natural) -> x")
  let expected_lam = Expr::Lam(
    "x",
    Expr::Builtin(Builtin::Natural),
    Expr::Var("x", 0),
  )
  assert_eq(lam, Ok(expected_lam))

  let let_expr = parse("let x = 10 in x + 5")
  let expected_let = Expr::Let(
    "x",
    (None : Expr?),
    Expr::NaturalLit(10),
    Expr::BinOp(BinOp::NaturalPlus, Expr::Var("x", 0), Expr::NaturalLit(5)),
  )
  assert_eq(let_expr, Ok(expected_let))
}

///|
test "parse operator precedence and application" {
  // 1 + 2 * 3 should parse as 1 + (2 * 3) because * has higher precedence than +
  let expr = parse("1 + 2 * 3")
  let expected = Expr::BinOp(
    BinOp::NaturalPlus,
    Expr::NaturalLit(1),
    Expr::BinOp(BinOp::NaturalTimes, Expr::NaturalLit(2), Expr::NaturalLit(3)),
  )
  assert_eq(expr, Ok(expected))

  // Field access: r.field
  let field = parse("r.name")
  assert_eq(field, Ok(Expr::Field(Expr::Var("r", 0), "name")))
}
