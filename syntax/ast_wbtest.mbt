///|
test "pretty print primitives" {
  assert_eq(Expr::BoolLit(true).to_string(), "True")
  assert_eq(Expr::BoolLit(false).to_string(), "False")
  assert_eq(Expr::NaturalLit(42).to_string(), "42")
  assert_eq(Expr::IntegerLit(42).to_string(), "+42")
  assert_eq(Expr::IntegerLit(-5).to_string(), "-5")
  assert_eq(Expr::Const(Const::Type).to_string(), "Type")
  assert_eq(Expr::Builtin(Builtin::NaturalFold).to_string(), "Natural/fold")
}

///|
test "pretty print functions and let" {
  let lam = Expr::Lam("x", Expr::Builtin(Builtin::Natural), Expr::Var("x", 0))
  assert_eq(lam.to_string(), "\\(x : Natural) -> x")

  let let_expr = Expr::Let(
    "a",
    Some(Expr::Builtin(Builtin::Natural)),
    Expr::NaturalLit(1),
    Expr::Var("a", 0),
  )
  assert_eq(let_expr.to_string(), "let a : Natural = 1 in a")
}

///|
test "pretty print record and list" {
  let rec = Expr::RecordLit([
    ("name", Expr::text("Alice")),
    ("age", Expr::NaturalLit(30)),
  ])
  assert_eq(rec.to_string(), "{ name = \"Alice\", age = 30 }")

  let list = Expr::ListLit(None, [Expr::NaturalLit(1), Expr::NaturalLit(2)])
  assert_eq(list.to_string(), "[1, 2]")
}
