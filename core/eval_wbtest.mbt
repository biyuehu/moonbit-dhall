///|
test "eval let expression" {
  // let x = 10 in x + 5
  let expr = @syntax.Expr::Let(
    "x",
    None,
    @syntax.Expr::NaturalLit(10),
    @syntax.Expr::BinOp(
      @syntax.BinOp::NaturalPlus,
      @syntax.Expr::Var("x", 0),
      @syntax.Expr::NaturalLit(5),
    ),
  )
  let result = eval(expr)
  assert_eq(result, @syntax.Expr::NaturalLit(15))
}

///|
test "eval lambda and application" {
  // (\(x : Natural) -> x * 2) 21
  let lam = @syntax.Expr::Lam(
    "x",
    @syntax.Expr::Builtin(@syntax.Builtin::Natural),
    @syntax.Expr::BinOp(
      @syntax.BinOp::NaturalTimes,
      @syntax.Expr::Var("x", 0),
      @syntax.Expr::NaturalLit(2),
    ),
  )
  let app = @syntax.Expr::App(lam, @syntax.Expr::NaturalLit(21))
  assert_eq(eval(app), @syntax.Expr::NaturalLit(42))
}

///|
test "eval natural builtins" {
  // Natural/isZero 0 => True, Natural/isZero 5 => False
  let is_zero = @syntax.Expr::Builtin(@syntax.Builtin::NaturalIsZero)
  assert_eq(
    eval(@syntax.Expr::App(is_zero, @syntax.Expr::NaturalLit(0))),
    @syntax.Expr::BoolLit(true),
  )
  assert_eq(
    eval(@syntax.Expr::App(is_zero, @syntax.Expr::NaturalLit(5))),
    @syntax.Expr::BoolLit(false),
  )

  // Natural/subtract 3 10 => 7
  let sub = @syntax.Expr::Builtin(@syntax.Builtin::NaturalSubtract)
  let sub_3_10 = @syntax.Expr::App(
    @syntax.Expr::App(sub, @syntax.Expr::NaturalLit(3)),
    @syntax.Expr::NaturalLit(10),
  )
  assert_eq(eval(sub_3_10), @syntax.Expr::NaturalLit(7))
}

///|
test "eval natural fold" {
  // Natural/fold 3 Natural (\(x : Natural) -> x + 10) 0 => 30
  let fold = @syntax.Expr::Builtin(@syntax.Builtin::NaturalFold)
  let succ = @syntax.Expr::Lam(
    "x",
    @syntax.Expr::Builtin(@syntax.Builtin::Natural),
    @syntax.Expr::BinOp(
      @syntax.BinOp::NaturalPlus,
      @syntax.Expr::Var("x", 0),
      @syntax.Expr::NaturalLit(10),
    ),
  )
  let call = @syntax.Expr::App(
    @syntax.Expr::App(
      @syntax.Expr::App(
        @syntax.Expr::App(fold, @syntax.Expr::NaturalLit(3)),
        @syntax.Expr::Builtin(@syntax.Builtin::Natural),
      ),
      succ,
    ),
    @syntax.Expr::NaturalLit(0),
  )
  assert_eq(eval(call), @syntax.Expr::NaturalLit(30))
}

///|
test "eval record operations" {
  // { a = 1, b = 2 }.a => 1
  let rec = @syntax.Expr::RecordLit([
    ("a", @syntax.Expr::NaturalLit(1)),
    ("b", @syntax.Expr::NaturalLit(2)),
  ])
  let field = @syntax.Expr::Field(rec, "a")
  assert_eq(eval(field), @syntax.Expr::NaturalLit(1))

  // { a = 1, b = 2 } // { b = 99, c = 3 }
  let right = @syntax.Expr::RecordLit([
    ("b", @syntax.Expr::NaturalLit(99)),
    ("c", @syntax.Expr::NaturalLit(3)),
  ])
  let prefer = @syntax.Expr::BinOp(@syntax.BinOp::Prefer, rec, right)
  let expected = @syntax.Expr::RecordLit([
    ("a", @syntax.Expr::NaturalLit(1)),
    ("b", @syntax.Expr::NaturalLit(99)),
    ("c", @syntax.Expr::NaturalLit(3)),
  ])
  assert_eq(eval(prefer), expected)
}

///|
test "eval list operations" {
  let list = @syntax.Expr::ListLit(None, [
    @syntax.Expr::NaturalLit(1),
    @syntax.Expr::NaturalLit(2),
    @syntax.Expr::NaturalLit(3),
  ])

  // List/length Natural list => 3
  let len_fn = @syntax.Expr::Builtin(@syntax.Builtin::ListLength)
  let len_call = @syntax.Expr::App(
    @syntax.Expr::App(len_fn, @syntax.Expr::Builtin(@syntax.Builtin::Natural)),
    list,
  )
  assert_eq(eval(len_call), @syntax.Expr::NaturalLit(3))

  // List/reverse Natural list => [3, 2, 1]
  let rev_fn = @syntax.Expr::Builtin(@syntax.Builtin::ListReverse)
  let rev_call = @syntax.Expr::App(
    @syntax.Expr::App(rev_fn, @syntax.Expr::Builtin(@syntax.Builtin::Natural)),
    list,
  )
  let expected_rev = @syntax.Expr::ListLit(None, [
    @syntax.Expr::NaturalLit(3),
    @syntax.Expr::NaturalLit(2),
    @syntax.Expr::NaturalLit(1),
  ])
  assert_eq(eval(rev_call), expected_rev)
}
