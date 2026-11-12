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

///|
test "eval list indexed" {
  let list = @syntax.Expr::ListLit(None, [
    @syntax.Expr::text("a"),
    @syntax.Expr::text("b"),
  ])
  let indexed_fn = @syntax.Expr::Builtin(@syntax.Builtin::ListIndexed)
  let call = @syntax.Expr::App(
    @syntax.Expr::App(indexed_fn, @syntax.Expr::Builtin(@syntax.Builtin::Text)),
    list,
  )
  let expected = @syntax.Expr::ListLit(None, [
    @syntax.Expr::RecordLit([
      ("index", @syntax.Expr::NaturalLit(0)),
      ("value", @syntax.Expr::text("a")),
    ]),
    @syntax.Expr::RecordLit([
      ("index", @syntax.Expr::NaturalLit(1)),
      ("value", @syntax.Expr::text("b")),
    ]),
  ])
  assert_eq(eval(call), expected)
}

///|
test "eval natural build" {
  // Natural/build (\(natural : Type) -> \(succ : natural -> natural) -> \(zero : natural) -> succ (succ zero)) => 2
  let build_fn = @syntax.Expr::Builtin(@syntax.Builtin::NaturalBuild)
  let builder = @syntax.Expr::Lam(
    "natural",
    @syntax.Expr::Const(@syntax.Const::Type),
    @syntax.Expr::Lam(
      "succ",
      @syntax.Expr::arrow(
        @syntax.Expr::Var("natural", 0),
        @syntax.Expr::Var("natural", 0),
      ),
      @syntax.Expr::Lam(
        "zero",
        @syntax.Expr::Var("natural", 0),
        @syntax.Expr::App(
          @syntax.Expr::Var("succ", 0),
          @syntax.Expr::App(
            @syntax.Expr::Var("succ", 0),
            @syntax.Expr::Var("zero", 0),
          ),
        ),
      ),
    ),
  )
  assert_eq(
    eval(@syntax.Expr::App(build_fn, builder)),
    @syntax.Expr::NaturalLit(2),
  )
}

///|
test "eval merge custom union" {
  let union_ty = @syntax.Expr::UnionType([
    ("Left", Some(@syntax.Expr::Builtin(@syntax.Builtin::Natural))),
    ("Right", Some(@syntax.Expr::Builtin(@syntax.Builtin::Bool))),
  ])
  let handlers = @syntax.Expr::RecordLit([
    (
      "Left",
      @syntax.Expr::Lam(
        "x",
        @syntax.Expr::Builtin(@syntax.Builtin::Natural),
        @syntax.Expr::BinOp(
          @syntax.BinOp::NaturalPlus,
          @syntax.Expr::Var("x", 0),
          @syntax.Expr::NaturalLit(10),
        ),
      ),
    ),
    (
      "Right",
      @syntax.Expr::Lam(
        "b",
        @syntax.Expr::Builtin(@syntax.Builtin::Bool),
        @syntax.Expr::NaturalLit(0),
      ),
    ),
  ])
  let variant = @syntax.Expr::App(
    @syntax.Expr::Field(union_ty, "Left"),
    @syntax.Expr::NaturalLit(5),
  )
  let merge_expr = @syntax.Expr::Merge(handlers, variant, None)
  assert_eq(eval(merge_expr), @syntax.Expr::NaturalLit(15))
}
