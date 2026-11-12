///|
test "type inference of primitives and constants" {
  assert_eq(
    type_of(@syntax.Expr::BoolLit(true)),
    Ok(@syntax.Expr::Builtin(@syntax.Builtin::Bool)),
  )
  assert_eq(
    type_of(@syntax.Expr::NaturalLit(42)),
    Ok(@syntax.Expr::Builtin(@syntax.Builtin::Natural)),
  )
  assert_eq(
    type_of(@syntax.Expr::IntegerLit(42)),
    Ok(@syntax.Expr::Builtin(@syntax.Builtin::Integer)),
  )
  assert_eq(
    type_of(@syntax.Expr::text("hello")),
    Ok(@syntax.Expr::Builtin(@syntax.Builtin::Text)),
  )
  assert_eq(
    type_of(@syntax.Expr::Const(@syntax.Const::Type)),
    Ok(@syntax.Expr::Const(@syntax.Const::Kind)),
  )
  assert_eq(
    type_of(@syntax.Expr::Const(@syntax.Const::Kind)),
    Ok(@syntax.Expr::Const(@syntax.Const::Sort)),
  )
}

///|
test "type inference of functions and applications" {
  // \(x : Natural) -> x
  let id_nat = @syntax.Expr::Lam(
    "x",
    @syntax.Expr::Builtin(@syntax.Builtin::Natural),
    @syntax.Expr::Var("x", 0),
  )
  let expected_fn_ty = @syntax.Expr::Pi(
    "x",
    @syntax.Expr::Builtin(@syntax.Builtin::Natural),
    @syntax.Expr::Builtin(@syntax.Builtin::Natural),
  )
  assert_eq(type_of(id_nat), Ok(expected_fn_ty))

  // (\(x : Natural) -> x) 42 => Natural
  let app = @syntax.Expr::App(id_nat, @syntax.Expr::NaturalLit(42))
  assert_eq(type_of(app), Ok(@syntax.Expr::Builtin(@syntax.Builtin::Natural)))

  // Natural/isZero 0 => Bool
  let is_zero = @syntax.Expr::App(
    @syntax.Expr::Builtin(@syntax.Builtin::NaturalIsZero),
    @syntax.Expr::NaturalLit(0),
  )
  assert_eq(type_of(is_zero), Ok(@syntax.Expr::Builtin(@syntax.Builtin::Bool)))
}

///|
test "type inference of records and lists" {
  let rec = @syntax.Expr::RecordLit([
    ("a", @syntax.Expr::NaturalLit(1)),
    ("b", @syntax.Expr::text("test")),
  ])
  let expected_rec_ty = @syntax.Expr::RecordType([
    ("a", @syntax.Expr::Builtin(@syntax.Builtin::Natural)),
    ("b", @syntax.Expr::Builtin(@syntax.Builtin::Text)),
  ])
  assert_eq(type_of(rec), Ok(expected_rec_ty))

  // Field access: rec.a => Natural
  let field = @syntax.Expr::Field(rec, "a")
  assert_eq(type_of(field), Ok(@syntax.Expr::Builtin(@syntax.Builtin::Natural)))

  // List: [1, 2, 3] => List Natural
  let list = @syntax.Expr::ListLit(None, [
    @syntax.Expr::NaturalLit(1),
    @syntax.Expr::NaturalLit(2),
    @syntax.Expr::NaturalLit(3),
  ])
  let expected_list_ty = @syntax.Expr::App(
    @syntax.Expr::Builtin(@syntax.Builtin::List),
    @syntax.Expr::Builtin(@syntax.Builtin::Natural),
  )
  assert_eq(type_of(list), Ok(expected_list_ty))
}

///|
test "type error on mismatched types" {
  // 1 + True (ill-typed)
  let bad_plus = @syntax.Expr::BinOp(
    @syntax.BinOp::NaturalPlus,
    @syntax.Expr::NaturalLit(1),
    @syntax.Expr::BoolLit(true),
  )
  assert_true(type_of(bad_plus) is Err(_))

  // if 42 then 1 else 2 (ill-typed condition)
  let bad_if = @syntax.Expr::BoolIf(
    @syntax.Expr::NaturalLit(42),
    @syntax.Expr::NaturalLit(1),
    @syntax.Expr::NaturalLit(2),
  )
  assert_true(type_of(bad_if) is Err(_))
}

///|
test "type inference of union constructor and merge" {
  let union_ty = @syntax.Expr::UnionType([
    ("Left", Some(@syntax.Expr::Builtin(@syntax.Builtin::Natural))),
    ("Right", Some(@syntax.Expr::Builtin(@syntax.Builtin::Bool))),
  ])
  // < Left : Natural | Right : Bool >.Left : Natural -> < Left : Natural | Right : Bool >
  let ctor = @syntax.Expr::Field(union_ty, "Left")
  let expected_ctor_ty = @syntax.Expr::Pi(
    "_",
    @syntax.Expr::Builtin(@syntax.Builtin::Natural),
    union_ty,
  )
  assert_eq(type_of(ctor), Ok(expected_ctor_ty))

  // Variant: < Left : Natural | Right : Bool >.Left 5 : < Left : Natural | Right : Bool >
  let variant = @syntax.Expr::App(ctor, @syntax.Expr::NaturalLit(5))
  assert_eq(type_of(variant), Ok(union_ty))
}
