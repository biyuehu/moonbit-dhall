///|
test "to_json canonical string" {
  let rec = @syntax.Expr::RecordLit([
    ("name", @syntax.Expr::text("MoonBit")),
    ("stars", @syntax.Expr::NaturalLit(100)),
    ("open_source", @syntax.Expr::BoolLit(true)),
    (
      "tags",
      @syntax.Expr::ListLit(None, [
        @syntax.Expr::text("wasm"),
        @syntax.Expr::text("dhall"),
      ]),
    ),
  ])
  let res = expr_to_json_canonical(rec)
  assert_eq(
    res,
    Ok(
      "{\"name\":\"MoonBit\",\"open_source\":true,\"stars\":100,\"tags\":[\"wasm\",\"dhall\"]}",
    ),
  )
}

///|
test "to_json with computation" {
  // let base = 8000 in { port = base + 80, host = "localhost" }
  let expr = @syntax.Expr::Let(
    "base",
    None,
    @syntax.Expr::NaturalLit(8000),
    @syntax.Expr::RecordLit([
      (
        "port",
        @syntax.Expr::BinOp(
          @syntax.BinOp::NaturalPlus,
          @syntax.Expr::Var("base", 0),
          @syntax.Expr::NaturalLit(80),
        ),
      ),
      ("host", @syntax.Expr::text("localhost")),
    ]),
  )
  let res = expr_to_json_canonical(expr)
  assert_eq(res, Ok("{\"host\":\"localhost\",\"port\":8080}"))
}
