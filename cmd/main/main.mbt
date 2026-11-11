///|
/// CLI Entry point for moonbit-dhall
fn main {
  println("== moonbit-dhall demo ==")

  // Define a Dhall configuration expression:
  // let base_port = 8000
  // in {
  //   server = "127.0.0.1",
  //   port = base_port + 80,
  //   endpoints = ["/health", "/api/v1"],
  //   debug = True
  // }
  let config_expr = @syntax.Expr::Let(
    "base_port",
    Some(@syntax.Expr::Builtin(@syntax.Builtin::Natural)),
    @syntax.Expr::NaturalLit(8000),
    @syntax.Expr::RecordLit([
      ("server", @syntax.Expr::text("127.0.0.1")),
      (
        "port",
        @syntax.Expr::BinOp(
          @syntax.BinOp::NaturalPlus,
          @syntax.Expr::Var("base_port", 0),
          @syntax.Expr::NaturalLit(80),
        ),
      ),
      (
        "endpoints",
        @syntax.Expr::ListLit(None, [
          @syntax.Expr::text("/health"),
          @syntax.Expr::text("/api/v1"),
        ]),
      ),
      ("debug", @syntax.Expr::BoolLit(true)),
    ]),
  )

  println("\n[Dhall Source]")
  println(config_expr.to_string())

  let normalized = @lib.eval(config_expr)
  println("\n[Normalized Dhall (Beta-reduced)]")
  println(normalized.to_string())

  println("\n[Exported Canonical JSON]")
  match @lib.expr_to_json_canonical(config_expr) {
    Ok(json_str) => println(json_str)
    Err(err) => println("Error exporting to JSON: " + err)
  }
}
