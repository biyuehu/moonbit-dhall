///|
/// Evaluates and normalizes a Dhall expression to its normal form (β-normalization)
pub fn eval(expr : @syntax.Expr) -> @syntax.Expr {
  @core.eval(expr)
}

///|
/// Converts a Dhall expression to MoonBit's JSON AST (`Json`)
pub fn expr_to_json(expr : @syntax.Expr) -> Result[Json, String] {
  @export_json.expr_to_json(expr)
}

///|
/// Formats a Dhall expression as a canonical JSON string with sorted keys
pub fn expr_to_json_canonical(expr : @syntax.Expr) -> Result[String, String] {
  @export_json.expr_to_json_canonical(expr)
}

///|
/// Checks if two Dhall expressions are α-equivalent
pub fn alpha_eq(a : @syntax.Expr, b : @syntax.Expr) -> Bool {
  @core.alpha_eq(a, b)
}
