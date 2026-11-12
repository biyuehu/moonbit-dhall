///|
/// Parses a Dhall source string into an AST
pub fn parse(src : String) -> Result[@syntax.Expr, String] {
  @syntax.parse(src)
}

///|
/// Evaluates and normalizes a Dhall expression to its normal form (β-normalization)
pub fn eval(expr : @syntax.Expr) -> @syntax.Expr {
  @core.eval(expr)
}

///|
/// Parses and evaluates a Dhall source code string to its normal form
pub fn evaluate_str(src : String) -> Result[@syntax.Expr, String] {
  match @syntax.parse(src) {
    Ok(expr) => Ok(@core.eval(expr))
    Err(e) => Err(e)
  }
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
/// End-to-end: parses a Dhall source string, normalizes it, and outputs canonical JSON
pub fn to_json_str(src : String) -> Result[String, String] {
  match @syntax.parse(src) {
    Ok(expr) => @export_json.expr_to_json_canonical(expr)
    Err(e) => Err(e)
  }
}

///|
/// Infers the type of a Dhall expression in the empty context
pub fn type_of(expr : @syntax.Expr) -> Result[@syntax.Expr, String] {
  @types.type_of(expr)
}

///|
/// Parses a Dhall source string and infers its type
pub fn type_check_str(src : String) -> Result[@syntax.Expr, String] {
  match @syntax.parse(src) {
    Ok(expr) => @types.type_of(expr)
    Err(e) => Err(e)
  }
}

///|
/// Checks if two Dhall expressions are α-equivalent
pub fn alpha_eq(a : @syntax.Expr, b : @syntax.Expr) -> Bool {
  @core.alpha_eq(a, b)
}
