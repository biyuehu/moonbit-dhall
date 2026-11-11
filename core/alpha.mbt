///|
/// Checks if two expressions are α-equivalent.
/// Since Dhall represents variables with de Bruijn indices relative to enclosing binders,
/// α-equivalence corresponds to equality under consistent indexing.
pub fn alpha_eq(a : @syntax.Expr, b : @syntax.Expr) -> Bool {
  a == b
}
