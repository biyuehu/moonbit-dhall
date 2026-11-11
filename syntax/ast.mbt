///|
/// Dhall universe constants forming an axiom hierarchy:
/// Type : Kind : Sort
#derive(Eq, Debug)
pub(all) enum Const {
  Type
  Kind
  Sort
}

///|
/// Builtin types and primitive functions defined in Dhall standard
#derive(Eq, Debug)
pub(all) enum Builtin {
  // Types
  Bool
  Natural
  Integer
  Double
  Text
  List
  Optional
  None

  // Natural operations
  NaturalFold
  NaturalBuild
  NaturalIsZero
  NaturalEven
  NaturalOdd
  NaturalToInteger
  NaturalShow
  NaturalSubtract

  // Integer operations
  IntegerShow
  IntegerToDouble
  IntegerNegate
  IntegerClamp

  // Double operations
  DoubleShow

  // Text operations
  TextShow
  TextReplace

  // List operations
  ListBuild
  ListFold
  ListLength
  ListHead
  ListLast
  ListIndexed
  ListReverse
}

///|
/// Binary operators in Dhall
#derive(Eq, Debug)
pub(all) enum BinOp {
  // Logic
  BoolOr // ||
  BoolAnd // &&
  BoolEQ // ==
  BoolNE // !=

  // Arithmetic
  NaturalPlus // +
  NaturalTimes // *

  // Text & List
  TextAppend // ++
  ListAppend // #

  // Record & Type operations
  CombineRecord // /\
  CombineRecordType // //\\
  Prefer // //
  Equivalent // ===
}

///|
/// Text interpolation chunk
#derive(Eq, Debug)
pub(all) enum TextChunk {
  Lit(String)
  Expr(Expr)
}

///|
/// Core Dhall Expression AST
#derive(Eq, Debug)
pub(all) enum Expr {
  // Constants and Builtins
  Const(Const)
  Builtin(Builtin)

  // Variables with de Bruijn index: Var(name, index)
  // x@0 is the nearest binding of x, x@1 is the enclosing one, etc.
  Var(String, Int32)

  // Functions and Types
  Lam(String, Expr, Expr) // λ(x : A) -> b
  Pi(String, Expr, Expr) // ∀(x : A) -> B or A -> B
  App(Expr, Expr) // f a
  Let(String, Expr?, Expr, Expr) // let x [: T] = r in e
  Annot(Expr, Expr) // e : T

  // Literals
  BoolLit(Bool)
  BoolIf(Expr, Expr, Expr) // if cond then t else f
  NaturalLit(BigInt)
  IntegerLit(BigInt)
  DoubleLit(Double)
  TextLit(Array[TextChunk])
  ListLit(Expr?, Array[Expr]) // [x, y] : Optional type annotation
  Some(Expr) // Some x

  // Records
  RecordLit(Array[(String, Expr)]) // { k1 = v1, k2 = v2 }
  RecordType(Array[(String, Expr)]) // { k1 : T1, k2 : T2 }
  Field(Expr, String) // r.field
  Project(Expr, Array[String]) // r.{ a, b }
  ProjectByType(Expr, Expr) // r.(T)

  // Unions
  UnionType(Array[(String, Expr?)]) // < x : T | y >
  Merge(Expr, Expr, Expr?) // merge handlers union [: Optional Type]
  ToMap(Expr, Expr?) // toMap r [: Optional Type]

  // Operators
  BinOp(BinOp, Expr, Expr)

  // Assertions
  Assert(Expr) // assert : e
}

///|
/// Helper to construct arrow type `A -> B` which is shorthand for `Pi("_", A, B)`
pub fn Expr::arrow(domain : Expr, codomain : Expr) -> Expr {
  Expr::Pi("_", domain, codomain)
}

///|
/// Helper to check if an expression is a simple text literal without interpolations
pub fn Expr::text(s : String) -> Expr {
  Expr::TextLit([TextChunk::Lit(s)])
}
