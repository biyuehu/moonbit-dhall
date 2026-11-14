///|
/// Lexical tokens for Dhall syntax
#derive(Eq, Debug)
pub(all) enum Token {
  // Literals
  BoolLit(Bool)
  NaturalLit(BigInt)
  IntegerLit(BigInt)
  DoubleLit(Double)
  TextLit(String)

  // Identifiers and Builtins
  Ident(String)
  Const(Const)
  Builtin(Builtin)

  // Keywords
  KwLet
  KwIn
  KwIf
  KwThen
  KwElse
  KwForall
  KwAssert
  KwMerge
  KwToMap
  KwSome
  KwWith

  // Symbols
  LParen // (
  RParen // )
  LBrace // {
  RBrace // }
  LBracket // [
  RBracket // ]
  LAngle // <
  RAngle // >
  Colon // :
  ColonColon // ::
  Comma // ,
  Dot // .
  Equal // =
  Backslash // \
  Arrow // ->
  At // @
  RecordEmpty // {=}

  // Operators
  OpPlus // +
  OpTimes // *
  OpAnd // &&
  OpOr // ||
  OpEQ // ==
  OpNE // !=
  OpTextAppend // ++
  OpListAppend // #
  OpPrefer // //
  OpCombineRecord // /\
  OpCombineRecordType // //\\
  OpEquivalent // ===
  EOF
}
