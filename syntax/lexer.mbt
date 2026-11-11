///|
/// Lexer for Dhall syntax
pub struct Lexer {
  src : String
  len : Int32
  mut pos : Int32
}

///|
/// Creates a new Lexer from input string
pub fn Lexer::new(src : String) -> Lexer {
  { src, len: src.length(), pos: 0, }
}

///|
fn Lexer::char_at(self : Lexer, index : Int32) -> Char {
  self.src[index].to_int().unsafe_to_char()
}

///|
fn Lexer::peek(self : Lexer) -> Char? {
  if self.pos >= self.len {
    None
  } else {
    Some(self.char_at(self.pos))
  }
}

///|
fn Lexer::peek_at(self : Lexer, offset : Int32) -> Char? {
  let idx = self.pos + offset
  if idx >= self.len || idx < 0 {
    None
  } else {
    Some(self.char_at(idx))
  }
}

///|
fn is_whitespace(c : Char) -> Bool {
  c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

///|
fn is_digit(c : Char) -> Bool {
  c >= '0' && c <= '9'
}

///|
fn is_next_digit(opt_c : Char?) -> Bool {
  match opt_c {
    Some(c) => is_digit(c)
    None => false
  }
}

///|
fn is_ident_start(c : Char) -> Bool {
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

///|
fn is_ident_part(c : Char) -> Bool {
  is_ident_start(c) || is_digit(c) || c == '-' || c == '/'
}

///|
fn Lexer::skip_whitespace_and_comments(self : Lexer) -> Unit {
  while self.pos < self.len {
    let c = self.char_at(self.pos)
    if is_whitespace(c) {
      self.pos = self.pos + 1
    } else if c == '-' && self.peek_at(1) == Some('-') {
      // Line comment
      self.pos = self.pos + 2
      while self.pos < self.len && self.char_at(self.pos) != '\n' {
        self.pos = self.pos + 1
      }
    } else if c == '{' && self.peek_at(1) == Some('-') {
      // Block comment
      self.pos = self.pos + 2
      let mut depth = 1
      while self.pos < self.len && depth > 0 {
        if self.char_at(self.pos) == '{' && self.peek_at(1) == Some('-') {
          depth = depth + 1
          self.pos = self.pos + 2
        } else if self.char_at(self.pos) == '-' && self.peek_at(1) == Some('}') {
          depth = depth - 1
          self.pos = self.pos + 2
        } else {
          self.pos = self.pos + 1
        }
      }
    } else {
      break
    }
  }
}

///|
fn Lexer::read_string(self : Lexer) -> Token raise ParseError {
  // Consume leading "
  self.pos = self.pos + 1
  let sb = StringBuilder()
  while self.pos < self.len {
    let c = self.char_at(self.pos)
    if c == '"' {
      self.pos = self.pos + 1
      return Token::TextLit(sb.to_string())
    } else if c == '\\' {
      self.pos = self.pos + 1
      if self.pos >= self.len {
        raise Message("Unexpected end of string in escape sequence")
      }
      match self.char_at(self.pos) {
        '"' => sb.write_string("\"")
        '\\' => sb.write_string("\\")
        'n' => sb.write_string("\n")
        'r' => sb.write_string("\r")
        't' => sb.write_string("\t")
        other => sb.write_char(other)
      }
      self.pos = self.pos + 1
    } else {
      sb.write_char(c)
      self.pos = self.pos + 1
    }
  }
  raise Message("Unterminated string literal")
}

///|
fn Lexer::read_number_or_ident(self : Lexer) -> Token {
  let start = self.pos
  while self.pos < self.len && is_ident_part(self.char_at(self.pos)) {
    self.pos = self.pos + 1
  }
  let word = self.src[start:self.pos].to_owned()

  // Check keywords & constants
  match word {
    "let" => Token::KwLet
    "in" => Token::KwIn
    "if" => Token::KwIf
    "then" => Token::KwThen
    "else" => Token::KwElse
    "forall" => Token::KwForall
    "assert" => Token::KwAssert
    "merge" => Token::KwMerge
    "toMap" => Token::KwToMap
    "Some" => Token::KwSome
    "True" => Token::BoolLit(true)
    "False" => Token::BoolLit(false)
    "Type" => Token::Const(Const::Type)
    "Kind" => Token::Const(Const::Kind)
    "Sort" => Token::Const(Const::Sort)
    "Bool" => Token::Builtin(Builtin::Bool)
    "Natural" => Token::Builtin(Builtin::Natural)
    "Integer" => Token::Builtin(Builtin::Integer)
    "Double" => Token::Builtin(Builtin::Double)
    "Text" => Token::Builtin(Builtin::Text)
    "List" => Token::Builtin(Builtin::List)
    "Optional" => Token::Builtin(Builtin::Optional)
    "None" => Token::Builtin(Builtin::None)
    "Natural/fold" => Token::Builtin(Builtin::NaturalFold)
    "Natural/build" => Token::Builtin(Builtin::NaturalBuild)
    "Natural/isZero" => Token::Builtin(Builtin::NaturalIsZero)
    "Natural/even" => Token::Builtin(Builtin::NaturalEven)
    "Natural/odd" => Token::Builtin(Builtin::NaturalOdd)
    "Natural/toInteger" => Token::Builtin(Builtin::NaturalToInteger)
    "Natural/show" => Token::Builtin(Builtin::NaturalShow)
    "Natural/subtract" => Token::Builtin(Builtin::NaturalSubtract)
    "Integer/show" => Token::Builtin(Builtin::IntegerShow)
    "Integer/toDouble" => Token::Builtin(Builtin::IntegerToDouble)
    "Integer/negate" => Token::Builtin(Builtin::IntegerNegate)
    "Integer/clamp" => Token::Builtin(Builtin::IntegerClamp)
    "Double/show" => Token::Builtin(Builtin::DoubleShow)
    "Text/show" => Token::Builtin(Builtin::TextShow)
    "Text/replace" => Token::Builtin(Builtin::TextReplace)
    "List/build" => Token::Builtin(Builtin::ListBuild)
    "List/fold" => Token::Builtin(Builtin::ListFold)
    "List/length" => Token::Builtin(Builtin::ListLength)
    "List/head" => Token::Builtin(Builtin::ListHead)
    "List/last" => Token::Builtin(Builtin::ListLast)
    "List/indexed" => Token::Builtin(Builtin::ListIndexed)
    "List/reverse" => Token::Builtin(Builtin::ListReverse)
    _ => {
      // Check if purely digits -> NaturalLit
      let mut all_digits = true
      for c in word {
        if is_digit(c) == false {
          all_digits = false
          break
        }
      }
      if all_digits && word.is_empty() == false {
        // Natural literal
        let mut n = BigInt::from_int(0)
        for c in word {
          let d = c.to_int() - '0'.to_int()
          n = n * 10 + BigInt::from_int(d)
        }
        Token::NaturalLit(n)
      } else {
        Token::Ident(word)
      }
    }
  }
}

///|
/// Scans the next token
pub fn Lexer::next_token(self : Lexer) -> Token raise ParseError {
  self.skip_whitespace_and_comments()
  if self.pos >= self.len {
    return Token::EOF
  }

  let c = self.char_at(self.pos)

  // Symbols and 2-char tokens
  if c == '(' {
    self.pos = self.pos + 1
    return Token::LParen
  }
  if c == ')' {
    self.pos = self.pos + 1
    return Token::RParen
  }
  if c == '{' {
    if self.peek_at(1) == Some('=') && self.peek_at(2) == Some('}') {
      self.pos = self.pos + 3
      return Token::RecordEmpty
    }
    self.pos = self.pos + 1
    return Token::LBrace
  }
  if c == '}' {
    self.pos = self.pos + 1
    return Token::RBrace
  }
  if c == '[' {
    self.pos = self.pos + 1
    return Token::LBracket
  }
  if c == ']' {
    self.pos = self.pos + 1
    return Token::RBracket
  }
  if c == '<' {
    self.pos = self.pos + 1
    return Token::LAngle
  }
  if c == '>' {
    self.pos = self.pos + 1
    return Token::RAngle
  }
  if c == ':' {
    self.pos = self.pos + 1
    return Token::Colon
  }
  if c == ',' {
    self.pos = self.pos + 1
    return Token::Comma
  }
  if c == '.' {
    self.pos = self.pos + 1
    return Token::Dot
  }
  if c == '=' {
    if self.peek_at(1) == Some('=') {
      if self.peek_at(2) == Some('=') {
        self.pos = self.pos + 3
        return Token::OpEquivalent
      }
      self.pos = self.pos + 2
      return Token::OpEQ
    }
    self.pos = self.pos + 1
    return Token::Equal
  }
  if c == '!' && self.peek_at(1) == Some('=') {
    self.pos = self.pos + 2
    return Token::OpNE
  }
  if c == '\\' {
    self.pos = self.pos + 1
    return Token::Backslash
  }
  if c == '@' {
    self.pos = self.pos + 1
    return Token::At
  }
  if c == '#' {
    self.pos = self.pos + 1
    return Token::OpListAppend
  }
  if c == '*' {
    self.pos = self.pos + 1
    return Token::OpTimes
  }
  if c == '+' {
    if self.peek_at(1) == Some('+') {
      self.pos = self.pos + 2
      return Token::OpTextAppend
    }
    // Check if signed integer literal: +123
    if is_next_digit(self.peek_at(1)) {
      self.pos = self.pos + 1
      let start = self.pos
      while self.pos < self.len && is_digit(self.char_at(self.pos)) {
        self.pos = self.pos + 1
      }
      let mut n = BigInt::from_int(0)
      for digit in self.src[start:self.pos].to_owned() {
        n = n * 10 + BigInt::from_int(digit.to_int() - '0'.to_int())
      }
      return Token::IntegerLit(n)
    }
    self.pos = self.pos + 1
    return Token::OpPlus
  }
  if c == '-' {
    if self.peek_at(1) == Some('>') {
      self.pos = self.pos + 2
      return Token::Arrow
    }
    // Check if negative integer literal: -123
    if is_next_digit(self.peek_at(1)) {
      self.pos = self.pos + 1
      let start = self.pos
      while self.pos < self.len && is_digit(self.char_at(self.pos)) {
        self.pos = self.pos + 1
      }
      let mut n = BigInt::from_int(0)
      for digit in self.src[start:self.pos].to_owned() {
        n = n * 10 + BigInt::from_int(digit.to_int() - '0'.to_int())
      }
      return Token::IntegerLit(-n)
    }
  }
  if c == '&' && self.peek_at(1) == Some('&') {
    self.pos = self.pos + 2
    return Token::OpAnd
  }
  if c == '|' && self.peek_at(1) == Some('|') {
    self.pos = self.pos + 2
    return Token::OpOr
  }
  if c == '/' {
    if self.peek_at(1) == Some('/') {
      self.pos = self.pos + 2
      return Token::OpPrefer
    }
    if self.peek_at(1) == Some('\\') {
      self.pos = self.pos + 2
      return Token::OpCombineRecord
    }
  }
  if c == '"' {
    return self.read_string()
  }

  if is_ident_start(c) || is_digit(c) {
    return self.read_number_or_ident()
  }

  raise Message("Unexpected character: " + c.to_string())
}
