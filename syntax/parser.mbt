///|
/// Recursive-descent & Pratt parser for Dhall
pub struct Parser {
  lexer : Lexer
  mut current : Token
}

///|
/// Creates a new Parser from input string
pub fn Parser::new(src : String) -> Parser raise ParseError {
  let lexer = Lexer::new(src)
  let p = { lexer, current: Token::EOF, }
  p.advance()
  p
}

///|
fn Parser::advance(self : Parser) -> Unit raise ParseError {
  self.current = self.lexer.next_token()
}

///|
fn Parser::expect(self : Parser, expected : Token) -> Unit raise ParseError {
  if self.current == expected {
    self.advance()
  } else {
    raise Message("Expected token but got unexpected token")
  }
}

///|
fn Parser::parse_label(self : Parser) -> String raise ParseError {
  match self.current {
    Token::Ident(name) => {
      self.advance()
      name
    }
    Token::TextLit(s) => {
      self.advance()
      s
    }
    Token::KwSome => {
      self.advance()
      "Some"
    }
    Token::Builtin(b) => {
      self.advance()
      b.to_string()
    }
    Token::Const(c) => {
      self.advance()
      c.to_string()
    }
    _ => raise Message("Expected label name")
  }
}

///|

///|
fn make_nested_record(path : Array[String], idx : Int32, val : Expr) -> Expr {
  if idx == path.length() - 1 {
    val
  } else {
    Expr::RecordLit([(path[idx + 1], make_nested_record(path, idx + 1, val))])
  }
}

///|
fn add_record_field(
  fields : Array[(String, Expr)],
  path : Array[String],
  val : Expr,
) -> Unit {
  let key = path[0]
  let field_val = make_nested_record(path, 0, val)
  let mut merged = false
  let mut i = 0
  while i < fields.length() {
    if fields[i].0 == key {
      fields[i] = (
        key,
        Expr::BinOp(BinOp::CombineRecord, fields[i].1, field_val),
      )
      merged = true
      break
    }
    i = i + 1
  }
  if merged == false {
    fields.push((key, field_val))
  }
}

///|
fn Parser::parse_atom(self : Parser) -> Expr raise ParseError {
  match self.current {
    Token::BoolLit(b) => {
      self.advance()
      Expr::BoolLit(b)
    }
    Token::NaturalLit(n) => {
      self.advance()
      Expr::NaturalLit(n)
    }
    Token::IntegerLit(n) => {
      self.advance()
      Expr::IntegerLit(n)
    }
    Token::DoubleLit(d) => {
      self.advance()
      Expr::DoubleLit(d)
    }
    Token::TextLit(s) => {
      self.advance()
      Expr::text(s)
    }
    Token::Const(c) => {
      self.advance()
      Expr::Const(c)
    }
    Token::Builtin(b) => {
      self.advance()
      Expr::Builtin(b)
    }
    Token::KwSome => {
      self.advance()
      let inner = self.parse_atom()
      Expr::Some(inner)
    }
    Token::Ident(name) => {
      self.advance()
      if self.current == Token::At {
        self.advance()
        match self.current {
          Token::NaturalLit(idx) => {
            self.advance()
            Expr::Var(name, idx.to_int())
          }
          _ => raise Message("Expected integer index after @")
        }
      } else {
        Expr::Var(name, 0)
      }
    }
    Token::LParen => {
      self.advance()
      let expr = self.parse_expr()
      self.expect(Token::RParen)
      expr
    }
    Token::RecordEmpty => {
      self.advance()
      Expr::RecordLit([])
    }
    Token::LBrace => {
      self.advance()
      if self.current == Token::RBrace {
        self.advance()
        return Expr::RecordType([])
      }
      let first_path : Array[String] = [self.parse_label()]
      while self.current == Token::Dot {
        self.advance()
        first_path.push(self.parse_label())
      }
      if self.current == Token::Equal {
        // Record literal: { key = val, ... }
        self.advance()
        let val = self.parse_expr()
        let fields : Array[(String, Expr)] = []
        add_record_field(fields, first_path, val)
        while self.current == Token::Comma {
          self.advance()
          let path : Array[String] = [self.parse_label()]
          while self.current == Token::Dot {
            self.advance()
            path.push(self.parse_label())
          }
          self.expect(Token::Equal)
          let v = self.parse_expr()
          add_record_field(fields, path, v)
        }
        self.expect(Token::RBrace)
        Expr::RecordLit(fields)
      } else if self.current == Token::Colon {
        // Record type: { key : Type, ... }
        self.advance()
        let ty = self.parse_expr()
        let fields : Array[(String, Expr)] = [(first_path[0], ty)]
        while self.current == Token::Comma {
          self.advance()
          let k = self.parse_label()
          self.expect(Token::Colon)
          let t = self.parse_expr()
          fields.push((k, t))
        }
        self.expect(Token::RBrace)
        Expr::RecordType(fields)
      } else {
        raise Message("Expected '=' or ':' after record field key")
      }
    }
    Token::LBracket => {
      self.advance()
      if self.current == Token::RBracket {
        self.advance()
        return Expr::ListLit((None : Expr?), [])
      }
      let first = self.parse_expr()
      let items : Array[Expr] = [first]
      while self.current == Token::Comma {
        self.advance()
        let item = self.parse_expr()
        items.push(item)
      }
      self.expect(Token::RBracket)
      Expr::ListLit((None : Expr?), items)
    }
    Token::LAngle => {
      self.advance()
      if self.current == Token::RAngle {
        self.advance()
        return Expr::UnionType([])
      }
      let alts : Array[(String, Expr?)] = []
      while true {
        let name = self.parse_label()
        let ty : Expr? = if self.current == Token::Colon {
          self.advance()
          Some(self.parse_expr())
        } else {
          None
        }
        alts.push((name, ty))
        if self.current == Token::OpOr {
          self.advance()
        } else {
          break
        }
      }
      self.expect(Token::RAngle)
      Expr::UnionType(alts)
    }
    _ => raise Message("Unexpected atom token")
  }
}

///|
/// Parses postfix expressions like field access `r.field` or projection `r.{ a, b }`
fn Parser::parse_postfix(self : Parser) -> Expr raise ParseError {
  let mut expr = self.parse_atom()
  while true {
    if self.current == Token::Dot {
      self.advance()
      if self.current == Token::LBrace {
        self.advance()
        let fields : Array[String] = []
        let f = self.parse_label()
        fields.push(f)
        while self.current == Token::Comma {
          self.advance()
          let next_f = self.parse_label()
          fields.push(next_f)
        }
        self.expect(Token::RBrace)
        expr = Expr::Project(expr, fields)
      } else {
        let field = self.parse_label()
        expr = Expr::Field(expr, field)
      }
    } else if self.current == Token::ColonColon {
      self.advance()
      let right = self.parse_atom()
      expr = Expr::Annot(
        Expr::BinOp(BinOp::Prefer, Expr::Field(expr, "default"), right),
        Expr::Field(expr, "Type"),
      )
    } else {
      break
    }
  }
  expr
}

///|
/// Checks if current token can start an atom (used for function application `f a`)
fn Parser::can_start_atom(self : Parser) -> Bool {
  match self.current {
    Token::BoolLit(_)
    | Token::NaturalLit(_)
    | Token::IntegerLit(_)
    | Token::DoubleLit(_)
    | Token::TextLit(_)
    | Token::Const(_)
    | Token::Builtin(_)
    | Token::Ident(_)
    | Token::LParen
    | Token::LBrace
    | Token::LBracket
    | Token::LAngle
    | Token::RecordEmpty
    | Token::KwSome => true
    _ => false
  }
}

///|
/// Parses function application: `f a b c`
fn Parser::parse_app(self : Parser) -> Expr raise ParseError {
  let mut expr = self.parse_postfix()
  while self.can_start_atom() {
    let arg = self.parse_postfix()
    expr = Expr::App(expr, arg)
  }
  expr
}

///|
/// Gets operator precedence
fn op_precedence(tok : Token) -> Int32 {
  match tok {
    Token::OpEquivalent => 10
    Token::OpOr => 20
    Token::OpPlus => 30
    Token::OpTextAppend | Token::OpListAppend => 40
    Token::OpAnd => 50
    Token::OpTimes => 60
    Token::OpEQ | Token::OpNE => 70
    Token::OpCombineRecord | Token::OpCombineRecordType => 80
    Token::OpPrefer => 90
    _ => 0
  }
}

///|
/// Converts binary operator token to BinOp enum
fn tok_to_binop(tok : Token) -> BinOp? {
  match tok {
    Token::OpEquivalent => Some(BinOp::Equivalent)
    Token::OpOr => Some(BinOp::BoolOr)
    Token::OpPlus => Some(BinOp::NaturalPlus)
    Token::OpTextAppend => Some(BinOp::TextAppend)
    Token::OpListAppend => Some(BinOp::ListAppend)
    Token::OpAnd => Some(BinOp::BoolAnd)
    Token::OpTimes => Some(BinOp::NaturalTimes)
    Token::OpEQ => Some(BinOp::BoolEQ)
    Token::OpNE => Some(BinOp::BoolNE)
    Token::OpCombineRecord => Some(BinOp::CombineRecord)
    Token::OpCombineRecordType => Some(BinOp::CombineRecordType)
    Token::OpPrefer => Some(BinOp::Prefer)
    _ => None
  }
}

///|
/// Parses binary operators using Pratt parsing
fn Parser::parse_binop(
  self : Parser,
  min_prec : Int32,
) -> Expr raise ParseError {
  let mut left = self.parse_app()
  while true {
    let prec = op_precedence(self.current)
    if prec <= min_prec {
      break
    }
    match tok_to_binop(self.current) {
      Some(op) => {
        self.advance()
        let right = self.parse_binop(prec)
        left = Expr::BinOp(op, left, right)
      }
      None => break
    }
  }
  left
}

///|
/// Parses any Dhall expression
pub fn Parser::parse_expr(self : Parser) -> Expr raise ParseError {
  match self.current {
    // Lambda: \(x : A) -> b
    Token::Backslash => {
      self.advance()
      self.expect(Token::LParen)
      let param = match self.current {
        Token::Ident(p) => {
          self.advance()
          p
        }
        _ => raise Message("Expected parameter name in lambda")
      }
      self.expect(Token::Colon)
      let ty = self.parse_expr()
      self.expect(Token::RParen)
      self.expect(Token::Arrow)
      let body = self.parse_expr()
      Expr::Lam(param, ty, body)
    }
    // Forall: forall (x : A) -> B
    Token::KwForall => {
      self.advance()
      self.expect(Token::LParen)
      let param = match self.current {
        Token::Ident(p) => {
          self.advance()
          p
        }
        _ => raise Message("Expected parameter name in forall")
      }
      self.expect(Token::Colon)
      let ty = self.parse_expr()
      self.expect(Token::RParen)
      self.expect(Token::Arrow)
      let body = self.parse_expr()
      Expr::Pi(param, ty, body)
    }
    // Let: let x [: T] = r in e
    Token::KwLet => {
      self.advance()
      let name = match self.current {
        Token::Ident(n) => {
          self.advance()
          n
        }
        _ => raise Message("Expected variable name in let binding")
      }
      let opt_ty : Expr? = if self.current == Token::Colon {
        self.advance()
        Some(self.parse_expr())
      } else {
        None
      }
      self.expect(Token::Equal)
      let val = self.parse_expr()
      self.expect(Token::KwIn)
      let body = self.parse_expr()
      Expr::Let(name, opt_ty, val, body)
    }
    // If: if c then t else f
    Token::KwIf => {
      self.advance()
      let cond = self.parse_expr()
      self.expect(Token::KwThen)
      let then_branch = self.parse_expr()
      self.expect(Token::KwElse)
      let else_branch = self.parse_expr()
      Expr::BoolIf(cond, then_branch, else_branch)
    }
    // Merge: merge handlers union
    Token::KwMerge => {
      self.advance()
      let handlers = self.parse_atom()
      let union = self.parse_atom()
      Expr::Merge(handlers, union, (None : Expr?))
    }
    // ToMap: toMap r
    Token::KwToMap => {
      self.advance()
      let rec = self.parse_atom()
      Expr::ToMap(rec, (None : Expr?))
    }
    // Assert: assert : e
    Token::KwAssert => {
      self.advance()
      self.expect(Token::Colon)
      let expr = self.parse_expr()
      Expr::Assert(expr)
    }
    _ => {
      let mut expr = self.parse_binop(0)
      while self.current == Token::KwWith {
        self.advance()
        let path : Array[String] = [self.parse_label()]
        while self.current == Token::Dot {
          self.advance()
          path.push(self.parse_label())
        }
        self.expect(Token::Equal)
        let val = self.parse_binop(0)
        expr = Expr::With(expr, path, val)
      }
      // Check for arrow type: A -> B
      if self.current == Token::Arrow {
        self.advance()
        let codomain = self.parse_expr()
        expr = Expr::arrow(expr, codomain)
      } else if self.current == Token::Colon {
        // Annotation: e : T
        self.advance()
        let ty = self.parse_expr()
        expr = Expr::Annot(expr, ty)
      }
      expr
    }
  }
}

///|
/// Parses a Dhall source string into an AST
pub fn parse(src : String) -> Result[Expr, String] {
  try {
    let parser = Parser::new(src)
    let expr = parser.parse_expr()
    if parser.current != Token::EOF {
      return Err("Unexpected trailing tokens")
    }
    Ok(expr)
  } catch {
    Message(msg) => Err(msg)
  }
}
