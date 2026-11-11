///|
/// Formats a Dhall Const to string
pub fn Const::to_string(self : Const) -> String {
  match self {
    Const::Type => "Type"
    Const::Kind => "Kind"
    Const::Sort => "Sort"
  }
}

///|
/// Formats a Dhall Builtin identifier to string
pub fn Builtin::to_string(self : Builtin) -> String {
  match self {
    Builtin::Bool => "Bool"
    Builtin::Natural => "Natural"
    Builtin::Integer => "Integer"
    Builtin::Double => "Double"
    Builtin::Text => "Text"
    Builtin::List => "List"
    Builtin::Optional => "Optional"
    Builtin::None => "None"
    Builtin::NaturalFold => "Natural/fold"
    Builtin::NaturalBuild => "Natural/build"
    Builtin::NaturalIsZero => "Natural/isZero"
    Builtin::NaturalEven => "Natural/even"
    Builtin::NaturalOdd => "Natural/odd"
    Builtin::NaturalToInteger => "Natural/toInteger"
    Builtin::NaturalShow => "Natural/show"
    Builtin::NaturalSubtract => "Natural/subtract"
    Builtin::IntegerShow => "Integer/show"
    Builtin::IntegerToDouble => "Integer/toDouble"
    Builtin::IntegerNegate => "Integer/negate"
    Builtin::IntegerClamp => "Integer/clamp"
    Builtin::DoubleShow => "Double/show"
    Builtin::TextShow => "Text/show"
    Builtin::TextReplace => "Text/replace"
    Builtin::ListBuild => "List/build"
    Builtin::ListFold => "List/fold"
    Builtin::ListLength => "List/length"
    Builtin::ListHead => "List/head"
    Builtin::ListLast => "List/last"
    Builtin::ListIndexed => "List/indexed"
    Builtin::ListReverse => "List/reverse"
  }
}

///|
/// Formats a binary operator to string
pub fn BinOp::to_string(self : BinOp) -> String {
  match self {
    BinOp::BoolOr => "||"
    BinOp::BoolAnd => "&&"
    BinOp::BoolEQ => "=="
    BinOp::BoolNE => "!="
    BinOp::NaturalPlus => "+"
    BinOp::NaturalTimes => "*"
    BinOp::TextAppend => "++"
    BinOp::ListAppend => "#"
    BinOp::CombineRecord => "/\\"
    BinOp::CombineRecordType => "//\\\\"
    BinOp::Prefer => "//"
    BinOp::Equivalent => "==="
  }
}

///|
/// Formats an expression to pretty Dhall string representation
pub fn Expr::to_string(self : Expr) -> String {
  match self {
    Expr::Const(c) => c.to_string()
    Expr::Builtin(b) => b.to_string()
    Expr::Var(name, index) =>
      if index == 0 {
        name
      } else {
        name + "@" + index.to_string()
      }
    Expr::Lam(param, type_, body) =>
      "\\(" + param + " : " + type_.to_string() + ") -> " + body.to_string()
    Expr::Pi(param, domain, codomain) =>
      if param == "_" {
        domain.to_string() + " -> " + codomain.to_string()
      } else {
        "forall (" +
        param +
        " : " +
        domain.to_string() +
        ") -> " +
        codomain.to_string()
      }
    Expr::App(f, arg) => {
      let f_str = match f {
        Expr::Lam(_) | Expr::Pi(_) | Expr::Let(_) | Expr::Annot(_) =>
          "(" + f.to_string() + ")"
        _ => f.to_string()
      }
      let arg_str = match arg {
        Expr::App(_)
        | Expr::Lam(_)
        | Expr::Pi(_)
        | Expr::Let(_)
        | Expr::Annot(_)
        | Expr::BinOp(_) => "(" + arg.to_string() + ")"
        _ => arg.to_string()
      }
      f_str + " " + arg_str
    }
    Expr::Let(name, type_annot, value, body) => {
      let annot_str = match type_annot {
        Some(t) => " : " + t.to_string()
        None => ""
      }
      "let " +
      name +
      annot_str +
      " = " +
      value.to_string() +
      " in " +
      body.to_string()
    }
    Expr::Annot(expr, type_) =>
      "(" + expr.to_string() + " : " + type_.to_string() + ")"
    Expr::BoolLit(b) => if b { "True" } else { "False" }
    Expr::BoolIf(c, t, f) =>
      "if " +
      c.to_string() +
      " then " +
      t.to_string() +
      " else " +
      f.to_string()
    Expr::NaturalLit(n) => n.to_string()
    Expr::IntegerLit(n) =>
      if n >= 0 {
        "+" + n.to_string()
      } else {
        n.to_string()
      }
    Expr::DoubleLit(d) => d.to_string()
    Expr::TextLit(chunks) => {
      let sb = StringBuilder()
      sb.write_string("\"")
      for chunk in chunks {
        match chunk {
          TextChunk::Lit(s) => sb.write_string(s)
          TextChunk::Expr(e) => {
            sb.write_string("${")
            sb.write_string(e.to_string())
            sb.write_string("}")
          }
        }
      }
      sb.write_string("\"")
      sb.to_string()
    }
    Expr::ListLit(type_annot, items) => {
      let sb = StringBuilder()
      sb.write_string("[")
      for i, item in items {
        if i > 0 {
          sb.write_string(", ")
        }
        sb.write_string(item.to_string())
      }
      sb.write_string("]")
      match type_annot {
        Some(t) => " : List " + t.to_string()
        None => sb.to_string()
      }
    }
    Expr::Some(inner) => "Some (" + inner.to_string() + ")"
    Expr::RecordLit(fields) => {
      if fields.is_empty() {
        return "{=}"
      }
      let sb = StringBuilder()
      sb.write_string("{ ")
      for i, field in fields {
        if i > 0 {
          sb.write_string(", ")
        }
        sb.write_string(field.0)
        sb.write_string(" = ")
        sb.write_string(field.1.to_string())
      }
      sb.write_string(" }")
      sb.to_string()
    }
    Expr::RecordType(fields) => {
      if fields.is_empty() {
        return "{}"
      }
      let sb = StringBuilder()
      sb.write_string("{ ")
      for i, field in fields {
        if i > 0 {
          sb.write_string(", ")
        }
        sb.write_string(field.0)
        sb.write_string(" : ")
        sb.write_string(field.1.to_string())
      }
      sb.write_string(" }")
      sb.to_string()
    }
    Expr::Field(target, field) => target.to_string() + "." + field
    Expr::Project(target, fields) => {
      let sb = StringBuilder()
      sb.write_string(target.to_string())
      sb.write_string(".{ ")
      for i, f in fields {
        if i > 0 {
          sb.write_string(", ")
        }
        sb.write_string(f)
      }
      sb.write_string(" }")
      sb.to_string()
    }
    Expr::ProjectByType(target, type_) =>
      target.to_string() + ".(" + type_.to_string() + ")"
    Expr::UnionType(alts) => {
      if alts.is_empty() {
        return "<>"
      }
      let sb = StringBuilder()
      sb.write_string("< ")
      for i, alt in alts {
        if i > 0 {
          sb.write_string(" | ")
        }
        sb.write_string(alt.0)
        match alt.1 {
          Some(t) => {
            sb.write_string(" : ")
            sb.write_string(t.to_string())
          }
          None => ()
        }
      }
      sb.write_string(" >")
      sb.to_string()
    }
    Expr::Merge(handlers, union, type_annot) => {
      let base = "merge " + handlers.to_string() + " " + union.to_string()
      match type_annot {
        Some(t) => "(" + base + " : " + t.to_string() + ")"
        None => base
      }
    }
    Expr::ToMap(record, type_annot) => {
      let base = "toMap " + record.to_string()
      match type_annot {
        Some(t) => "(" + base + " : " + t.to_string() + ")"
        None => base
      }
    }
    Expr::BinOp(op, l, r) =>
      "(" + l.to_string() + " " + op.to_string() + " " + r.to_string() + ")"
    Expr::Assert(expr) => "assert : " + expr.to_string()
  }
}
