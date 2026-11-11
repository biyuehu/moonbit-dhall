///|
/// Converts a normalized Dhall expression to MoonBit's JSON AST (`Json`)
pub fn expr_to_json(expr : @syntax.Expr) -> Result[Json, String] {
  let norm = @core.eval(expr)
  match norm {
    @syntax.Expr::BoolLit(b) => Ok(Json(b))
    @syntax.Expr::NaturalLit(n) => {
      let s = n.to_string()
      Ok(Json::number(0.0, repr=s))
    }
    @syntax.Expr::IntegerLit(n) => {
      let s = n.to_string()
      Ok(Json::number(0.0, repr=s))
    }
    @syntax.Expr::DoubleLit(d) => Ok(Json(d))
    @syntax.Expr::TextLit(chunks) => {
      let sb = StringBuilder()
      for chunk in chunks {
        match chunk {
          @syntax.TextChunk::Lit(s) => sb.write_string(s)
          @syntax.TextChunk::Expr(e) =>
            return Err(
              "Cannot convert unnormalized text expression to JSON: " +
              e.to_string(),
            )
        }
      }
      Ok(Json(sb.to_string()))
    }
    @syntax.Expr::ListLit(_, items) => {
      let arr : Array[Json] = []
      for item in items {
        match expr_to_json(item) {
          Ok(j) => arr.push(j)
          Err(e) => return Err(e)
        }
      }
      Ok(Json(arr))
    }
    @syntax.Expr::Some(inner) => expr_to_json(inner)
    @syntax.Expr::Builtin(@syntax.Builtin::None) => Ok(Json::null())
    @syntax.Expr::RecordLit(fields) => {
      let map : Map[String, Json] = Map([])
      for kv in fields {
        match expr_to_json(kv.1) {
          Ok(j) => map[kv.0] = j
          Err(e) => return Err(e)
        }
      }
      Ok(Json(map))
    }
    _ =>
      Err(
        "Cannot convert non-literal Dhall expression to JSON: " +
        norm.to_string(),
      )
  }
}

///|
/// Escapes a string for JSON output
fn escape_json_string(s : String) -> String {
  let sb = StringBuilder()
  sb.write_string("\"")
  for c in s {
    match c {
      '"' => sb.write_string("\\\"")
      '\\' => sb.write_string("\\\\")
      '\n' => sb.write_string("\\n")
      '\r' => sb.write_string("\\r")
      '\t' => sb.write_string("\\t")
      _ => sb.write_char(c)
    }
  }
  sb.write_string("\"")
  sb.to_string()
}

///|
/// Formats a Dhall expression as a canonical JSON string with deterministic sorted keys
pub fn expr_to_json_canonical(expr : @syntax.Expr) -> Result[String, String] {
  let norm = @core.eval(expr)
  match norm {
    @syntax.Expr::BoolLit(b) => if b { Ok("true") } else { Ok("false") }
    @syntax.Expr::NaturalLit(n) => Ok(n.to_string())
    @syntax.Expr::IntegerLit(n) => Ok(n.to_string())
    @syntax.Expr::DoubleLit(d) => Ok(d.to_string())
    @syntax.Expr::TextLit(chunks) => {
      let sb = StringBuilder()
      for chunk in chunks {
        match chunk {
          @syntax.TextChunk::Lit(s) => sb.write_string(s)
          @syntax.TextChunk::Expr(e) =>
            return Err(
              "Cannot convert unnormalized text expression to JSON: " +
              e.to_string(),
            )
        }
      }
      Ok(escape_json_string(sb.to_string()))
    }
    @syntax.Expr::ListLit(_, items) => {
      let sb = StringBuilder()
      sb.write_string("[")
      for i, item in items {
        if i > 0 {
          sb.write_string(",")
        }
        match expr_to_json_canonical(item) {
          Ok(s) => sb.write_string(s)
          Err(e) => return Err(e)
        }
      }
      sb.write_string("]")
      Ok(sb.to_string())
    }
    @syntax.Expr::Some(inner) => expr_to_json_canonical(inner)
    @syntax.Expr::Builtin(@syntax.Builtin::None) => Ok("null")
    @syntax.Expr::RecordLit(fields) => {
      let sb = StringBuilder()
      sb.write_string("{")
      for i, kv in fields {
        if i > 0 {
          sb.write_string(",")
        }
        sb.write_string(escape_json_string(kv.0))
        sb.write_string(":")
        match expr_to_json_canonical(kv.1) {
          Ok(s) => sb.write_string(s)
          Err(e) => return Err(e)
        }
      }
      sb.write_string("}")
      Ok(sb.to_string())
    }
    _ =>
      Err(
        "Cannot convert non-literal Dhall expression to JSON: " +
        norm.to_string(),
      )
  }
}
