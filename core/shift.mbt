///|
/// Shifts de Bruijn indices of variable `var_name` at or above `min_index` by `amount`.
/// This implements the formal Dhall shifting specification:
/// `shift d (x@m) e`
pub fn shift(
  amount : Int32,
  var_name : String,
  min_index : Int32,
  expr : @syntax.Expr,
) -> @syntax.Expr {
  match expr {
    @syntax.Expr::Var(name, index) =>
      if name == var_name && index >= min_index {
        @syntax.Expr::Var(name, index + amount)
      } else {
        @syntax.Expr::Var(name, index)
      }
    @syntax.Expr::Lam(param, type_, body) => {
      let next_min = if param == var_name { min_index + 1 } else { min_index }
      @syntax.Expr::Lam(
        param,
        shift(amount, var_name, min_index, type_),
        shift(amount, var_name, next_min, body),
      )
    }
    @syntax.Expr::Pi(param, domain, codomain) => {
      let next_min = if param == var_name { min_index + 1 } else { min_index }
      @syntax.Expr::Pi(
        param,
        shift(amount, var_name, min_index, domain),
        shift(amount, var_name, next_min, codomain),
      )
    }
    @syntax.Expr::Let(param, type_annot, value, body) => {
      let next_min = if param == var_name { min_index + 1 } else { min_index }
      let new_type = match type_annot {
        Some(t) => Some(shift(amount, var_name, min_index, t))
        None => None
      }
      @syntax.Expr::Let(
        param,
        new_type,
        shift(amount, var_name, min_index, value),
        shift(amount, var_name, next_min, body),
      )
    }
    @syntax.Expr::App(f, arg) =>
      @syntax.Expr::App(
        shift(amount, var_name, min_index, f),
        shift(amount, var_name, min_index, arg),
      )
    @syntax.Expr::Annot(e, t) =>
      @syntax.Expr::Annot(
        shift(amount, var_name, min_index, e),
        shift(amount, var_name, min_index, t),
      )
    @syntax.Expr::BoolIf(c, t, f) =>
      @syntax.Expr::BoolIf(
        shift(amount, var_name, min_index, c),
        shift(amount, var_name, min_index, t),
        shift(amount, var_name, min_index, f),
      )
    @syntax.Expr::TextLit(chunks) => {
      let new_chunks = chunks.map(fn(chunk) {
        match chunk {
          @syntax.TextChunk::Lit(s) => @syntax.TextChunk::Lit(s)
          @syntax.TextChunk::Expr(e) =>
            @syntax.TextChunk::Expr(shift(amount, var_name, min_index, e))
        }
      })
      @syntax.Expr::TextLit(new_chunks)
    }
    @syntax.Expr::ListLit(t, items) => {
      let new_t = match t {
        Some(ty) => Some(shift(amount, var_name, min_index, ty))
        None => None
      }
      let new_items = items.map(fn(item) {
        shift(amount, var_name, min_index, item)
      })
      @syntax.Expr::ListLit(new_t, new_items)
    }
    @syntax.Expr::Some(inner) =>
      @syntax.Expr::Some(shift(amount, var_name, min_index, inner))
    @syntax.Expr::RecordLit(fields) => {
      let new_fields = fields.map(fn(kv) {
        (kv.0, shift(amount, var_name, min_index, kv.1))
      })
      @syntax.Expr::RecordLit(new_fields)
    }
    @syntax.Expr::RecordType(fields) => {
      let new_fields = fields.map(fn(kv) {
        (kv.0, shift(amount, var_name, min_index, kv.1))
      })
      @syntax.Expr::RecordType(new_fields)
    }
    @syntax.Expr::Field(target, field) =>
      @syntax.Expr::Field(shift(amount, var_name, min_index, target), field)
    @syntax.Expr::Project(target, fields) =>
      @syntax.Expr::Project(shift(amount, var_name, min_index, target), fields)
    @syntax.Expr::ProjectByType(target, ty) =>
      @syntax.Expr::ProjectByType(
        shift(amount, var_name, min_index, target),
        shift(amount, var_name, min_index, ty),
      )
    @syntax.Expr::With(target, path, val) =>
      @syntax.Expr::With(
        shift(amount, var_name, min_index, target),
        path,
        shift(amount, var_name, min_index, val),
      )
    @syntax.Expr::UnionType(alts) => {
      let new_alts = alts.map(fn(kv) {
        let new_t = match kv.1 {
          Some(ty) => Some(shift(amount, var_name, min_index, ty))
          None => None
        }
        (kv.0, new_t)
      })
      @syntax.Expr::UnionType(new_alts)
    }
    @syntax.Expr::Merge(handlers, union, t) => {
      let new_t = match t {
        Some(ty) => Some(shift(amount, var_name, min_index, ty))
        None => None
      }
      @syntax.Expr::Merge(
        shift(amount, var_name, min_index, handlers),
        shift(amount, var_name, min_index, union),
        new_t,
      )
    }
    @syntax.Expr::ToMap(record, t) => {
      let new_t = match t {
        Some(ty) => Some(shift(amount, var_name, min_index, ty))
        None => None
      }
      @syntax.Expr::ToMap(shift(amount, var_name, min_index, record), new_t)
    }
    @syntax.Expr::BinOp(op, l, r) =>
      @syntax.Expr::BinOp(
        op,
        shift(amount, var_name, min_index, l),
        shift(amount, var_name, min_index, r),
      )
    @syntax.Expr::Assert(e) =>
      @syntax.Expr::Assert(shift(amount, var_name, min_index, e))
    // Terminals without sub-expressions or variables
    @syntax.Expr::Const(_)
    | @syntax.Expr::Builtin(_)
    | @syntax.Expr::BoolLit(_)
    | @syntax.Expr::NaturalLit(_)
    | @syntax.Expr::IntegerLit(_)
    | @syntax.Expr::DoubleLit(_) => expr
  }
}
