///|
/// Substitutes `replacement` for variable `(var_name, var_index)` in `expr`.
/// This implements the formal Dhall substitution specification:
/// `subst (x@m) v e`
pub fn subst(
  var_name : String,
  var_index : Int32,
  replacement : @syntax.Expr,
  expr : @syntax.Expr,
) -> @syntax.Expr {
  match expr {
    @syntax.Expr::Var(name, index) =>
      if name == var_name && index == var_index {
        replacement
      } else {
        @syntax.Expr::Var(name, index)
      }
    @syntax.Expr::Lam(param, type_, body) => {
      let next_index = if param == var_name { var_index + 1 } else { var_index }
      let shifted_replacement = shift(1, param, 0, replacement)
      @syntax.Expr::Lam(
        param,
        subst(var_name, var_index, replacement, type_),
        subst(var_name, next_index, shifted_replacement, body),
      )
    }
    @syntax.Expr::Pi(param, domain, codomain) => {
      let next_index = if param == var_name { var_index + 1 } else { var_index }
      let shifted_replacement = shift(1, param, 0, replacement)
      @syntax.Expr::Pi(
        param,
        subst(var_name, var_index, replacement, domain),
        subst(var_name, next_index, shifted_replacement, codomain),
      )
    }
    @syntax.Expr::Let(param, type_annot, value, body) => {
      let next_index = if param == var_name { var_index + 1 } else { var_index }
      let shifted_replacement = shift(1, param, 0, replacement)
      let new_type = match type_annot {
        Some(t) => Some(subst(var_name, var_index, replacement, t))
        None => None
      }
      @syntax.Expr::Let(
        param,
        new_type,
        subst(var_name, var_index, replacement, value),
        subst(var_name, next_index, shifted_replacement, body),
      )
    }
    @syntax.Expr::App(f, arg) =>
      @syntax.Expr::App(
        subst(var_name, var_index, replacement, f),
        subst(var_name, var_index, replacement, arg),
      )
    @syntax.Expr::Annot(e, t) =>
      @syntax.Expr::Annot(
        subst(var_name, var_index, replacement, e),
        subst(var_name, var_index, replacement, t),
      )
    @syntax.Expr::BoolIf(c, t, f) =>
      @syntax.Expr::BoolIf(
        subst(var_name, var_index, replacement, c),
        subst(var_name, var_index, replacement, t),
        subst(var_name, var_index, replacement, f),
      )
    @syntax.Expr::TextLit(chunks) => {
      let new_chunks = chunks.map(fn(chunk) {
        match chunk {
          @syntax.TextChunk::Lit(s) => @syntax.TextChunk::Lit(s)
          @syntax.TextChunk::Expr(e) =>
            @syntax.TextChunk::Expr(subst(var_name, var_index, replacement, e))
        }
      })
      @syntax.Expr::TextLit(new_chunks)
    }
    @syntax.Expr::ListLit(t, items) => {
      let new_t = match t {
        Some(ty) => Some(subst(var_name, var_index, replacement, ty))
        None => None
      }
      let new_items = items.map(fn(item) {
        subst(var_name, var_index, replacement, item)
      })
      @syntax.Expr::ListLit(new_t, new_items)
    }
    @syntax.Expr::Some(inner) =>
      @syntax.Expr::Some(subst(var_name, var_index, replacement, inner))
    @syntax.Expr::RecordLit(fields) => {
      let new_fields = fields.map(fn(kv) {
        (kv.0, subst(var_name, var_index, replacement, kv.1))
      })
      @syntax.Expr::RecordLit(new_fields)
    }
    @syntax.Expr::RecordType(fields) => {
      let new_fields = fields.map(fn(kv) {
        (kv.0, subst(var_name, var_index, replacement, kv.1))
      })
      @syntax.Expr::RecordType(new_fields)
    }
    @syntax.Expr::Field(target, field) =>
      @syntax.Expr::Field(
        subst(var_name, var_index, replacement, target),
        field,
      )
    @syntax.Expr::Project(target, fields) =>
      @syntax.Expr::Project(
        subst(var_name, var_index, replacement, target),
        fields,
      )
    @syntax.Expr::ProjectByType(target, ty) =>
      @syntax.Expr::ProjectByType(
        subst(var_name, var_index, replacement, target),
        subst(var_name, var_index, replacement, ty),
      )
    @syntax.Expr::UnionType(alts) => {
      let new_alts = alts.map(fn(kv) {
        let new_t = match kv.1 {
          Some(ty) => Some(subst(var_name, var_index, replacement, ty))
          None => None
        }
        (kv.0, new_t)
      })
      @syntax.Expr::UnionType(new_alts)
    }
    @syntax.Expr::Merge(handlers, union, t) => {
      let new_t = match t {
        Some(ty) => Some(subst(var_name, var_index, replacement, ty))
        None => None
      }
      @syntax.Expr::Merge(
        subst(var_name, var_index, replacement, handlers),
        subst(var_name, var_index, replacement, union),
        new_t,
      )
    }
    @syntax.Expr::ToMap(record, t) => {
      let new_t = match t {
        Some(ty) => Some(subst(var_name, var_index, replacement, ty))
        None => None
      }
      @syntax.Expr::ToMap(
        subst(var_name, var_index, replacement, record),
        new_t,
      )
    }
    @syntax.Expr::BinOp(op, l, r) =>
      @syntax.Expr::BinOp(
        op,
        subst(var_name, var_index, replacement, l),
        subst(var_name, var_index, replacement, r),
      )
    @syntax.Expr::Assert(e) =>
      @syntax.Expr::Assert(subst(var_name, var_index, replacement, e))
    @syntax.Expr::Const(_)
    | @syntax.Expr::Builtin(_)
    | @syntax.Expr::BoolLit(_)
    | @syntax.Expr::NaturalLit(_)
    | @syntax.Expr::IntegerLit(_)
    | @syntax.Expr::DoubleLit(_) => expr
  }
}
