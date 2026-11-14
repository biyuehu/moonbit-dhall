///|
/// Sorts record key-value pairs lexicographically by label
fn sort_fields(
  fields : Array[(String, @syntax.Expr)],
) -> Array[(String, @syntax.Expr)] {
  let arr = fields.copy()
  let mut i = 1
  while i < arr.length() {
    let key = arr[i]
    let mut j = i - 1
    while j >= 0 && @syntax.lexical_compare(arr[j].0, key.0) > 0 {
      arr[j + 1] = arr[j]
      j = j - 1
    }
    arr[j + 1] = key
    i = i + 1
  }
  arr
}

///|
/// Evaluates and normalizes a Dhall expression to its normal form (β-normalization)
pub fn eval(expr : @syntax.Expr) -> @syntax.Expr {
  match expr {
    // Terminals
    @syntax.Expr::Const(_)
    | @syntax.Expr::Builtin(_)
    | @syntax.Expr::Var(_)
    | @syntax.Expr::BoolLit(_)
    | @syntax.Expr::NaturalLit(_)
    | @syntax.Expr::IntegerLit(_)
    | @syntax.Expr::DoubleLit(_) => expr
    @syntax.Expr::Annot(e, _) => eval(e)
    @syntax.Expr::Lam(param, type_, body) =>
      @syntax.Expr::Lam(param, eval(type_), eval(body))
    @syntax.Expr::Pi(param, domain, codomain) =>
      @syntax.Expr::Pi(param, eval(domain), eval(codomain))
    @syntax.Expr::Let(param, _, value, body) => {
      let val_norm = eval(value)
      let substituted = subst(param, 0, val_norm, body)
      let shifted = shift(-1, param, 0, substituted)
      eval(shifted)
    }
    @syntax.Expr::App(f, arg) => {
      let f_norm = eval(f)
      let arg_norm = eval(arg)
      match f_norm {
        @syntax.Expr::Lam(param, _, body) => {
          let substituted = subst(param, 0, arg_norm, body)
          let shifted = shift(-1, param, 0, substituted)
          eval(shifted)
        }
        // Builtin application reductions
        _ => reduce_app(f_norm, arg_norm)
      }
    }
    @syntax.Expr::BoolIf(c, t, f) => {
      let c_norm = eval(c)
      match c_norm {
        @syntax.Expr::BoolLit(true) => eval(t)
        @syntax.Expr::BoolLit(false) => eval(f)
        _ => {
          let t_norm = eval(t)
          let f_norm = eval(f)
          if t_norm == f_norm {
            t_norm
          } else {
            @syntax.Expr::BoolIf(c_norm, t_norm, f_norm)
          }
        }
      }
    }
    @syntax.Expr::TextLit(chunks) => {
      let result_chunks : Array[@syntax.TextChunk] = []
      let sb = StringBuilder()
      for chunk in chunks {
        match chunk {
          @syntax.TextChunk::Lit(s) => sb.write_string(s)
          @syntax.TextChunk::Expr(e) => {
            let e_norm = eval(e)
            match e_norm {
              @syntax.Expr::TextLit(sub_chunks) =>
                for sub in sub_chunks {
                  match sub {
                    @syntax.TextChunk::Lit(s) => sb.write_string(s)
                    @syntax.TextChunk::Expr(sub_e) => {
                      if sb.is_empty() == false {
                        result_chunks.push(
                          @syntax.TextChunk::Lit(sb.to_string()),
                        )
                        sb.reset()
                      }
                      result_chunks.push(@syntax.TextChunk::Expr(sub_e))
                    }
                  }
                }
              _ => {
                if sb.is_empty() == false {
                  result_chunks.push(@syntax.TextChunk::Lit(sb.to_string()))
                  sb.reset()
                }
                result_chunks.push(@syntax.TextChunk::Expr(e_norm))
              }
            }
          }
        }
      }
      if sb.is_empty() == false {
        result_chunks.push(@syntax.TextChunk::Lit(sb.to_string()))
      }
      @syntax.Expr::TextLit(result_chunks)
    }
    @syntax.Expr::ListLit(t, items) => {
      let t_norm = match t {
        Some(ty) => Some(eval(ty))
        None => None
      }
      let items_norm = items.map(eval)
      @syntax.Expr::ListLit(t_norm, items_norm)
    }
    @syntax.Expr::Some(inner) => @syntax.Expr::Some(eval(inner))
    @syntax.Expr::RecordLit(fields) => {
      let fields_norm = fields.map(fn(kv) { (kv.0, eval(kv.1)) })
      @syntax.Expr::RecordLit(sort_fields(fields_norm))
    }
    @syntax.Expr::RecordType(fields) => {
      let fields_norm = fields.map(fn(kv) { (kv.0, eval(kv.1)) })
      @syntax.Expr::RecordType(sort_fields(fields_norm))
    }
    @syntax.Expr::Field(target, field) => {
      let target_norm = eval(target)
      match target_norm {
        @syntax.Expr::RecordLit(fields) => {
          let mut found : @syntax.Expr? = None
          for kv in fields {
            if kv.0 == field {
              found = Some(kv.1)
              break
            }
          }
          match found {
            Some(v) => v
            None => @syntax.Expr::Field(target_norm, field)
          }
        }
        _ => @syntax.Expr::Field(target_norm, field)
      }
    }
    @syntax.Expr::Project(target, fields) => {
      let target_norm = eval(target)
      match target_norm {
        @syntax.Expr::RecordLit(kvs) => {
          let selected : Array[(String, @syntax.Expr)] = []
          for f in fields {
            for kv in kvs {
              if kv.0 == f {
                selected.push(kv)
                break
              }
            }
          }
          @syntax.Expr::RecordLit(sort_fields(selected))
        }
        _ => @syntax.Expr::Project(target_norm, fields)
      }
    }
    @syntax.Expr::ProjectByType(target, ty) =>
      @syntax.Expr::ProjectByType(eval(target), eval(ty))
    @syntax.Expr::UnionType(alts) => {
      let alts_norm = alts.map(fn(kv) {
        let ty_norm = match kv.1 {
          Some(ty) => Some(eval(ty))
          None => None
        }
        (kv.0, ty_norm)
      })
      let mut i = 1
      while i < alts_norm.length() {
        let key = alts_norm[i]
        let mut j = i - 1
        while j >= 0 && @syntax.lexical_compare(alts_norm[j].0, key.0) > 0 {
          alts_norm[j + 1] = alts_norm[j]
          j = j - 1
        }
        alts_norm[j + 1] = key
        i = i + 1
      }
      @syntax.Expr::UnionType(alts_norm)
    }
    @syntax.Expr::Merge(handlers, union, t) => {
      let h_norm = eval(handlers)
      let u_norm = eval(union)
      let t_norm = match t {
        Some(ty) => Some(eval(ty))
        None => None
      }
      match (h_norm, u_norm) {
        // merge { Some = \x -> x, None = default } (Some v)
        (@syntax.Expr::RecordLit(fields), @syntax.Expr::Some(v)) => {
          for kv in fields {
            if kv.0 == "Some" {
              return eval(@syntax.Expr::App(kv.1, v))
            }
          }
          @syntax.Expr::Merge(h_norm, u_norm, t_norm)
        }
        // merge { None = default, ... } (None _) or None
        (
          @syntax.Expr::RecordLit(fields),
          @syntax.Expr::Builtin(@syntax.Builtin::None),
        )
        | (
          @syntax.Expr::RecordLit(fields),
          @syntax.Expr::App(@syntax.Expr::Builtin(@syntax.Builtin::None), _),
        ) => {
          for kv in fields {
            if kv.0 == "None" {
              return eval(kv.1)
            }
          }
          @syntax.Expr::Merge(h_norm, u_norm, t_norm)
        }
        // merge handlers (< Left : Natural | Right : Bool >.Left 5)
        (
          @syntax.Expr::RecordLit(fields),
          @syntax.Expr::App(
            @syntax.Expr::Field(@syntax.Expr::UnionType(_), tag),
            v
          ),
        ) => {
          for kv in fields {
            if kv.0 == tag {
              return eval(@syntax.Expr::App(kv.1, v))
            }
          }
          @syntax.Expr::Merge(h_norm, u_norm, t_norm)
        }
        // merge handlers (< Red | Green | Blue >.Red)
        (
          @syntax.Expr::RecordLit(fields),
          @syntax.Expr::Field(@syntax.Expr::UnionType(_), tag),
        ) => {
          for kv in fields {
            if kv.0 == tag {
              return eval(kv.1)
            }
          }
          @syntax.Expr::Merge(h_norm, u_norm, t_norm)
        }
        _ => @syntax.Expr::Merge(h_norm, u_norm, t_norm)
      }
    }
    @syntax.Expr::ToMap(record, t) => {
      let r_norm = eval(record)
      let t_norm = match t {
        Some(ty) => Some(eval(ty))
        None => None
      }
      match r_norm {
        @syntax.Expr::RecordLit(fields) => {
          let items : Array[@syntax.Expr] = fields.map(fn(kv) {
            @syntax.Expr::RecordLit([
              ("mapKey", @syntax.Expr::text(kv.0)),
              ("mapValue", kv.1),
            ])
          })
          @syntax.Expr::ListLit(t_norm, items)
        }
        _ => @syntax.Expr::ToMap(r_norm, t_norm)
      }
    }
    @syntax.Expr::With(target, path, val) => {
      let t_norm = eval(target)
      let v_norm = eval(val)
      eval_with(t_norm, path, 0, v_norm)
    }
    @syntax.Expr::BinOp(op, l, r) => eval_binop(op, eval(l), eval(r))
    @syntax.Expr::Assert(e) => @syntax.Expr::Assert(eval(e))
  }
}

///|
fn eval_with(
  target : @syntax.Expr,
  path : Array[String],
  idx : Int32,
  val : @syntax.Expr,
) -> @syntax.Expr {
  match target {
    @syntax.Expr::RecordLit(fields) => {
      let k = path[idx]
      let new_fields : Array[(String, @syntax.Expr)] = []
      let mut found = false
      for kv in fields {
        if kv.0 == k {
          found = true
          let new_val = if idx == path.length() - 1 {
            val
          } else {
            eval_with(kv.1, path, idx + 1, val)
          }
          new_fields.push((k, new_val))
        } else {
          new_fields.push(kv)
        }
      }
      if found == false {
        let new_val = if idx == path.length() - 1 {
          val
        } else {
          eval_with(@syntax.Expr::RecordLit([]), path, idx + 1, val)
        }
        new_fields.push((k, new_val))
      }
      @syntax.Expr::RecordLit(sort_fields(new_fields))
    }
    _ => {
      let rem_path : Array[String] = []
      let mut i = idx
      while i < path.length() {
        rem_path.push(path[i])
        i = i + 1
      }
      @syntax.Expr::With(target, rem_path, val)
    }
  }
}

///|
/// Helper to reduce binary operators on normalized operands
fn eval_binop(
  op : @syntax.BinOp,
  l : @syntax.Expr,
  r : @syntax.Expr,
) -> @syntax.Expr {
  match (op, l, r) {
    // BoolOr
    (@syntax.BinOp::BoolOr, @syntax.Expr::BoolLit(true), _) =>
      @syntax.Expr::BoolLit(true)
    (@syntax.BinOp::BoolOr, _, @syntax.Expr::BoolLit(true)) =>
      @syntax.Expr::BoolLit(true)
    (@syntax.BinOp::BoolOr, @syntax.Expr::BoolLit(false), other) => other
    (@syntax.BinOp::BoolOr, other, @syntax.Expr::BoolLit(false)) => other

    // BoolAnd
    (@syntax.BinOp::BoolAnd, @syntax.Expr::BoolLit(false), _) =>
      @syntax.Expr::BoolLit(false)
    (@syntax.BinOp::BoolAnd, _, @syntax.Expr::BoolLit(false)) =>
      @syntax.Expr::BoolLit(false)
    (@syntax.BinOp::BoolAnd, @syntax.Expr::BoolLit(true), other) => other
    (@syntax.BinOp::BoolAnd, other, @syntax.Expr::BoolLit(true)) => other

    // BoolEQ
    (@syntax.BinOp::BoolEQ, @syntax.Expr::BoolLit(a), @syntax.Expr::BoolLit(b)) =>
      @syntax.Expr::BoolLit(a == b)
    (@syntax.BinOp::BoolEQ, @syntax.Expr::BoolLit(true), other) => other
    (@syntax.BinOp::BoolEQ, other, @syntax.Expr::BoolLit(true)) => other

    // BoolNE
    (@syntax.BinOp::BoolNE, @syntax.Expr::BoolLit(a), @syntax.Expr::BoolLit(b)) =>
      @syntax.Expr::BoolLit(a != b)
    (@syntax.BinOp::BoolNE, @syntax.Expr::BoolLit(false), other) => other
    (@syntax.BinOp::BoolNE, other, @syntax.Expr::BoolLit(false)) => other

    // NaturalPlus
    (
      @syntax.BinOp::NaturalPlus,
      @syntax.Expr::NaturalLit(a),
      @syntax.Expr::NaturalLit(b),
    ) => @syntax.Expr::NaturalLit(a + b)
    (@syntax.BinOp::NaturalPlus, @syntax.Expr::NaturalLit(0), other) => other
    (@syntax.BinOp::NaturalPlus, other, @syntax.Expr::NaturalLit(0)) => other

    // NaturalTimes
    (
      @syntax.BinOp::NaturalTimes,
      @syntax.Expr::NaturalLit(a),
      @syntax.Expr::NaturalLit(b),
    ) => @syntax.Expr::NaturalLit(a * b)
    (@syntax.BinOp::NaturalTimes, @syntax.Expr::NaturalLit(0), _) =>
      @syntax.Expr::NaturalLit(0)
    (@syntax.BinOp::NaturalTimes, _, @syntax.Expr::NaturalLit(0)) =>
      @syntax.Expr::NaturalLit(0)
    (@syntax.BinOp::NaturalTimes, @syntax.Expr::NaturalLit(1), other) => other
    (@syntax.BinOp::NaturalTimes, other, @syntax.Expr::NaturalLit(1)) => other

    // TextAppend
    (
      @syntax.BinOp::TextAppend,
      @syntax.Expr::TextLit(chunks1),
      @syntax.Expr::TextLit(chunks2),
    ) => {
      let combined = chunks1.copy()
      for c in chunks2 {
        combined.push(c)
      }
      eval(@syntax.Expr::TextLit(combined))
    }

    // ListAppend
    (
      @syntax.BinOp::ListAppend,
      @syntax.Expr::ListLit(t1, items1),
      @syntax.Expr::ListLit(t2, items2),
    ) => {
      let combined = items1.copy()
      for it in items2 {
        combined.push(it)
      }
      let t = match t1 {
        Some(_) => t1
        None => t2
      }
      @syntax.Expr::ListLit(t, combined)
    }

    // Prefer (//): record merge, right overrides left
    (
      @syntax.BinOp::Prefer,
      @syntax.Expr::RecordLit(left_kvs),
      @syntax.Expr::RecordLit(right_kvs),
    ) => {
      let res : Array[(String, @syntax.Expr)] = []
      // Add right entries
      for kv in right_kvs {
        res.push(kv)
      }
      // Add left entries that are not in right
      for l_kv in left_kvs {
        let mut exists = false
        for r_kv in right_kvs {
          if r_kv.0 == l_kv.0 {
            exists = true
            break
          }
        }
        if exists == false {
          res.push(l_kv)
        }
      }
      @syntax.Expr::RecordLit(sort_fields(res))
    }

    // CombineRecord (/\): recursive record merge
    (
      @syntax.BinOp::CombineRecord,
      @syntax.Expr::RecordLit(left_kvs),
      @syntax.Expr::RecordLit(right_kvs),
    ) => {
      let res : Array[(String, @syntax.Expr)] = []
      for r_kv in right_kvs {
        let mut merged = false
        for l_kv in left_kvs {
          if l_kv.0 == r_kv.0 {
            res.push(
              (
                r_kv.0,
                eval(
                  @syntax.Expr::BinOp(
                    @syntax.BinOp::CombineRecord,
                    l_kv.1,
                    r_kv.1,
                  ),
                ),
              ),
            )
            merged = true
            break
          }
        }
        if merged == false {
          res.push(r_kv)
        }
      }
      for l_kv in left_kvs {
        let mut exists = false
        for r_kv in right_kvs {
          if r_kv.0 == l_kv.0 {
            exists = true
            break
          }
        }
        if exists == false {
          res.push(l_kv)
        }
      }
      @syntax.Expr::RecordLit(sort_fields(res))
    }

    // CombineRecordType (//\\): recursive record type merge
    (
      @syntax.BinOp::CombineRecordType,
      @syntax.Expr::RecordType(left_kvs),
      @syntax.Expr::RecordType(right_kvs),
    ) => {
      let res : Array[(String, @syntax.Expr)] = []
      for r_kv in right_kvs {
        let mut merged = false
        for l_kv in left_kvs {
          if l_kv.0 == r_kv.0 {
            res.push(
              (
                r_kv.0,
                eval(
                  @syntax.Expr::BinOp(
                    @syntax.BinOp::CombineRecordType,
                    l_kv.1,
                    r_kv.1,
                  ),
                ),
              ),
            )
            merged = true
            break
          }
        }
        if merged == false {
          res.push(r_kv)
        }
      }
      for l_kv in left_kvs {
        let mut exists = false
        for r_kv in right_kvs {
          if r_kv.0 == l_kv.0 {
            exists = true
            break
          }
        }
        if exists == false {
          res.push(l_kv)
        }
      }
      @syntax.Expr::RecordType(sort_fields(res))
    }
    _ => @syntax.Expr::BinOp(op, l, r)
  }
}

///|
/// Helper to reduce builtin functions applied to arguments
fn reduce_app(f : @syntax.Expr, arg : @syntax.Expr) -> @syntax.Expr {
  match f {
    // 1-argument Builtins
    @syntax.Expr::Builtin(b) =>
      match b {
        @syntax.Builtin::NaturalIsZero =>
          match arg {
            @syntax.Expr::NaturalLit(n) => @syntax.Expr::BoolLit(n == 0)
            _ => @syntax.Expr::App(f, arg)
          }
        @syntax.Builtin::NaturalEven =>
          match arg {
            @syntax.Expr::NaturalLit(n) => @syntax.Expr::BoolLit(n % 2 == 0)
            _ => @syntax.Expr::App(f, arg)
          }
        @syntax.Builtin::NaturalOdd =>
          match arg {
            @syntax.Expr::NaturalLit(n) => @syntax.Expr::BoolLit(n % 2 != 0)
            _ => @syntax.Expr::App(f, arg)
          }
        @syntax.Builtin::NaturalToInteger =>
          match arg {
            @syntax.Expr::NaturalLit(n) => @syntax.Expr::IntegerLit(n)
            _ => @syntax.Expr::App(f, arg)
          }
        @syntax.Builtin::NaturalBuild => {
          let succ = @syntax.Expr::Lam(
            "n",
            @syntax.Expr::Builtin(@syntax.Builtin::Natural),
            @syntax.Expr::BinOp(
              @syntax.BinOp::NaturalPlus,
              @syntax.Expr::Var("n", 0),
              @syntax.Expr::NaturalLit(1),
            ),
          )
          let app1 = @syntax.Expr::App(
            arg,
            @syntax.Expr::Builtin(@syntax.Builtin::Natural),
          )
          let app2 = @syntax.Expr::App(app1, succ)
          let app3 = @syntax.Expr::App(app2, @syntax.Expr::NaturalLit(0))
          eval(app3)
        }
        @syntax.Builtin::NaturalShow =>
          match arg {
            @syntax.Expr::NaturalLit(n) => @syntax.Expr::text(n.to_string())
            _ => @syntax.Expr::App(f, arg)
          }
        @syntax.Builtin::IntegerShow =>
          match arg {
            @syntax.Expr::IntegerLit(n) =>
              if n >= 0 {
                @syntax.Expr::text("+" + n.to_string())
              } else {
                @syntax.Expr::text(n.to_string())
              }
            _ => @syntax.Expr::App(f, arg)
          }
        @syntax.Builtin::IntegerNegate =>
          match arg {
            @syntax.Expr::IntegerLit(n) => @syntax.Expr::IntegerLit(-n)
            _ => @syntax.Expr::App(f, arg)
          }
        @syntax.Builtin::IntegerClamp =>
          match arg {
            @syntax.Expr::IntegerLit(n) =>
              if n >= 0 {
                @syntax.Expr::NaturalLit(n)
              } else {
                @syntax.Expr::NaturalLit(0)
              }
            _ => @syntax.Expr::App(f, arg)
          }
        @syntax.Builtin::DoubleShow =>
          match arg {
            @syntax.Expr::DoubleLit(d) => @syntax.Expr::text(d.to_string())
            _ => @syntax.Expr::App(f, arg)
          }
        @syntax.Builtin::TextShow =>
          match arg {
            @syntax.Expr::TextLit(chunks) =>
              if chunks.length() == 1 {
                match chunks[0] {
                  @syntax.TextChunk::Lit(s) =>
                    // Escapes string into Dhall text representation
                    @syntax.Expr::text(s)
                  _ => @syntax.Expr::App(f, arg)
                }
              } else {
                @syntax.Expr::App(f, arg)
              }
            _ => @syntax.Expr::App(f, arg)
          }
        _ => @syntax.Expr::App(f, arg)
      }

    // 2-argument Builtins: App(App(Builtin, arg1), arg2)
    @syntax.Expr::App(@syntax.Expr::Builtin(b), arg1) =>
      match b {
        @syntax.Builtin::NaturalSubtract =>
          match (arg1, arg) {
            (@syntax.Expr::NaturalLit(a), @syntax.Expr::NaturalLit(b_val)) =>
              if b_val >= a {
                @syntax.Expr::NaturalLit(b_val - a)
              } else {
                @syntax.Expr::NaturalLit(0)
              }
            _ => @syntax.Expr::App(f, arg)
          }
        // List/length : forall (a : Type) -> List a -> Natural
        @syntax.Builtin::ListLength =>
          match arg {
            @syntax.Expr::ListLit(_, items) => {
              let len = items.length()
              let big = BigInt::from_int(len)
              @syntax.Expr::NaturalLit(big)
            }
            _ => @syntax.Expr::App(f, arg)
          }
        // List/reverse : forall (a : Type) -> List a -> List a
        @syntax.Builtin::ListReverse =>
          match arg {
            @syntax.Expr::ListLit(t, items) => {
              let rev = items.copy()
              rev.rev_in_place()
              @syntax.Expr::ListLit(t, rev)
            }
            _ => @syntax.Expr::App(f, arg)
          }
        // List/head : forall (a : Type) -> List a -> Optional a
        @syntax.Builtin::ListHead =>
          match arg {
            @syntax.Expr::ListLit(_, items) =>
              if items.is_empty() {
                @syntax.Expr::Builtin(@syntax.Builtin::None)
              } else {
                @syntax.Expr::Some(items[0])
              }
            _ => @syntax.Expr::App(f, arg)
          }
        // List/last : forall (a : Type) -> List a -> Optional a
        @syntax.Builtin::ListLast =>
          match arg {
            @syntax.Expr::ListLit(_, items) =>
              if items.is_empty() {
                @syntax.Expr::Builtin(@syntax.Builtin::None)
              } else {
                @syntax.Expr::Some(items[items.length() - 1])
              }
            _ => @syntax.Expr::App(f, arg)
          }
        // List/indexed : forall (a : Type) -> List a -> List { index : Natural, value : a }
        @syntax.Builtin::ListIndexed =>
          match arg {
            @syntax.Expr::ListLit(_, items) => {
              let res : Array[@syntax.Expr] = []
              let mut i = BigInt::from_int(0)
              for item in items {
                res.push(
                  @syntax.Expr::RecordLit([
                    ("index", @syntax.Expr::NaturalLit(i)),
                    ("value", item),
                  ]),
                )
                i = i + 1
              }
              @syntax.Expr::ListLit(None, res)
            }
            _ => @syntax.Expr::App(f, arg)
          }
        // List/build : forall (a : Type) -> (forall (list : Type) -> (a -> list -> list) -> list -> list) -> List a
        @syntax.Builtin::ListBuild => {
          let ty_list_a = @syntax.Expr::App(
            @syntax.Expr::Builtin(@syntax.Builtin::List),
            arg1,
          )
          let cons = @syntax.Expr::Lam(
            "x",
            arg1,
            @syntax.Expr::Lam(
              "xs",
              ty_list_a,
              @syntax.Expr::BinOp(
                @syntax.BinOp::ListAppend,
                @syntax.Expr::ListLit(None, [@syntax.Expr::Var("x", 0)]),
                @syntax.Expr::Var("xs", 0),
              ),
            ),
          )
          let nil = @syntax.Expr::ListLit(Some(arg1), [])
          let app1 = @syntax.Expr::App(arg, ty_list_a)
          let app2 = @syntax.Expr::App(app1, cons)
          let app3 = @syntax.Expr::App(app2, nil)
          eval(app3)
        }
        _ => @syntax.Expr::App(f, arg)
      }

    // 4-argument NaturalFold: Natural/fold n t succ zero
    @syntax.Expr::App(
      @syntax.Expr::App(
        @syntax.Expr::App(
          @syntax.Expr::Builtin(@syntax.Builtin::NaturalFold),
          n
        ),
        _t
      ),
      succ
    ) =>
      match n {
        @syntax.Expr::NaturalLit(count) => {
          let mut res = arg // zero
          let mut i = BigInt::from_int(0)
          while i < count {
            res = eval(@syntax.Expr::App(succ, res))
            i = i + 1
          }
          res
        }
        _ => @syntax.Expr::App(f, arg)
      }

    // 5-argument ListFold: List/fold a xs list cons nil
    @syntax.Expr::App(
      @syntax.Expr::App(
        @syntax.Expr::App(
          @syntax.Expr::App(
            @syntax.Expr::Builtin(@syntax.Builtin::ListFold),
            _a
          ),
          xs
        ),
        _list
      ),
      cons
    ) =>
      match xs {
        @syntax.Expr::ListLit(_, items) => {
          let mut res = arg // nil
          let mut i = items.length() - 1
          while i >= 0 {
            res = eval(
              @syntax.Expr::App(@syntax.Expr::App(cons, items[i]), res),
            )
            i = i - 1
          }
          res
        }
        _ => @syntax.Expr::App(f, arg)
      }
    _ => @syntax.Expr::App(f, arg)
  }
}
