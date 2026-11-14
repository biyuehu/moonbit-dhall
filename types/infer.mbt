///|
/// Checks if two types are equivalent modulo β-normalization and α-equivalence
fn match_types(a : @syntax.Expr, b : @syntax.Expr) -> Bool {
  let na = @core.eval(a)
  let nb = @core.eval(b)
  @core.alpha_eq(na, nb)
}

///|
/// Helper to construct arrow type `A -> B`
fn arrow(a : @syntax.Expr, b : @syntax.Expr) -> @syntax.Expr {
  @syntax.Expr::Pi("_", a, b)
}

///|
/// Helper to get the type of a Builtin
fn builtin_type(b : @syntax.Builtin) -> @syntax.Expr {
  let ty_bool = @syntax.Expr::Builtin(@syntax.Builtin::Bool)
  let ty_nat = @syntax.Expr::Builtin(@syntax.Builtin::Natural)
  let ty_int = @syntax.Expr::Builtin(@syntax.Builtin::Integer)
  let ty_double = @syntax.Expr::Builtin(@syntax.Builtin::Double)
  let ty_text = @syntax.Expr::Builtin(@syntax.Builtin::Text)
  let ty_list = fn(t) {
    @syntax.Expr::App(@syntax.Expr::Builtin(@syntax.Builtin::List), t)
  }
  let ty_opt = fn(t) {
    @syntax.Expr::App(@syntax.Expr::Builtin(@syntax.Builtin::Optional), t)
  }
  let c_type = @syntax.Expr::Const(@syntax.Const::Type)

  match b {
    // Primitive types have type Type
    @syntax.Builtin::Bool
    | @syntax.Builtin::Natural
    | @syntax.Builtin::Integer
    | @syntax.Builtin::Double
    | @syntax.Builtin::Text => c_type

    // List : Type -> Type
    @syntax.Builtin::List => arrow(c_type, c_type)

    // Optional : Type -> Type
    @syntax.Builtin::Optional => arrow(c_type, c_type)

    // None : forall (A : Type) -> Optional A
    @syntax.Builtin::None =>
      @syntax.Expr::Pi("A", c_type, ty_opt(@syntax.Expr::Var("A", 0)))

    // Natural operations
    @syntax.Builtin::NaturalIsZero
    | @syntax.Builtin::NaturalEven
    | @syntax.Builtin::NaturalOdd => arrow(ty_nat, ty_bool)
    @syntax.Builtin::NaturalToInteger => arrow(ty_nat, ty_int)
    @syntax.Builtin::NaturalShow => arrow(ty_nat, ty_text)
    @syntax.Builtin::NaturalSubtract => arrow(ty_nat, arrow(ty_nat, ty_nat))

    // Natural/fold : Natural -> forall (natural : Type) -> (natural -> natural) -> natural -> natural
    @syntax.Builtin::NaturalFold =>
      arrow(
        ty_nat,
        @syntax.Expr::Pi(
          "natural",
          c_type,
          arrow(
            arrow(
              @syntax.Expr::Var("natural", 0),
              @syntax.Expr::Var("natural", 0),
            ),
            arrow(
              @syntax.Expr::Var("natural", 0),
              @syntax.Expr::Var("natural", 0),
            ),
          ),
        ),
      )

    // Natural/build : (forall (natural : Type) -> (natural -> natural) -> natural -> natural) -> Natural
    @syntax.Builtin::NaturalBuild =>
      arrow(
        @syntax.Expr::Pi(
          "natural",
          c_type,
          arrow(
            arrow(
              @syntax.Expr::Var("natural", 0),
              @syntax.Expr::Var("natural", 0),
            ),
            arrow(
              @syntax.Expr::Var("natural", 0),
              @syntax.Expr::Var("natural", 0),
            ),
          ),
        ),
        ty_nat,
      )

    // Integer operations
    @syntax.Builtin::IntegerShow => arrow(ty_int, ty_text)
    @syntax.Builtin::IntegerToDouble => arrow(ty_int, ty_double)
    @syntax.Builtin::IntegerNegate => arrow(ty_int, ty_int)
    @syntax.Builtin::IntegerClamp => arrow(ty_int, ty_nat)

    // Double operations
    @syntax.Builtin::DoubleShow => arrow(ty_double, ty_text)

    // Text operations
    @syntax.Builtin::TextShow => arrow(ty_text, ty_text)
    @syntax.Builtin::TextReplace =>
      arrow(ty_text, arrow(ty_text, arrow(ty_text, ty_text)))

    // List operations
    // List/length : forall (a : Type) -> List a -> Natural
    @syntax.Builtin::ListLength =>
      @syntax.Expr::Pi(
        "a",
        c_type,
        arrow(ty_list(@syntax.Expr::Var("a", 0)), ty_nat),
      )

    // List/reverse : forall (a : Type) -> List a -> List a
    @syntax.Builtin::ListReverse =>
      @syntax.Expr::Pi(
        "a",
        c_type,
        arrow(
          ty_list(@syntax.Expr::Var("a", 0)),
          ty_list(@syntax.Expr::Var("a", 0)),
        ),
      )

    // List/head : forall (a : Type) -> List a -> Optional a
    @syntax.Builtin::ListHead =>
      @syntax.Expr::Pi(
        "a",
        c_type,
        arrow(
          ty_list(@syntax.Expr::Var("a", 0)),
          ty_opt(@syntax.Expr::Var("a", 0)),
        ),
      )

    // List/last : forall (a : Type) -> List a -> Optional a
    @syntax.Builtin::ListLast =>
      @syntax.Expr::Pi(
        "a",
        c_type,
        arrow(
          ty_list(@syntax.Expr::Var("a", 0)),
          ty_opt(@syntax.Expr::Var("a", 0)),
        ),
      )

    // List/fold : forall (a : Type) -> List a -> forall (list : Type) -> (a -> list -> list) -> list -> list
    @syntax.Builtin::ListFold =>
      @syntax.Expr::Pi(
        "a",
        c_type,
        arrow(
          ty_list(@syntax.Expr::Var("a", 0)),
          @syntax.Expr::Pi(
            "list",
            c_type,
            arrow(
              arrow(
                @syntax.Expr::Var("a", 1),
                arrow(
                  @syntax.Expr::Var("list", 0),
                  @syntax.Expr::Var("list", 0),
                ),
              ),
              arrow(@syntax.Expr::Var("list", 0), @syntax.Expr::Var("list", 0)),
            ),
          ),
        ),
      )

    // List/build : forall (a : Type) -> (forall (list : Type) -> (a -> list -> list) -> list -> list) -> List a
    @syntax.Builtin::ListBuild =>
      @syntax.Expr::Pi(
        "a",
        c_type,
        arrow(
          @syntax.Expr::Pi(
            "list",
            c_type,
            arrow(
              arrow(
                @syntax.Expr::Var("a", 1),
                arrow(
                  @syntax.Expr::Var("list", 0),
                  @syntax.Expr::Var("list", 0),
                ),
              ),
              arrow(@syntax.Expr::Var("list", 0), @syntax.Expr::Var("list", 0)),
            ),
          ),
          ty_list(@syntax.Expr::Var("a", 0)),
        ),
      )

    // List/indexed : forall (a : Type) -> List a -> List { index : Natural, value : a }
    @syntax.Builtin::ListIndexed =>
      @syntax.Expr::Pi(
        "a",
        c_type,
        arrow(
          ty_list(@syntax.Expr::Var("a", 0)),
          ty_list(
            @syntax.Expr::RecordType([
              ("index", ty_nat),
              ("value", @syntax.Expr::Var("a", 0)),
            ]),
          ),
        ),
      )
  }
}

///|
/// Infers the type of a Dhall expression within a given Context
pub fn type_with(
  ctx : Context,
  expr : @syntax.Expr,
) -> @syntax.Expr raise TypeError {
  match expr {
    // Axioms
    @syntax.Expr::Const(c) =>
      match c {
        @syntax.Const::Type => @syntax.Expr::Const(@syntax.Const::Kind)
        @syntax.Const::Kind => @syntax.Expr::Const(@syntax.Const::Sort)
        @syntax.Const::Sort => raise Message("Sort has no type")
      }

    // Builtins
    @syntax.Expr::Builtin(b) => builtin_type(b)

    // Variables
    @syntax.Expr::Var(name, index) =>
      match ctx.lookup(name, index) {
        Some(ty) => ty
        None =>
          raise Message("Unbound variable: " + name + "@" + index.to_string())
      }

    // Literals
    @syntax.Expr::BoolLit(_) => @syntax.Expr::Builtin(@syntax.Builtin::Bool)
    @syntax.Expr::NaturalLit(_) =>
      @syntax.Expr::Builtin(@syntax.Builtin::Natural)
    @syntax.Expr::IntegerLit(_) =>
      @syntax.Expr::Builtin(@syntax.Builtin::Integer)
    @syntax.Expr::DoubleLit(_) => @syntax.Expr::Builtin(@syntax.Builtin::Double)
    @syntax.Expr::TextLit(chunks) => {
      let expected_text = @syntax.Expr::Builtin(@syntax.Builtin::Text)
      for chunk in chunks {
        match chunk {
          @syntax.TextChunk::Lit(_) => ()
          @syntax.TextChunk::Expr(e) => {
            let t = type_with(ctx, e)
            if match_types(t, expected_text) == false {
              raise Message("Interpolated expression must have type Text")
            }
          }
        }
      }
      expected_text
    }

    // List literals
    @syntax.Expr::ListLit(opt_ty, items) =>
      if items.is_empty() {
        match opt_ty {
          Some(t) => {
            let _ = type_with(ctx, t)
            @syntax.Expr::App(@syntax.Expr::Builtin(@syntax.Builtin::List), t)
          }
          None =>
            raise Message(
              "Cannot infer type of empty list literal without type annotation",
            )
        }
      } else {
        let first_ty = type_with(ctx, items[0])
        let mut i = 1
        while i < items.length() {
          let item_ty = type_with(ctx, items[i])
          if match_types(item_ty, first_ty) == false {
            raise Message("List element type mismatch in list literal")
          }
          i = i + 1
        }
        match opt_ty {
          Some(annot_ty) =>
            if match_types(first_ty, annot_ty) == false {
              raise Message("List elements do not match annotated element type")
            }
          None => ()
        }
        @syntax.Expr::App(
          @syntax.Expr::Builtin(@syntax.Builtin::List),
          first_ty,
        )
      }

    // Optional Some
    @syntax.Expr::Some(inner) => {
      let inner_ty = type_with(ctx, inner)
      @syntax.Expr::App(
        @syntax.Expr::Builtin(@syntax.Builtin::Optional),
        inner_ty,
      )
    }

    // Records
    @syntax.Expr::RecordLit(fields) => {
      let type_fields : Array[(String, @syntax.Expr)] = []
      for kv in fields {
        let t = type_with(ctx, kv.1)
        type_fields.push((kv.0, t))
      }
      @syntax.Expr::RecordType(type_fields)
    }
    @syntax.Expr::RecordType(fields) => {
      for kv in fields {
        let t = type_with(ctx, kv.1)
        let t_norm = @core.eval(t)
        match t_norm {
          @syntax.Expr::Const(_) => ()
          _ => raise Message("Record field type must live in a universe")
        }
      }
      @syntax.Expr::Const(@syntax.Const::Type)
    }
    @syntax.Expr::UnionType(alts) => {
      for kv in alts {
        match kv.1 {
          Some(t) => {
            let ty = type_with(ctx, t)
            let ty_norm = @core.eval(ty)
            match ty_norm {
              @syntax.Expr::Const(_) => ()
              _ =>
                raise Message("Union alternative type must live in a universe")
            }
          }
          None => ()
        }
      }
      @syntax.Expr::Const(@syntax.Const::Type)
    }

    // Lambda abstraction: \(x : A) -> b
    @syntax.Expr::Lam(param, param_ty, body) => {
      // Check param_ty is a valid type
      let _ = type_with(ctx, param_ty)
      let body_ctx = ctx.push(param, param_ty)
      let body_ty = type_with(body_ctx, body)
      @syntax.Expr::Pi(param, param_ty, body_ty)
    }

    // Pi product type: forall (x : A) -> B
    @syntax.Expr::Pi(param, domain, codomain) => {
      let dom_u = @core.eval(type_with(ctx, domain))
      let cod_ctx = ctx.push(param, domain)
      let cod_u = @core.eval(type_with(cod_ctx, codomain))

      match (dom_u, cod_u) {
        (@syntax.Expr::Const(c1), @syntax.Expr::Const(c2)) =>
          // Dhall function type universe rules
          if c2 == @syntax.Const::Type {
            @syntax.Expr::Const(@syntax.Const::Type)
          } else if c2 == @syntax.Const::Kind {
            if c1 == @syntax.Const::Sort {
              @syntax.Expr::Const(@syntax.Const::Sort)
            } else {
              @syntax.Expr::Const(@syntax.Const::Kind)
            }
          } else {
            @syntax.Expr::Const(@syntax.Const::Sort)
          }
        _ =>
          raise Message(
            "Pi domain and codomain must be valid types in universes",
          )
      }
    }

    // Function application: f a
    @syntax.Expr::App(f, arg) => {
      let f_ty = @core.eval(type_with(ctx, f))
      match f_ty {
        @syntax.Expr::Pi(param, domain, codomain) => {
          let arg_ty = type_with(ctx, arg)
          if match_types(arg_ty, domain) == false {
            raise Message(
              "Function argument type mismatch: expected " +
              domain.to_string() +
              " but got " +
              arg_ty.to_string(),
            )
          }
          let substituted = @core.subst(param, 0, arg, codomain)
          let shifted = @core.shift(-1, param, 0, substituted)
          @core.eval(shifted)
        }
        _ =>
          raise Message(
            "Cannot apply expression that is not a function: " + f.to_string(),
          )
      }
    }

    // Let binding: let x [: T] = r in e
    @syntax.Expr::Let(name, opt_ty, val, body) => {
      let val_ty = type_with(ctx, val)
      match opt_ty {
        Some(expected_ty) =>
          if match_types(val_ty, expected_ty) == false {
            raise Message("Let binding value does not match annotated type")
          }
        None => ()
      }
      let subst_body = @core.subst(name, 0, val, body)
      let shifted_body = @core.shift(-1, name, 0, subst_body)
      type_with(ctx, shifted_body)
    }

    // Annotation: e : T
    @syntax.Expr::Annot(e, expected_ty) => {
      let _ = type_with(ctx, expected_ty)
      let inferred = type_with(ctx, e)
      if match_types(inferred, expected_ty) == false {
        raise Message(
          "Expression type does not match annotation: expected " +
          expected_ty.to_string() +
          " but got " +
          inferred.to_string(),
        )
      }
      expected_ty
    }

    // Conditionals: if c then t else f
    @syntax.Expr::BoolIf(c, t, f) => {
      let c_ty = type_with(ctx, c)
      if match_types(c_ty, @syntax.Expr::Builtin(@syntax.Builtin::Bool)) ==
        false {
        raise Message("Condition in if-expression must be of type Bool")
      }
      let t_ty = type_with(ctx, t)
      let f_ty = type_with(ctx, f)
      if match_types(t_ty, f_ty) == false {
        raise Message("Branches of if-expression must have matching types")
      }
      t_ty
    }

    // Field access: r.k
    @syntax.Expr::Field(target, field_name) => {
      let target_norm = @core.eval(target)
      match target_norm {
        @syntax.Expr::UnionType(alts) => {
          let mut found : @syntax.Expr?? = None
          for kv in alts {
            if kv.0 == field_name {
              found = Some(kv.1)
              break
            }
          }
          match found {
            Some(opt_inner_ty) =>
              match opt_inner_ty {
                Some(inner_ty) => arrow(inner_ty, target_norm)
                None => target_norm
              }
            None =>
              raise Message("Union does not have constructor: " + field_name)
          }
        }
        _ => {
          let target_ty = @core.eval(type_with(ctx, target))
          match target_ty {
            @syntax.Expr::RecordType(fields) => {
              let mut found : @syntax.Expr? = None
              for kv in fields {
                if kv.0 == field_name {
                  found = Some(kv.1)
                  break
                }
              }
              match found {
                Some(ty) => ty
                None =>
                  raise Message("Record does not have field: " + field_name)
              }
            }
            _ => raise Message("Field access on non-record target")
          }
        }
      }
    }

    // Projection: r.{ a, b }
    @syntax.Expr::Project(target, field_names) => {
      let target_ty = @core.eval(type_with(ctx, target))
      match target_ty {
        @syntax.Expr::RecordType(fields) => {
          let res : Array[(String, @syntax.Expr)] = []
          for req in field_names {
            let mut found = false
            for kv in fields {
              if kv.0 == req {
                res.push(kv)
                found = true
                break
              }
            }
            if found == false {
              raise Message("Record missing projected field: " + req)
            }
          }
          @syntax.Expr::RecordType(res)
        }
        _ => raise Message("Projection on non-record target")
      }
    }
    @syntax.Expr::ProjectByType(target, ty_expr) => {
      let ty_norm = @core.eval(ty_expr)
      match ty_norm {
        @syntax.Expr::RecordType(expected_fields) => {
          let target_ty = @core.eval(type_with(ctx, target))
          match target_ty {
            @syntax.Expr::RecordType(actual_fields) => {
              for exp in expected_fields {
                let mut found = false
                for act in actual_fields {
                  if act.0 == exp.0 {
                    if match_types(act.1, exp.1) == false {
                      raise Message(
                        "Type mismatch in project by type for field: " + exp.0,
                      )
                    }
                    found = true
                    break
                  }
                }
                if found == false {
                  raise Message("Missing field in project by type: " + exp.0)
                }
              }
              ty_norm
            }
            _ => raise Message("Project by type target must be a record")
          }
        }
        _ => raise Message("Project by type expression must be a record type")
      }
    }
    @syntax.Expr::With(target, path, val) => {
      let target_ty = @core.eval(type_with(ctx, target))
      let val_ty = type_with(ctx, val)
      infer_with_type(ctx, target_ty, path, 0, val_ty)
    }

    // Binary operators
    @syntax.Expr::BinOp(op, l, r) => {
      let l_ty = type_with(ctx, l)
      let r_ty = type_with(ctx, r)
      let ty_bool = @syntax.Expr::Builtin(@syntax.Builtin::Bool)
      let ty_nat = @syntax.Expr::Builtin(@syntax.Builtin::Natural)
      let ty_text = @syntax.Expr::Builtin(@syntax.Builtin::Text)

      match op {
        @syntax.BinOp::BoolOr
        | @syntax.BinOp::BoolAnd
        | @syntax.BinOp::BoolEQ
        | @syntax.BinOp::BoolNE => {
          if match_types(l_ty, ty_bool) == false ||
            match_types(r_ty, ty_bool) == false {
            raise Message("Boolean operator requires Bool operands")
          }
          ty_bool
        }
        @syntax.BinOp::NaturalPlus | @syntax.BinOp::NaturalTimes => {
          if match_types(l_ty, ty_nat) == false ||
            match_types(r_ty, ty_nat) == false {
            raise Message("Arithmetic operator requires Natural operands")
          }
          ty_nat
        }
        @syntax.BinOp::TextAppend => {
          if match_types(l_ty, ty_text) == false ||
            match_types(r_ty, ty_text) == false {
            raise Message("Text append requires Text operands")
          }
          ty_text
        }
        @syntax.BinOp::ListAppend => {
          let l_norm = @core.eval(l_ty)
          let r_norm = @core.eval(r_ty)
          match (l_norm, r_norm) {
            (
              @syntax.Expr::App(
                @syntax.Expr::Builtin(@syntax.Builtin::List),
                t1
              ),
              @syntax.Expr::App(
                @syntax.Expr::Builtin(@syntax.Builtin::List),
                t2
              ),
            ) => {
              if match_types(t1, t2) == false {
                raise Message(
                  "List append requires lists of matching element types",
                )
              }
              l_norm
            }
            _ => raise Message("List append (#) requires List operands")
          }
        }
        @syntax.BinOp::Prefer => {
          let l_norm = @core.eval(l_ty)
          let r_norm = @core.eval(r_ty)
          match (l_norm, r_norm) {
            (@syntax.Expr::RecordType(f1), @syntax.Expr::RecordType(f2)) => {
              let res : Array[(String, @syntax.Expr)] = []
              for kv in f2 {
                res.push(kv)
              }
              for kv1 in f1 {
                let mut exists = false
                for kv2 in f2 {
                  if kv2.0 == kv1.0 {
                    exists = true
                    break
                  }
                }
                if exists == false {
                  res.push(kv1)
                }
              }
              @syntax.Expr::RecordType(res)
            }
            _ => raise Message("Prefer (//) requires Record operands")
          }
        }
        @syntax.BinOp::CombineRecord => {
          let l_norm = @core.eval(l_ty)
          let r_norm = @core.eval(r_ty)
          match (l_norm, r_norm) {
            (@syntax.Expr::RecordType(f1), @syntax.Expr::RecordType(f2)) => {
              let res : Array[(String, @syntax.Expr)] = []
              for kv in f1 {
                res.push(kv)
              }
              for kv2 in f2 {
                let mut merged = false
                let mut i = 0
                while i < res.length() {
                  if res[i].0 == kv2.0 {
                    let combined_t = @syntax.Expr::BinOp(
                      @syntax.BinOp::CombineRecord,
                      res[i].1,
                      kv2.1,
                    )
                    res[i] = (kv2.0, combined_t)
                    merged = true
                    break
                  }
                  i = i + 1
                }
                if merged == false {
                  res.push(kv2)
                }
              }
              @syntax.Expr::RecordType(res)
            }
            _ => raise Message("Combine (/\\) requires Record operands")
          }
        }
        @syntax.BinOp::Equivalent => {
          if match_types(l_ty, r_ty) == false {
            raise Message(
              "Equivalence check (===) requires operands of matching types",
            )
          }
          @syntax.Expr::Const(@syntax.Const::Type)
        }
        @syntax.BinOp::CombineRecordType => {
          let l_norm = @core.eval(l_ty)
          let r_norm = @core.eval(r_ty)
          match (l_norm, r_norm) {
            (@syntax.Expr::Const(c1), @syntax.Expr::Const(c2)) => {
              let l_val = @core.eval(l)
              let r_val = @core.eval(r)
              match (l_val, r_val) {
                (@syntax.Expr::RecordType(f1), @syntax.Expr::RecordType(f2)) => {
                  let res : Array[(String, @syntax.Expr)] = []
                  for kv in f1 {
                    res.push(kv)
                  }
                  for kv2 in f2 {
                    let mut merged = false
                    let mut i = 0
                    while i < res.length() {
                      if res[i].0 == kv2.0 {
                        let combined_t = @syntax.Expr::BinOp(
                          @syntax.BinOp::CombineRecordType,
                          res[i].1,
                          kv2.1,
                        )
                        res[i] = (kv2.0, combined_t)
                        merged = true
                        break
                      }
                      i = i + 1
                    }
                    if merged == false {
                      res.push(kv2)
                    }
                  }
                  let res_type = @syntax.Expr::RecordType(res)
                  let _ = type_with(ctx, res_type)
                  let max_c = if c1 == @syntax.Const::Sort ||
                    c2 == @syntax.Const::Sort {
                    @syntax.Const::Sort
                  } else if c1 == @syntax.Const::Kind ||
                    c2 == @syntax.Const::Kind {
                    @syntax.Const::Kind
                  } else {
                    @syntax.Const::Type
                  }
                  @syntax.Expr::Const(max_c)
                }
                _ =>
                  raise Message(
                    "CombineRecordType (//\\\\) requires record type operands",
                  )
              }
            }
            _ =>
              raise Message(
                "CombineRecordType (//\\\\) operands must be record types",
              )
          }
        }
      }
    }

    // Assert: assert : e
    @syntax.Expr::Assert(e) => {
      let e_ty = @core.eval(type_with(ctx, e))
      match e_ty {
        @syntax.Expr::Const(@syntax.Const::Type) => ()
        _ => raise Message("Asserted expression must be of type Type")
      }
      match e {
        @syntax.Expr::BinOp(@syntax.BinOp::Equivalent, l, r) =>
          if @core.eval(l) != @core.eval(r) {
            raise Message("Assertion failed: expressions are not equivalent")
          }
        _ => raise Message("Assert expression must be an equivalence (===)")
      }
      e
    }

    // Merge: merge handlers union
    @syntax.Expr::Merge(handlers, union, opt_ty) => {
      let h_ty = @core.eval(type_with(ctx, handlers))
      let u_ty = @core.eval(type_with(ctx, union))
      match (h_ty, u_ty) {
        (@syntax.Expr::RecordType(h_fields), @syntax.Expr::UnionType(u_alts)) => {
          if h_fields.length() != u_alts.length() {
            raise Message(
              "Handlers count does not match union alternatives count",
            )
          }
          match opt_ty {
            Some(t) => t
            None => {
              if h_fields.is_empty() {
                raise Message(
                  "Cannot infer return type of empty merge without annotation",
                )
              }
              let first_h = h_fields[0].1
              let first_h_ty = @core.eval(type_with(ctx, first_h))
              match first_h_ty {
                @syntax.Expr::Pi(_, _, cod) => cod
                other => other
              }
            }
          }
        }
        _ =>
          raise Message("Merge requires a record of handlers and a union value")
      }
    }

    // ToMap
    @syntax.Expr::ToMap(rec, opt_ty) => {
      let r_ty = @core.eval(type_with(ctx, rec))
      match r_ty {
        @syntax.Expr::RecordType(fields) =>
          if fields.is_empty() {
            match opt_ty {
              Some(t) => t
              None =>
                raise Message(
                  "Cannot infer toMap type on empty record without annotation",
                )
            }
          } else {
            let elem_ty = fields[0].1
            let entry_ty = @syntax.Expr::RecordType([
              ("mapKey", @syntax.Expr::Builtin(@syntax.Builtin::Text)),
              ("mapValue", elem_ty),
            ])
            @syntax.Expr::App(
              @syntax.Expr::Builtin(@syntax.Builtin::List),
              entry_ty,
            )
          }
        _ => raise Message("toMap requires a Record operand")
      }
    }
  }
}

///|
fn infer_with_type(
  ctx : Context,
  target_ty : @syntax.Expr,
  path : Array[String],
  idx : Int32,
  val_ty : @syntax.Expr,
) -> @syntax.Expr raise TypeError {
  match target_ty {
    @syntax.Expr::RecordType(fields) => {
      let k = path[idx]
      let new_fields : Array[(String, @syntax.Expr)] = []
      let mut found = false
      for kv in fields {
        if kv.0 == k {
          found = true
          let new_field_ty = if idx == path.length() - 1 {
            val_ty
          } else {
            infer_with_type(ctx, kv.1, path, idx + 1, val_ty)
          }
          new_fields.push((k, new_field_ty))
        } else {
          new_fields.push(kv)
        }
      }
      if found == false {
        let new_field_ty = if idx == path.length() - 1 {
          val_ty
        } else {
          infer_with_type(
            ctx,
            @syntax.Expr::RecordType([]),
            path,
            idx + 1,
            val_ty,
          )
        }
        new_fields.push((k, new_field_ty))
      }
      @syntax.Expr::RecordType(new_fields)
    }
    _ => raise Message("with expression target must be a record")
  }
}

///|
/// Infers the type of an expression in the empty context
pub fn type_of(expr : @syntax.Expr) -> Result[@syntax.Expr, String] {
  try {
    let ty = type_with(Context::new(), expr)
    Ok(@core.eval(ty))
  } catch {
    Message(msg) => Err(msg)
  }
}
