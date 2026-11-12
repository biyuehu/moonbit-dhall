///|
/// Typing context Gamma tracking types of bound variables
pub struct Context {
  bindings : Array[(String, @syntax.Expr)]
}

///|
/// Creates an empty Context
pub fn Context::new() -> Context {
  { bindings: [], }
}

///|
/// Pushes a new variable binding onto the context
pub fn Context::push(
  self : Context,
  name : String,
  type_ : @syntax.Expr,
) -> Context {
  let new_bindings = self.bindings.copy()
  new_bindings.push((name, type_))
  { bindings: new_bindings, }
}

///|
/// Looks up variable (name, index) in context and shifts its type appropriately
pub fn Context::lookup(
  self : Context,
  name : String,
  index : Int32,
) -> @syntax.Expr? {
  let mut count = 0
  let mut target_idx = -1
  let mut i = self.bindings.length() - 1
  while i >= 0 {
    if self.bindings[i].0 == name {
      if count == index {
        target_idx = i
        break
      }
      count = count + 1
    }
    i = i - 1
  }
  if target_idx < 0 {
    return None
  }
  let mut ty = self.bindings[target_idx].1
  let mut j = target_idx + 1
  while j < self.bindings.length() {
    let binder_name = self.bindings[j].0
    ty = @core.shift(1, binder_name, 0, ty)
    j = j + 1
  }
  Some(ty)
}
