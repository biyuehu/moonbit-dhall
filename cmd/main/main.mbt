///|
/// CLI Entry point for moonbit-dhall
fn main {
  println("== moonbit-dhall CLI ==")

  let dhall_src =
    #|let base_port = 8000
    #|in {
    #|  server = "127.0.0.1",
    #|  port = base_port + 80,
    #|  endpoints = ["/health", "/api/v1"],
    #|  debug = True
    #|}

  println("\n[1. Raw Dhall Source]")
  println(dhall_src)

  match @lib.parse(dhall_src) {
    Ok(ast) => {
      println("\n[2. Parsed AST]")
      println(ast.to_string())

      let normalized = @lib.eval(ast)
      println("\n[3. Normalized (Beta-reduced)]")
      println(normalized.to_string())

      println("\n[4. Exported Canonical JSON]")
      match @lib.expr_to_json_canonical(normalized) {
        Ok(json) => println(json)
        Err(err) => println("Error exporting to JSON: " + err)
      }
    }
    Err(err) => println("Parse error: " + err)
  }
}
