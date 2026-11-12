///|
test "end-to-end dhall to json with computation" {
  let dhall_src =
    #|let base_port = 8000
    #|in {
    #|  host = "localhost",
    #|  port = base_port + 80,
    #|  tags = ["api", "v1"]
    #|}
  let json_str = @moonbit-dhall.to_json_str(dhall_src)
  assert_eq(
    json_str,
    Ok("{\"host\":\"localhost\",\"port\":8080,\"tags\":[\"api\",\"v1\"]}"),
  )
}

///|
test "end-to-end builtins evaluation" {
  assert_eq(@moonbit-dhall.to_json_str("Natural/isZero 0"), Ok("true"))
  assert_eq(@moonbit-dhall.to_json_str("Natural/isZero 5"), Ok("false"))
  assert_eq(
    @moonbit-dhall.to_json_str("List/reverse Natural [1, 2, 3]"),
    Ok("[3,2,1]"),
  )
}

///|
test "end-to-end record merge" {
  let src = "{ a = 1, b = 2 } // { b = 99, c = 3 }"
  assert_eq(@moonbit-dhall.to_json_str(src), Ok("{\"a\":1,\"b\":99,\"c\":3}"))
}

///|
test "end-to-end type check" {
  let src = "\\(x : Natural) -> x + 1"
  let ty = @moonbit-dhall.type_check_str(src)
  assert_true(ty is Ok(_))

  let bad_src = "\\(x : Natural) -> x + True"
  let bad_ty = @moonbit-dhall.type_check_str(bad_src)
  assert_true(bad_ty is Err(_))
}
