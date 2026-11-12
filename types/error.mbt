///|
/// Type errors encountered during type inference and checking
#derive(Eq, Debug)
pub(all) suberror TypeError {
  Message(String)
}

///|
pub impl Show for TypeError with fn output(self, logger) {
  match self {
    Message(msg) => logger.write_string(msg)
  }
}
