// package dwayne.org




// val x = List(
//   Asterisk(a => levelLense(a.length)),
//   Space(),
//   Optional(
//     Space(),
//     Keyword(k => keywordLense(k))
//   ),
//   Space(),
//   Title(t => titleLense(t)),
//   Optional(
//     Char(":"),
//     RepeatedWithDelim(
//       Tag(t => tagLense(t)),
//       Char(:)
//       )
//     Char(":"),
//     )
//   )
