type t<'a> = (int, Seq.t<'a>)

exception Err(string)

// let length = (slice: t<'a>) => {
// 	let (len, _) = slice
// 	len
// }
// let value = (slice: t<'a>) => {
// 	let (_, v) = slice
// 	v
// }

let car = slice => {
  let (_, seq) = slice
  switch seq {
  | Seq.Nil => raise(Err("unexpected slice, which is Nil."))
  | Seq.Cons(c) => Seq.car(c)
  }
}

let cdr = slice => {
  let (len, seq) = slice
  if len == 0 {
    raise(Err("unexpected slice, which is 0 length."))
  } else {
    switch seq {
    | Seq.Nil => raise(Err("unexpected slice, which is Nil."))
    | Seq.Cons(c) => (len - 1, Seq.cdr(c))
    }
  }
}

let toSeq = slice => {
  let (len, seq) = slice
  Seq.take(seq, len)
}

let toLazySeq = slice => {
  let (len, seq) = slice
  Seq.lazyTake(seq, len)
}

let seqSplitUnsafe = (seq, n) => {
  ((n, seq), Seq.drop(seq, n))
}

let seqSplitWhile = (seq, test) => {
  let (len, rest) = Seq.dropWhile(seq, test)
  ((len, seq), rest)
}
