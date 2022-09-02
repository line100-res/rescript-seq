open Seq_Core

type t<'a> = (int, Seq_Core.t<'a>)

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
  | Nil => raise(Err("unexpected slice, which is Nil."))
  | Cons(c) => car(c)
  }
}

let cdr = slice => {
  let (len, seq) = slice
  if len == 0 {
    raise(Err("unexpected slice, which is 0 length."))
  } else {
    switch seq {
    | Nil => raise(Err("unexpected slice, which is Nil."))
    | Cons(c) => (len - 1, cdr(c))
    }
  }
}

let toSeq = slice => {
  let (len, seq) = slice
  take(seq, len)
}

let toLazySeq = slice => {
  let (len, seq) = slice
  lazyTake(seq, len)
}

let seqSplitUnsafe = (seq, n) => {
  ((n, seq), drop(seq, n))
}

let seqSplitWhile = (seq, test) => {
  let (len, rest) = dropWhile(seq, test)
  ((len, seq), rest)
}
