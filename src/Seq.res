type rec t<'a> =
  | Nil
  | Cons(cons<'a>)
and cons<'a> = {
  head: 'a,
  mutable tail: tail<'a>,
}
and tail<'a> =
  | Forced(t<'a>)
  | Lazy(unit => option<t<'a>>)

exception Err(string)

let unwrap = (opt) => {
  switch opt {
  | Some(v) => v
  | None => raise(Err("unwrap None error."))
  }
}

let force = cons => {
  cons.tail = switch cons.tail {
  | Forced(s) => Forced(s)
  | Lazy(thunk) => {
      let ret = thunk()
      switch ret {
      | None => Lazy(thunk)
      | Some(v) => Forced(v)
      }
    }
  }
  cons
}

let car = cons => {
  let {head} = cons
  head
}

let cdr = cons => {
  let {tail} = force(cons)
  switch tail {
  | Forced(s) => Some(s)
  | Lazy(_) => None
  }
}
let next = cdr

let cons = (head, tail) => {
  Cons({head: head, tail: tail})
}

let consWait = (head, wait, then) => {
  cons(
    head,
    Lazy(
      () => {
        switch wait() {
        | Some(v) => Some(then(v))
        | None => None
        }
      },
    ),
  )
}


// create a lazy seq
let make = (~thunk=?, head) => {
  switch thunk {
  | Some(f) => cons(head, Lazy(f))
  | None => cons(head, Forced(Nil))
  }
}

// conditional operator
// isEmpty, equal, match, every

let isEmpty = lst => {
  switch lst {
  | Nil => true
  | _ => false
  }
}

let equal = (~equalf=?, seq1, seq2) => {
  let eq = switch equalf {
  | None => (a, b) => a == b
  | Some(f) => f
  }
  let rec loop = (seq1, seq2) =>
    switch (seq1, seq2) {
    | (Nil, Nil) => true
    | (Cons(c1), Cons(c2)) =>
      if eq(c1.head, c2.head) {
        switch (cdr(c1), cdr(c2)) {
        | (None, _) | (_, None) => false
        | (Some(t1), Some(t2)) => loop(t1, t2)
        }
      } else {
        false
      }
    | (_, _) => false
    }
  loop(seq1, seq2)
}

let match = (~equal=?, seq, lst) => {
  let eq = switch equal {
  | None => (a, b) => a == b
  | Some(f) => f
  }
  let rec loop = (index, s, l) => {
    switch (s, l) {
    | (Nil, _) | (_, list{}) => index
    | (Cons(sc), list{lh, ...lt}) => if eq(sc.head, lh) {
        switch cdr(sc) {
        | Some(t) => loop(index + 1, t, lt)
        | None => index
        }
      } else {
        index
      }
    }
  }
  loop(0, seq, lst)
}

let rec every = (seq, test) => {
  switch seq {
  | Nil => false
  | Cons(c) => if test(c.head) {
      switch cdr(c) {
      | None => false
      | Some(t) => every(t, test)
      }
    } else {
      false
    }
  }
}

// transformation operator
// map, append

let rec map = (seq, f) => {
  switch seq {
  | Nil => Nil
  | Cons(c) =>
    cons(
      f(c.head),
      Lazy(
        () => {
          switch cdr(c) {
          | Some(t) => Some(map(t, f))
          | None => None
          }
        },
      ),
    )
  }
}

let rec append = (seq, thunk) => {
  switch seq {
  | Nil => thunk()
  | Cons(c) =>
    cons(
      c.head,
      Lazy(
        () => {
          switch cdr(c) {
          | Some(t) => Some(append(t, thunk))
          | None => None
          }
        },
      ),
    )
  }
}

let countForced = seq => {
  let rec loop = (index, s) => {
    switch s {
    | Nil => (index, true)
    | Cons({tail}) =>
      switch tail {
      | Lazy(_) => (index + 1, false)
      | Forced(tail) => loop(index + 1, tail)
      }
    }
  }
  loop(0, seq)
}

// filtering operators
// take, takeWhile, lazyTake

let rec take = (seq: t<'a>, n) => {
  if n == 0 {
    Nil
  } else {
    switch seq {
    | Nil => Nil
    | Cons(c) =>
      switch force(c) {
      | {head, tail: Forced(v)} => cons(head, Forced(take(v, n - 1)))
      | {head, tail: Lazy(wait)} => consWait(head, wait, v => take(v, n - 1))
      }
    }
  }
}

let rec lazyTake = (seq, n) => {
  if n == 0 {
    Nil
  } else {
    switch seq {
    | Nil => Nil
    | Cons(c) =>
      switch force(c) {
      | {head, tail: Forced(v)} => cons(head, Lazy(() => Some(lazyTake(v, n - 1))))
      | {head, tail: Lazy(wait)} => consWait(head, wait, v => lazyTake(v, n - 1))
      }
    }
  }
}

// FIXME: 拆分takeWhileCount和takeWhile
let takeWhile = (seq, test) => {
  let count = ref(0)
  let rec loop = input => {
    switch input {
    | Nil => Nil
    | Cons(c) =>
      switch force(c) {
      | {head, tail: Forced(v)} =>
        if test(head) {
          count := count.contents + 1
          cons(head, Forced(loop(v)))
        } else {
          Nil
        }
      | {tail: Lazy(_)} => {
          count := count.contents + 1
          Nil
        }
      }
    }
  }
  let l = loop(seq)
  (count.contents, l)
}

// drop, dropWhile

let rec drop = (seq, n) => {
  if n == 0 {
    seq
  } else {
    switch seq {
    | Nil => Nil
    | Cons(c) =>
      switch force(c) {
      | {tail: Forced(v)} => drop(v, n - 1)
      | wait => Cons(wait)
      }
    }
  }
}

let dropWhile = (seq, test) => {
  let rec loop = (count, input) => {
    switch input {
    | Nil => (count, Nil)
    | Cons(c) =>
      switch force(c) {
      | {head, tail: Forced(v)} =>
        if test(head) {
          loop(count + 1, v)
        } else {
          (count, v)
        }
      | wait => (count, Cons(wait))
      }
    }
  }
  loop(0, seq)
}

// reduce, reduceWhile

// let rec reduce = (seq, n, f, acc) => {
//   if n == 0 {
//     (acc, seq)
//   } else {
//     switch seq {
//     | Nil => (acc, seq)
//     | Cons(c) => reduce(cdr(c), n - 1, f, f(acc, c.head))
//     }
//   }
// }

// let reduceWhile = (seq, test, f, acc) => {
//   let rec loop = (input, acc, index) => {
//     switch input {
//     | Nil => (acc, index, input)
//     | Cons(c) =>
//       if test(c.head) {
//         loop(cdr(c), f(acc, c.head), index + 1)
//       } else {
//         (acc, index, input)
//       }
//     }
//   }
//   loop(seq, acc, 0)
// }

// let reduceWhile2 = (seq, f, acc) => {
//   let rec loop = (input, acc, index) => {
//     switch input {
//     | Nil => (acc, index, input)
//     | Cons(c) =>
//       switch f(acc, c.head, index) {
//       | Some(acc) => loop(cdr(c), acc, index + 1)
//       | None => (acc, index, input)
//       }
//     }
//   }
//   loop(seq, acc, 0)
// }

// switch lst {
// | Nil => index
// | Cons(c1) =>
//   switch input {
//   | Nil => index
//   | Cons(c0) =>
//     if eq(c0.head, c1.head) {
//       loop(index + 1, cdr(c0), cdr(c1))
//     } else {
//       index
//     }
//   }
// }
//   }
//   loop(0, seq, lst)
// }

// takeListCount

// let takeListCount = (seq, n) => {
//   let count = ref(0)
//   let rec loop = (seq, n, got) => {
//     if n == 0 {
//       count := got
//       list{}
//     } else {
//       switch seq {
//       | Nil => {
//           count := got
//           list{}
//         }
//       | Cons(c) => list{c.head, ...loop(cdr(c), n - 1, got + 1)}
//       }
//     }
//   }
//   let l = loop(seq, n, 0)
//   (count.contents, l)
// }

// creation operators
// fromList, fromArray, fromArrayInPlace, fromString

let rec fromList = lst => {
  switch lst {
  | list{} => Nil
  | list{a} => cons(a, Forced(Nil))
  | list{a, ...rest} => cons(a, Lazy(() => Some(fromList(rest))))
  }
}

let fromArrayInPlace = (arr: array<'a>) => {
  let rec gen = () => {
    switch arr {
    | [] => Some(Nil)
    | [a] => Some(cons(a, Forced(Nil)))
    | _ =>
      switch Js.Array2.shift(arr) {
      | Some(a) => Some(cons(a, Lazy(gen)))
      | _ => raise(Err("unreachable"))
      }
    }
  }
  switch gen() {
  | Some(s) => s
  | None => raise(Err("unreachable"))
  }
}

let fromArray = (arr: array<'a>) => {
  let rec gen = index => {
    switch Belt.Array.get(arr, index) {
    | None => Some(Nil)
    | Some(a) => Some(cons(a, Lazy(() => gen(index + 1))))
    }
  }
  switch gen(0) {
  | Some(s) => s
  | None => raise(Err("unreachable"))
  }
}

let fromString = str => {
  let rec gen = index => {
    switch Js.String2.charAt(str, index) {
    | "" => Some(Nil)
    | c => Some(cons(c, Lazy(() => gen(index + 1))))
    }
  }
  switch gen(0) {
  | Some(s) => s
  | None => raise(Err("unreachable"))
  }
}
