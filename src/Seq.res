type rec t<'a> =
  | Nil
  | Cons(cons<'a>)
and cons<'a> = {
  head: 'a,
  mutable tail: tail<'a>,
}
and tail<'a> =
  | Forced(t<'a>)
  | Lazy(unit => t<'a>)

exception Err(string)

let force = cons => {
  let {tail} = cons
  cons.tail = switch tail {
  | Forced(s) => Forced(s)
  | Lazy(thunk) => Forced(thunk())
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
  | Forced(s) => s
  | Lazy(_) => raise(Err("cdr force unexpected error."))
  }
}

let cons = (head, tail) => {
  Cons({head: head, tail: tail})
}

// create a lazy seq
let make = (~thunk=?, head) => {
  switch thunk {
  | Some(f) => cons(head, Lazy(f))
  | None => cons(head, Forced(Nil))
  }
}

let rec equal = (seq1, seq2) => {
  switch (seq1, seq2) {
  | (Nil, Nil) => true
  | (Cons(c1), Cons(c2)) =>
    if c1.head == c2.head {
      equal(cdr(c1), cdr(c2))
    } else {
      false
    }
  | (_, _) => false
  }
}

let rec map = (seq, f) => {
  switch seq {
  | Nil => Nil
  | Cons(c) => cons(f(c.head), Lazy(() => map(cdr(c), f)))
  }
}

let rec append = (seq, thunk) => {
  switch seq {
  | Nil => thunk()
  | Cons(c) => cons(c.head, Lazy(() => append(cdr(c), thunk)))
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

// take, takeWhile, lazyTake

let rec take = (seq, n) => {
  if n == 0 {
    Nil
  } else {
    switch seq {
    | Nil => Nil
    | Cons(c) => cons(c.head, Forced(take(cdr(c), n - 1)))
    }
  }
}

let rec lazyTake = (seq, n) => {
  if n == 0 {
    Nil
  } else {
    switch seq {
    | Nil => Nil
    | Cons(c) => cons(c.head, Lazy(() => lazyTake(cdr(c), n - 1)))
    }
  }
}

let takeWhile = (seq, test) => {
  let count = ref(0)
  let rec loop = input => {
    switch input {
    | Nil => Nil
    | Cons(c) =>
      if test(c.head) {
        count := count.contents + 1
        cons(c.head, Forced(loop(cdr(c))))
      } else {
        Nil
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
    | Cons(c) => drop(cdr(c), n - 1)
    }
  }
}

let dropWhile = (seq, test) => {
  let rec loop = (count, input) => {
    switch input {
    | Nil => (0, Nil)
    | Cons(c) =>
      if test(c.head) {
        loop(count + 1, cdr(c))
      } else {
        (count, input)
      }
    }
  }
  loop(0, seq)
}

// reduce, reduceWhile

let rec reduce = (seq, n, f, acc) => {
  if n == 0 {
    (acc, seq)
  } else {
    switch seq {
    | Nil => (acc, seq)
    | Cons(c) => reduce(cdr(c), n - 1, f, f(acc, c.head))
    }
  }
}

let reduceWhile = (seq, test, f, acc) => {
  let rec loop = (input, acc, index) => {
    switch input {
    | Nil => (acc, index, input)
    | Cons(c) =>
      if test(c.head) {
        loop(cdr(c), f(acc, c.head), index + 1)
      } else {
        (acc, index, input)
      }
    }
  }
  loop(seq, acc, 0)
}

let reduceWhile2 = (seq, f, acc) => {
  let rec loop = (input, acc, index) => {
    switch input {
    | Nil => (acc, index, input)
    | Cons(c) =>
      switch f(acc, c.head, index) {
      | Some(acc) => loop(cdr(c), acc, index + 1)
      | None => (acc, index, input)
      }
    }
  }
  loop(seq, acc, 0)
}

// match

let match = (~equal=?, seq, lst) => {
  let eq = switch equal {
  | None => (a, b) => a == b
  | Some(f) => f
  }
  let rec loop = (index, input, lst) => {
    switch lst {
    | Nil => index
    | Cons(c1) =>
      switch input {
      | Nil => index
      | Cons(c0) =>
        if eq(c0.head, c1.head) {
          loop(index + 1, cdr(c0), cdr(c1))
        } else {
          index
        }
      }
    }
  }
  loop(0, seq, lst)
}

// takeListCount

let takeListCount = (seq, n) => {
  let count = ref(0)
  let rec loop = (seq, n, got) => {
    if n == 0 {
      count := got
      list{}
    } else {
      switch seq {
      | Nil => {
        count := got
        list{}
      }
      | Cons(c) => list{c.head, ...loop(cdr(c), n - 1, got + 1)}
      }
    }
  }
  let l = loop(seq, n, 0)
  (count.contents, l)
}

// fromList, fromArray

let rec fromList = lst => {
  switch lst {
  | list{} => Nil
  | list{a} => cons(a, Forced(Nil))
  | list{a, ...rest} => cons(a, Lazy(() => fromList(rest)))
  }
}

let fromArrayInPlace = (arr: array<'a>) => {
  let rec gen = () => {
    switch arr {
    | [] => Nil
    | [a] => cons(a, Forced(Nil))
    | _ => {
      switch Js.Array2.shift(arr) {
      | Some(a) => cons(a, Lazy(gen))
      | _ => raise(Err("unreachable"))
      }
    }
    }
  }
  gen()
}

let fromArray= (arr: array<'a>) => {
  let rec gen = index => {
    switch Belt.Array.get(arr, index) {
    | None => Nil
    | Some(a) => {
        cons(a, Lazy(() => gen(index + 1)))
      }
    }
  }
  gen(0)
}

let fromString = str => {
  let rec gen = index => {
    switch Js.String2.charAt(str, index) {
    | "" => Nil
    | c => cons(c, Lazy(() => gen(index + 1)))
    }
  }
  gen(0)
}