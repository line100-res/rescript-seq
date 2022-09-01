open Test
open TestUtils

exception Err(string)

let unwrap = (opt, err) => {
  switch opt {
  | Some(v) => v
  | None => raise(err)
  }
}

// create a seq from string
let createCharSeq = str => {
  let rec gen = index => {
    switch Js.String2.charAt(str, index) {
    | "" => Some(Seq.Nil)
    | c => Some(Seq.make(c, ~thunk=() => gen(index + 1)))
    }
  }
  unwrap(gen(0), Err("createCharSeq: unexpected"))
}

test("test seq of 'abc'", () => {
  let seq = createCharSeq("abc")

  switch seq {
  | Seq.Nil => fail(~message="first is nil", ())
  | Seq.Cons(c) => {
      let first = Seq.car(c)
      let seq = Seq.cdr(c)
      stringEqual(~message="first is a", first, "a")

      switch seq {
      | None | Some(Seq.Nil) => fail(~message="second is nil", ())
      | Some(Seq.Cons(c)) => {
          let second = Seq.car(c)
          let seq = Seq.cdr(c)
          stringEqual(~message="second is b", second, "b")

          switch seq {
          | None | Some(Seq.Nil) => fail(~message="third is nil", ())
          | Some(Seq.Cons(c)) => {
              let second = Seq.car(c)
              let seq = Seq.cdr(c)
              stringEqual(~message="third is c", second, "c")
              assertion(~message="seq should end", (a, b) => a == b, seq, Some(Seq.Nil))
            }
          }
        }
      }
    }
  }
})

test("test seq match", () => {
  let seq = createCharSeq("abc")
  seqEqual(~message="seq match should be 3", seq, list{"a", "b", "c"})

  let arr = Seq.fromArrayInPlace(["a", "b", "c"])
  seqEqual(~message="array inplace seq match should be 3", arr, list{"a", "b", "c"})

  let arr = Seq.fromArray(["a", "b", "c"])
  seqEqual(~message="array seq match should be 3", arr, list{"a", "b", "c"})
})

// test("test takeListCount", () => {
//   let seq = [1, 2, 3, 4, 5] -> Seq.fromArray
//   let (count, l) = Seq.takeListCount(seq, 3)
//   intEqual(~message="count is 3", count, 3)
//   seqEqual(~message="seq is 1 2 3", l -> Seq.fromList, list{1, 2, 3})

//   let (count, l) = Seq.takeListCount(seq, 10)
//   intEqual(~message="count is 5", count, 5)
//   seqEqual(~message="seq is 1 2 3 4 5", l -> Seq.fromList, list{1, 2, 3, 4, 5})
// })

test("infinit seq", () => {
  let rec createInfinitSeq = (i) =>
    Seq.cons(i, Seq.Lazy(() => Some(createInfinitSeq(i + 1))))

  let seq = createInfinitSeq(0)
  let first3 = Seq.take(seq, 3)

  seqEqual(~message="get first 3 from infinit list", first3, list{0, 1, 2})
})