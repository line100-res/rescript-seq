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

test("test seq append", () => {
  let s1 = Seq.fromArray([1, 2])
  let s2 = Seq.fromArray([3])
  seqEqual(~message="seq 1 2 + seq 3, is seq 1 2 3", Seq.append(s1, () => s2), list{1, 2, 3})
})

test("test seq merge", () => {
  let s1 = Seq.fromArray([1, 2])
  let s2 = Seq.fromArray([3])
  seqEqual(~message="seq 1 2 merge seq 3, is seq 1 3 2", Seq.merge(s1, s2), list{1, 3, 2})

  let s1 = Seq.fromArray([1])
  let s2 = Seq.fromArray([2, 3])
  seqEqual(~message="seq 1 merge seq 2 3, is seq 1 2 3", Seq.merge(s1, s2), list{1, 2, 3})

  let s1 = Seq.fromArray([1])
  let s2 = Seq.fromArray([2, 3, 4])
  seqEqual(~message="seq 1 merge seq 2 3 4, is seq 1 2 3 4", Seq.merge(s1, s2), list{1, 2, 3, 4})

  let s1 = Seq.fromArray([1, 2, 3])
  let s2 = Seq.fromArray([4])
  seqEqual(~message="seq 1 2 3 merge seq 4, is seq 1 2 3 4", Seq.merge(s1, s2), list{1, 4, 2, 3})
})

let rec createInfinitSeq = (i) =>
  Seq.cons(i, Seq.Lazy(() => Some(createInfinitSeq(i + 1))))

test("infinit seq take 3", () => {
  let seq = createInfinitSeq(0)
  let first3 = Seq.take(seq, 3)
  seqEqual(~message="take first 3 from infinit list", first3, list{0, 1, 2})

  let first3 = Seq.lazyTake(seq, 3)
  seqEqual(~message="lazyTake first 3 from infinit list", first3, list{0, 1, 2})

  let first3 = Seq.takeWhile(seq, v => v < 3)
  seqEqual(~message="takeWhile v < 3 from infinit list", first3, list{0, 1, 2})

  let first3 = Seq.lazyTakeWhile(seq, v => v < 3)
  seqEqual(~message="lazyTakeWhile v < 3 from infinit list", first3, list{0, 1, 2})
})

test("infinit seq map + 1", () => {
  let seq = createInfinitSeq(0)
  let first3 = seq->Seq.take(3)->Seq.map(v => v + 1)
  seqEqual(~message="map v + 1 after take first 3 from infinit list", first3, list{1, 2, 3})

  let first3 = seq->Seq.lazyMap(v => v + 1)->Seq.take(3)
  seqEqual(~message="lazyMap v + 1 before take first 3 from infinit list", first3, list{1, 2, 3})
})