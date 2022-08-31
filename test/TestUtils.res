open Test

let identity = (a, b) => a == b

let intEqual = (~message=?, a: int, b: int) =>
  assertion(~message?, ~operator="intEqual", identity, a, b)

let stringEqual = (~message=?, a: string, b: string) =>
  assertion(~message?, ~operator="stringEqual", identity, a, b)

let seqEqual = (~message=?, seq: Seq.t<'a>, lst: list<'a>) => {
  let len1 = List.length(lst)
  let mathcer = lst->Seq.fromList
  let ret = Seq.match(seq, mathcer)
  let (len0, isForced) = Seq.countForced(seq)
  assertion(~message="[seq is forced]", ~operator="seqEqual", identity, isForced, true)
  assertion(~message="[length is equal]", ~operator="seqEqual", identity, len0, len1)
  assertion(~message?, ~operator="seqEqual", identity, ret, len1)
}

let arrayEqual = (~message=?, a, b, comp) => {
  assertion(~message?, ~operator="arrayEqual", (a, b) => Belt.Array.eq(a, b, comp), a, b)
}
