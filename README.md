# rescript-seq

A `Lazy Sequence` implimation. Highly abstracted stream like `RxJs` but without `time`.

```txt

  Deleted Head                        Infinit Tail

┌───┐   ┌───┐   ┌──┬┐   ┌──┬┐   ┌──┬┐
│   │   │   │   │ a│┼───► b│┼───► c│┼───►  ?
└───┘   └───┘   └──┴┘   └──┴┘   └──┴┘
                  ▲       ▲
                  │       │
                  │       │
          ┌─────┬─┘    ┌──┴──┐
          │ ref │      │ ref │
          └─────┘      └─────┘

```

## Installation

```sh
npm install @line100/rescript-seq
```

## Usage

```rescript
// infinit sequence of numbers
let rec createInfinitSeq = (i) =>
    Seq.cons(i, Seq.Lazy(() => Some(createInfinitSeq(i + 1))))

// take and lazyTake
let seq = createInfinitSeq(0)
let first3 = Seq.take(seq, 3)
seqEqual(~message="take first 3 from infinit list", first3, list{0, 1, 2})

let first3 = Seq.lazyTake(seq, 3)
seqEqual(~message="lazyTake first 3 from infinit list", first3, list{0, 1, 2})


// map and lazyMap
let first3 = seq->Seq.take(3)->Seq.map(v => v + 1)
seqEqual(~message="map v + 1 after take first 3 from infinit list", first3, list{1, 2, 3})

let first3 = seq->Seq.lazyMap(v => v + 1)->Seq.take(3)
seqEqual(~message="lazyMap v + 1 before take first 3 from infinit list", first3, list{1, 2, 3})
```

## API

### Creation Operators
- [x] fromList: `(list<'a>) => t<'a>`
- [x] fromArray: `(Array<'a>) => t<'a>`
- [x] fromArrayInPlace: `(Array<'a>) => t<'a>`
- [x] fromString: `(string) => t<string>`, convert `string` to `sequence of char`

### Conditional Operator
- [x] isEmpty: `(t<'a>) => bool`
- [x] equal: `(t<'a>, t<'a>) => bool`
- [x] match: `(t<'a>, list<'a>) => bool`
- [x] every: `(t<'a>, 'a => bool) = bool`

### Transformation Operator
- [x] map: `(t<'a>, 'a => 'b) => t<'b>`
- [x] append: `(t<'a>, t<'a>) => t<'a>`

- [x] lazyMap: `(t<'a>, 'a => 'b) => t<'b>`

### filtering operators
- [x] take: `(t<'a>, int) => t<'a>`
- [x] takeWhile: `(t<'a>, 'a => bool) => t<'a>`
- [x] drop: `(t<'a>, int) => t<'a>`
- [x] dropWhile: `(t<'a>, 'a => bool) => t<'a>`

- [x] lazyTake: `(t<'a>, int) => t<'a>`
- [x] lazyTakeWhile: `(t<'a>, 'a => bool) => t<'a>`