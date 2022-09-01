# rescript-seq

A simple `Lazy Sequence` implimation.

## Installation

```sh
npm install @line100/rescript-seq
```

## Usage

```rescript
let rec createInfinitSeq = (i) =>
    Seq.cons(i, Seq.Lazy(() => Some(createInfinitSeq(i + 1))))

let seq = createInfinitSeq(0)
let first3 = Seq.take(seq, 3) // 0 -> 1 -> 2
```


## API

### Creation Operators
- [x] fromList: `(list<'a>) => t<'a>`
- [x] fromArray: `(Array<'a>) => t<'a>`
- [x] fromArrayInPlace: `(Array<'a>) => t<'a>`
- [x] fromString: `(string) => t<string>`

### Conditional Operator
- [x] isEmpty: `(t<'a>) => bool`
- [x] equal: `(t<'a>, t<'a>) => bool`
- [x] match: `(t<'a>, list<'a>) => bool`
- [x] every: `(t<'a>, 'a => bool) = bool`

### Transformation Operator
- [x] map: `(t<'a>, 'a => 'b) => t<'b>`
- [x] append: `(t<'a>, t<'a>) => t<'a>`

### filtering operators
- take: `(t<'a>, int) => t<'a>`
- takeLazy: `(t<'a>, int) => t<'a>`
- takeWhile: `(t<'a>, 'a => bool) => t<'a>`
- drop: `(t<'a>, int) => t<'a>`
- dropWhile: `(t<'a>, 'a => bool) => t<'a>`