# rescript-seq

A simple `Lazy Sequence` implimation.

## Installation

```sh
npm install @line100/rescript-seq
```

## Usage

```rescript
let rec createInfinitSeq = (i) =>
    Seq.cons(i, Seq.Lazy(() => createInfinitSeq(i + 1)))

let seq = createInfinitSeq(0)
let first3 = Seq.take(seq, 3)
```


## Build

- Build: `npm run build`
- Clean: `npm run clean`
- Build & watch: `npm run start`
- Build & test: `npm run test`