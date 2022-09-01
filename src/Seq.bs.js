// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Caml_exceptions = require("rescript/lib/js/caml_exceptions.js");

var Err = /* @__PURE__ */Caml_exceptions.create("Seq.Err");

function unwrap(opt) {
  if (opt !== undefined) {
    return Caml_option.valFromOption(opt);
  }
  throw {
        RE_EXN_ID: Err,
        _1: "unwrap None error.",
        Error: new Error()
      };
}

function force(cons) {
  var s = cons.tail;
  var tmp;
  if (s.TAG === /* Forced */0) {
    tmp = {
      TAG: /* Forced */0,
      _0: s._0
    };
  } else {
    var thunk = s._0;
    var ret = Curry._1(thunk, undefined);
    tmp = ret !== undefined ? ({
          TAG: /* Forced */0,
          _0: ret
        }) : ({
          TAG: /* Lazy */1,
          _0: thunk
        });
  }
  cons.tail = tmp;
  return cons;
}

function car(cons) {
  return cons.head;
}

function cdr(cons) {
  var match = force(cons);
  var tail = match.tail;
  if (tail.TAG === /* Forced */0) {
    return tail._0;
  }
  
}

function cons(head, tail) {
  return /* Cons */{
          _0: {
            head: head,
            tail: tail
          }
        };
}

function consWait(head, wait, $$then) {
  return /* Cons */{
          _0: {
            head: head,
            tail: {
              TAG: /* Lazy */1,
              _0: (function (param) {
                  var v = Curry._1(wait, undefined);
                  if (v !== undefined) {
                    return Curry._1($$then, Caml_option.valFromOption(v));
                  }
                  
                })
            }
          }
        };
}

function consMapTail(c, f) {
  var match = force(c);
  var head = match.head;
  var t = match.tail;
  if (t.TAG !== /* Forced */0) {
    return consWait(head, t._0, f);
  }
  var tail = {
    TAG: /* Forced */0,
    _0: Curry._1(f, t._0)
  };
  return /* Cons */{
          _0: {
            head: head,
            tail: tail
          }
        };
}

function lazyConsMapTail(c, f) {
  var match = force(c);
  var head = match.head;
  var t = match.tail;
  if (t.TAG !== /* Forced */0) {
    return consWait(head, t._0, f);
  }
  var t$1 = t._0;
  return /* Cons */{
          _0: {
            head: head,
            tail: {
              TAG: /* Lazy */1,
              _0: (function (param) {
                  return Curry._1(f, t$1);
                })
            }
          }
        };
}

function consMap(c, fh, ft) {
  var match = force(c);
  var head = match.head;
  var t = match.tail;
  if (t.TAG !== /* Forced */0) {
    return consWait(Curry._1(fh, head), t._0, ft);
  }
  var tail = {
    TAG: /* Forced */0,
    _0: Curry._1(ft, t._0)
  };
  var head$1 = Curry._1(fh, head);
  return /* Cons */{
          _0: {
            head: head$1,
            tail: tail
          }
        };
}

function lazyConsMap(c, fh, ft) {
  var match = force(c);
  var head = match.head;
  var t = match.tail;
  if (t.TAG !== /* Forced */0) {
    return consWait(Curry._1(fh, head), t._0, ft);
  }
  var t$1 = t._0;
  var head$1 = Curry._1(fh, head);
  return /* Cons */{
          _0: {
            head: head$1,
            tail: {
              TAG: /* Lazy */1,
              _0: (function (param) {
                  return Curry._1(ft, t$1);
                })
            }
          }
        };
}

function make(thunk, head) {
  if (thunk !== undefined) {
    return /* Cons */{
            _0: {
              head: head,
              tail: {
                TAG: /* Lazy */1,
                _0: thunk
              }
            }
          };
  } else {
    return /* Cons */{
            _0: {
              head: head,
              tail: {
                TAG: /* Forced */0,
                _0: /* Nil */0
              }
            }
          };
  }
}

function isEmpty(lst) {
  if (lst) {
    return false;
  } else {
    return true;
  }
}

function equal(equalf, seq1, seq2) {
  var eq = equalf !== undefined ? equalf : Caml_obj.caml_equal;
  var _seq1 = seq1;
  var _seq2 = seq2;
  while(true) {
    var seq2$1 = _seq2;
    var seq1$1 = _seq1;
    if (!seq1$1) {
      if (seq2$1) {
        return false;
      } else {
        return true;
      }
    }
    if (!seq2$1) {
      return false;
    }
    var c2 = seq2$1._0;
    var c1 = seq1$1._0;
    if (!Curry._2(eq, c1.head, c2.head)) {
      return false;
    }
    var match = cdr(c1);
    var match$1 = cdr(c2);
    if (match === undefined) {
      return false;
    }
    if (match$1 === undefined) {
      return false;
    }
    _seq2 = match$1;
    _seq1 = match;
    continue ;
  };
}

function match(equal, seq, lst) {
  var eq = equal !== undefined ? equal : Caml_obj.caml_equal;
  var _index = 0;
  var _s = seq;
  var _l = lst;
  while(true) {
    var l = _l;
    var s = _s;
    var index = _index;
    if (!s) {
      return index;
    }
    if (!l) {
      return index;
    }
    var sc = s._0;
    if (!Curry._2(eq, sc.head, l.hd)) {
      return index;
    }
    var t = cdr(sc);
    if (t === undefined) {
      return index;
    }
    _l = l.tl;
    _s = t;
    _index = index + 1 | 0;
    continue ;
  };
}

function every(_seq, test) {
  while(true) {
    var seq = _seq;
    if (!seq) {
      return false;
    }
    var c = seq._0;
    if (!Curry._1(test, c.head)) {
      return false;
    }
    var t = cdr(c);
    if (t === undefined) {
      return false;
    }
    _seq = t;
    continue ;
  };
}

function map(seq, f) {
  if (seq) {
    return consMap(seq._0, f, (function (t) {
                  return map(t, f);
                }));
  } else {
    return /* Nil */0;
  }
}

function lazyMap(seq, f) {
  if (seq) {
    return lazyConsMap(seq._0, f, (function (t) {
                  return lazyMap(t, f);
                }));
  } else {
    return /* Nil */0;
  }
}

function append(seq, thunk) {
  if (!seq) {
    return Curry._1(thunk, undefined);
  }
  var c = seq._0;
  return /* Cons */{
          _0: {
            head: c.head,
            tail: {
              TAG: /* Lazy */1,
              _0: (function (param) {
                  var t = cdr(c);
                  if (t !== undefined) {
                    return append(t, thunk);
                  }
                  
                })
            }
          }
        };
}

function countForced(seq) {
  var _index = 0;
  var _s = seq;
  while(true) {
    var s = _s;
    var index = _index;
    if (!s) {
      return [
              index,
              true
            ];
    }
    var tail = s._0.tail;
    if (tail.TAG !== /* Forced */0) {
      return [
              index + 1 | 0,
              false
            ];
    }
    _s = tail._0;
    _index = index + 1 | 0;
    continue ;
  };
}

function take(seq, n) {
  if (n === 0 || !seq) {
    return /* Nil */0;
  } else {
    return consMapTail(seq._0, (function (v) {
                  return take(v, n - 1 | 0);
                }));
  }
}

function lazyTake(seq, n) {
  if (n === 0 || !seq) {
    return /* Nil */0;
  } else {
    return lazyConsMapTail(seq._0, (function (v) {
                  return lazyTake(v, n - 1 | 0);
                }));
  }
}

function takeWhile(seq, test) {
  if (!seq) {
    return /* Nil */0;
  }
  var c = seq._0;
  if (Curry._1(test, c.head)) {
    return consMapTail(c, (function (v) {
                  return takeWhile(v, test);
                }));
  } else {
    return /* Nil */0;
  }
}

function lazyTakeWhile(seq, test) {
  if (!seq) {
    return /* Nil */0;
  }
  var c = seq._0;
  if (Curry._1(test, c.head)) {
    return lazyConsMapTail(c, (function (v) {
                  return lazyTakeWhile(v, test);
                }));
  } else {
    return /* Nil */0;
  }
}

function drop(_seq, _n) {
  while(true) {
    var n = _n;
    var seq = _seq;
    if (n === 0) {
      return seq;
    }
    if (!seq) {
      return /* Nil */0;
    }
    var wait = force(seq._0);
    var v = wait.tail;
    if (v.TAG !== /* Forced */0) {
      return /* Cons */{
              _0: wait
            };
    }
    _n = n - 1 | 0;
    _seq = v._0;
    continue ;
  };
}

function dropWhile(seq, test) {
  var _count = 0;
  var _input = seq;
  while(true) {
    var input = _input;
    var count = _count;
    if (!input) {
      return [
              count,
              /* Nil */0
            ];
    }
    var wait = force(input._0);
    var v = wait.tail;
    if (v.TAG !== /* Forced */0) {
      return [
              count,
              /* Cons */{
                _0: wait
              }
            ];
    }
    var v$1 = v._0;
    if (!Curry._1(test, wait.head)) {
      return [
              count,
              v$1
            ];
    }
    _input = v$1;
    _count = count + 1 | 0;
    continue ;
  };
}

function fromList(lst) {
  if (!lst) {
    return /* Nil */0;
  }
  var rest = lst.tl;
  var a = lst.hd;
  if (rest) {
    return /* Cons */{
            _0: {
              head: a,
              tail: {
                TAG: /* Lazy */1,
                _0: (function (param) {
                    return fromList(rest);
                  })
              }
            }
          };
  } else {
    return /* Cons */{
            _0: {
              head: a,
              tail: {
                TAG: /* Forced */0,
                _0: /* Nil */0
              }
            }
          };
  }
}

function fromArrayInPlace(arr) {
  var gen = function (param) {
    var len = arr.length;
    if (len !== 1) {
      if (len === 0) {
        return /* Nil */0;
      }
      var a = arr.shift();
      if (a !== undefined) {
        return /* Cons */{
                _0: {
                  head: a,
                  tail: {
                    TAG: /* Lazy */1,
                    _0: gen
                  }
                }
              };
      }
      throw {
            RE_EXN_ID: Err,
            _1: "unreachable",
            Error: new Error()
          };
    }
    var a$1 = arr[0];
    return /* Cons */{
            _0: {
              head: a$1,
              tail: {
                TAG: /* Forced */0,
                _0: /* Nil */0
              }
            }
          };
  };
  return unwrap(gen(undefined));
}

function fromArray(arr) {
  var gen = function (index) {
    var a = Belt_Array.get(arr, index);
    if (a !== undefined) {
      return /* Cons */{
              _0: {
                head: Caml_option.valFromOption(a),
                tail: {
                  TAG: /* Lazy */1,
                  _0: (function (param) {
                      return gen(index + 1 | 0);
                    })
                }
              }
            };
    } else {
      return /* Nil */0;
    }
  };
  return unwrap(gen(0));
}

function fromString(str) {
  var gen = function (index) {
    var c = str.charAt(index);
    if (c === "") {
      return /* Nil */0;
    } else {
      return /* Cons */{
              _0: {
                head: c,
                tail: {
                  TAG: /* Lazy */1,
                  _0: (function (param) {
                      return gen(index + 1 | 0);
                    })
                }
              }
            };
    }
  };
  return unwrap(gen(0));
}

var next = cdr;

exports.Err = Err;
exports.unwrap = unwrap;
exports.force = force;
exports.car = car;
exports.cdr = cdr;
exports.next = next;
exports.cons = cons;
exports.consWait = consWait;
exports.consMapTail = consMapTail;
exports.lazyConsMapTail = lazyConsMapTail;
exports.consMap = consMap;
exports.lazyConsMap = lazyConsMap;
exports.make = make;
exports.isEmpty = isEmpty;
exports.equal = equal;
exports.match = match;
exports.every = every;
exports.map = map;
exports.lazyMap = lazyMap;
exports.append = append;
exports.countForced = countForced;
exports.take = take;
exports.lazyTake = lazyTake;
exports.takeWhile = takeWhile;
exports.lazyTakeWhile = lazyTakeWhile;
exports.drop = drop;
exports.dropWhile = dropWhile;
exports.fromList = fromList;
exports.fromArrayInPlace = fromArrayInPlace;
exports.fromArray = fromArray;
exports.fromString = fromString;
/* No side effect */
