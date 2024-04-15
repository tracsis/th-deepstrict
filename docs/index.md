---
layout: default
---

# th-deepstrict

### Introduction

Excessive laziness is a common worry when analyzing the performance characteristics of Haskell programs. 
It can lead to higher than expected memory usage due to heap objects being retained for longer than necessary.
This is known as a "space leak". 
Like any other performance issue, it can be tricky to track down and fix, especially in large, complex codebases.

At [Tracsis](https://tracsis.com) we have written a library, `th-deepstrict`, to help avoid this class of problem by asserting the strictness of a datatype at compile time.

We will give a brief introduction to excessive laziness; survey existing tools; and then present `th-deepstrict`.

### Excessive Laziness

At run-time, laziness is represented using a type of heap object called a "thunk". 
Thunks are suspended computations. Often they will require some data to be executed. 
This data will be kept alive until the thunk is run or the thunk is garbage collected. 
For instance to compute `x + y`, the values `x` and `y` must be retained in memory.
When thunks are evaluated, they are replaced with the computed value, 
and the computation's retained data can be freed. 

Most of the time thunks are quite low-cost. They trade a little cost in memory for saving
the cost of computing a value in the case where the thunk is never evaluated.

Problems only start to appear when thunks retain large structures, or long chains/trees of thunks are built up.
These cases can lead to excessive memory usage.

### Existing tools
Haskell programmers have a variety of tools at their disposal to help locate these space leaks 
once they occur. For instance: 
[info table profiling](https://well-typed.com/blog/2021/01/first-look-at-hi-profiling-mode/), [ghc-debug](https://ghc.gitlab.haskell.org/ghc-debug/) or [profiling for large stacks](https://neilmitchell.blogspot.com/2015/09/detecting-space-leaks.html)

Yet, often we want to catch these issues before they are noticed in a running application.
One tool at our disposal is convention. For instance, seasoned Haskell programmers know to prefer `foldl'`; to not rely on lazy tuples; to use streaming libraries instead of lazy lists/IO; etc. Knowledge of the sorts of things that can lead to space leaks is invaluable, but it places sole responsibility on the programmer.
Even the best programmers make mistakes, and only understand a fraction of a complex codebase.

Another helpful tool is [`nothunks`](https://hackage.haskell.org/package/nothunks), which is a library that allows writing unit tests to assert that particular structures don't contain thunks at run-time. This is very helpful for testing things like datastructures that have a relatively small amount of states. 
In that instance, you can be pretty sure that your unit tests are covering all of the cases, and that you won't have excessive thunks at run-time. 
On the other hand, large datatypes representing complex domains are often less easy to test this way. They might have a much larger state space, which would be difficult to cover exhaustively with unit tests.

### `th-deepstrict`

Often we know what datatypes are at high risk of space leaks. These are usually complex, and retained for a long period of time (usually as part of the program's state).
For types like these we want to make very strong guarantees that they will not contain excessive thunks. We want to show that they won't contain thunks at all or only in very specific fields. `th-deepstrict` allows us to assert properties like this at compile-time using Template Haskell.

We say that a type is strict if evaluating that type means no fields are thunks. A type is then *deep* strict if it is strict and the types of all of its fields are deep strict.
In other words, a type is deep strict if evaluating it means that it does not contain any thunks (even transitively).

Practically, this means that every field recursively has a bang (`!`). This is a difficult property to enforce by hand, since a single `Maybe` or lazy tuple can breach this property. For instance:
```
data A = A !Int
data B = B !(Maybe Int)
data Maybe a = 
  Nothing
  | Just a
```
Both `A` and `B` are strict, as all fields have a `!`. But `B` is not deep strict, because `Maybe` doesn't have a bang on the field of its `Just` constructor.

`th-deepstrict` let's us make assertions like below:
```
$(assertDeepStrict =<< [t| A |])
$(assertDeepStrict =<< [t| B |])
--     Main.B
-- is not Deep Strict, because:
-- Main.B
--  con Main.B
--    field 0
--      GHC.Maybe.Maybe GHC.Types.Int 
--        con GHC.Maybe.Just field 0 is lazy
```
`A` succeeds whereas `B` fails. 

As you can see, when a type is not deep strict, `th-deepstrict` gives us the reason why.
This means that it is helpful for writing golden tests for datatypes where you know some fields aren't meant to be deep strict, but where you don't want to regress on other fields.
Examples of golden tests can be found in the test suite.

`th-deepstrict` works best when combined with libraries like `strict`, or `strict-wrapper`, and `strict-containers`.

Many datastructures in the Haskell ecosystem, while guaranteed not to contain unwanted thunks, are not technically deep strict.
This is the case because GHC's array does not guarantee through its types that values are not thunks, even if libraries guarantee this in practice.
Even `strict-container`'s `Map` type will come out as not deep strict. To work around issues like this, `th-deepstrict` allows inputting deep strictness overrides.
So, you can say `Map k x` is deep strict if both `k` and `x` are deep strict.

### Conclusion

`th-deepstrict` is an addition to the Haskell programmers arsenal of tools to reason about and enforce strictness conditions.
It allows programmers to see clearly where laziness lies in a data structure. 
This allows one to be confident that space leaks cannot arise from a datatype.
