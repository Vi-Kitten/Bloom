# Bloom

**Big WIP Language**

A language for writing safe, strongly normalising programs, with mixed paradigm semantics.

# Intended Features

## Functional Syntax

Parenthesis scare me.
```rs
obj.method $
    f x y <$> [1, 2, 3]
```

## Linear Typing

One may type contracts such that:
- Entitlements may not be exceeded.
- Obligations may not be avoided.

Among other things, this means instances of *arbitrary* types cannot be duplicated or dropped. 

## Generic Variance

Certain generic types preserve subtyping relations, others still reverse them, we call such parameters covariant and contravariant respectively.

Covariant paramters are denoted with `out`, and are usually collections of some sort.

Contravariant parameters are denoted with `in`, and are usually consumers.

Anything else is assumed to not impact subtyping relations.

## Interfaces

For all your type errasing needs.
```rs
enum LoopStep[out State, out U] {
    case Continue State
    case Exit U
}

interface Collector[in T, out U] {
    def give: T -> LoopStep[Self, U]
    def end: U
}

pin interface Iterable[out T] {
    pin def collect[U]: Collector[T, U] -> U

    pin def pop: Maybe[T] = self.collect new Collector {
        give item = Exit $ Some item
        end = None
    }
}
```

## Traits

For behaviours that aren't object oriented.
```rs
trait Monoid {
    def <> : Self -> Self -* Self
    def empty: Self
}

trait Monad where Self[out type] {
    def bind[T, U]: (T -> Self[U]) -> (Self[T] -> Self[U])
    def pure[T]: T -> Self[T]
}
```