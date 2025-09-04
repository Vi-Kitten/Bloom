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

## Interfaces

For all your type errasing needs.
```rs
enum LoopStep[out State, out U] {
    case Continue State
    case Exit U
}

interface Collector[in T, out U] {
    fn give: T -> LoopStep Self U
    fn end: U
}

pin interface Iterable[out T] {
    pin fn collect[U]: Collector T U -> U

    pin fn pop: Maybe T {
        self.collect new Collector {
            give item = Exit $ Some item
            end = None
        }
    }
}
```

## Traits

For behaviours that aren't object oriented.
```rs
trait Monoid {
    static fn <> : Self -> Self -* Self
    static fn empty: Self
}
```