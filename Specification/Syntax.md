# Bloom Syntax Specification

This document will detail what valid bloom syntax should look like with code examples.

For parser syntax we will use [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form).

Bloom is an LL(1) Language (after tokenisation).

## Tokenization

This section will specify what valid tokens are and how they are formed.

### Comments

Comments are not relevant to the interpretation of the syntax. They are introduced by `# ` and `#| `, the white space after the symbol is obligatory

```cs
# normal comment
#| documentation comment
```

### Names

Names may be of the following 3 forms.

- Pascal case `PascalCase123`.
- Snake case `snake_case123`.
- Custom symbolic `<|>` (which may be made from any of `!$%^&*-+=:@~|<>?./`).

Some name will be classified as keywords, the rest will be treated as identifiers.

A special case is made for `?.` which gets processed into `?` and `.` so that you can bind in the middle of a method chain.

### Token Litterals

Natural numbers consist of a sequence of simple digits, if this sequence of single digits is followed by a snake case name without space then that name is interpreted as a *unit*.

Strings are broken up into four toke types:
- Small string `"non-interpolated"`
- String start `"interpolation-start{`
- String middle `}interpolation-continue{`
- String end `}interpolation-end"`

### White Space

All white space is removed before sending the tokens to the parser, however certain spacing tokens are generated:

- `<open>` corresponds to an opening curly bracket.
- `<item>` corresponds to the start of a new item inside a curly bracket section.
- `<close>` corresponds to a closing curly bracket.

The complexity comes with how `<item>`s are placed and how `<close>` must be structured.

#### Empty Blocks

In the case that there are only white space tokens between the "{" and the "}" the whole section is replaced with `<open> <close>`.

```py
{
    # empty :3
}
```

#### Inline Blocks

If a curly bracket region is only on one line, the "{" is replaced with `<open> <item>` and the "}" is replaced with `<closed>`, this makes sure that you can only make a single line curly region with one statement.

```
{ attr = (x, y) }
```

#### Multi-line Blocks

In the general case we again replace the "{" with `<open>` but this time we remember the indentation of the first non-empty line, then:
- All indentation levels greater then this are ignored.
- All matching indentation levels in the block are replaced with `<item>`.
- The first indentation level less then this line must be immdediately followed by "}", in which case we form the `<close>` token. 

```rs
{
    let x = 1
    let y = 2
    x + y
}
```

#### Keyword Fusing

If the `<item>` token comes before any of the keywords:
- "end"
- "elif"
- "else"
- "with"
- "catch"

Then they will be **fused** forming the tokens:
- `<item"end">`
- `<item"elif">`
- `<item"else">`
- `<item"with">`
- `<item"catch">`

...respectively.

## Common Syntax

Syntax that is shared between different areas.

### Patterns

The parser `<total-pattern>` is the same as `<pattern>`, but signifies that the parsed pattern should be total. It has no grammar significance, just semantic significance.

Same with `<compact-total-pattern>` and `<compact-pattern>`.

```bnf
<record-match> ::=
    <open> (<item> <snake-name> "=" <pattern>)* <close>

<compact-pattern> ::=
    "mut"? <snake-name>
    | "(" ( <pattern> ("," <pattern>)* )? ")"

<curry-pattern> ::=
    <compact-pattern> ("|" <compact-pattern>)*
    | <pascal-name> <compact-pattern>* <record-match>?

<tuple-pattern> ::=
    <curry-pattern> ("," <curry-pattern>)*

<pattern> ::=
    <tuple-pattern> (<symbolic-name> <tuple-pattern>)*

<multi-pattern> ::=
    <pattern> (";" <pattern>)*
```

### Argument Handling

```bnf
<arg> ::=
    <total-pattern> <typed>?

<tuple-args> ::=
    <arg> ("," <arg>)*

<curry-args> ::=
    <tuple-args>? (";" <tuple-args>?)*
```

### Map and Gaurd Syntax

Anywhere where you could write `x y =>`, you may also use certain syntax extensions designed for brevity of compound control flow.

```bnf
<gaurd> ::=
    ("|" <pattern>)+ "=>" <action>

<multi-gaurd> ::=
    ("|" <multi-pattern>)+ "=>" <action>

<map> ::=
    <compact-total-pattern> "=>" <action>
    | <gaurd>*

<multi-map> ::=
    <compact-total-pattern>+ "=>" <action>
    | <multi-gaurd>*
```

## Expressions

### Expression Structure

Expressions are built up through multiple levels of semantics.

#### Compact

The first level are compact expressions, these are the fundimental building blocks that other syntax has precedence over.

```bnf
<compact-expr> ::=
    "(" <expr>? ")" 
    | <block>
    | <snake-name>
```

#### Chains

A chain is a series of member accesses and method calls, the keyword ";" is used to delilit curried arguments in this compact syntax.

```bnf
<invocation> ::=
    "(" <expr>? (";" <expr>?)* ")"
<chain-part> ::=
    "." <snake-name> <invocation>?
    | "?" ("." <snake-name> <invocation>?)?
<chain-expr> ::=
    <compact-expr> <chain-part>*
```

#### Currying

A functional, lambda calculus inspired syntax for function application.

Certain function flavoured keywords live here.

```bnf
<curry-expr> ::=
    "new" <pascal-name> <chain-expr>*
    | <chain-expr>+
    | <fold>
    | <yield>
```

#### Symbols

Where tuples are constructed and custom operators applied!

Operators in symbolic expressions (specifically those at the start and end) may be partially applied, creating a lambda.

```bnf
<tuple-expr> ::=
    <curry-expr> ("," <curry-expr>)*

<symbolic-full-expr> ::=
    <tuple-expr> (<symbolic-name> <symbolic-full-expr>?)?
<symbolic-partial-expr> ::=
    <symbolic-name> (<tuple-expr> <symbolic-partial-expr>?)?
<symbolic-expr> ::=
    <symbolic-full-expr>
    | <symbolic-partial-expr>
```

#### Expression Keywords

Finally we have control flow, which wraps everything else providing structure to the expression.
The keyword `$` creates a fold right chain of sub-expressions which can help reduce cluttered parenthesis.

```bnf
<control-flow> ::=
    <lambda>
    | <if-else>
    | <for-in>
    | <match-with>
    | <switch-with>
    | <try-catch>

<expr> ::=
    <symbolic-expr> (<control-flow> | "$" <expr>)?

<exit-flow> ::=
    <break>
    | <continue>
    | <goto>
    | <return>
    | <throw>

<do-action> ::=
    "do" <expr>
    | <exit-flow>
    | "pass"

<action> ::=
    <expr>
    | <exit-flow>
    | "pass"
```

### Control Flow

Syntax that handles branching and logic execution.

#### Lambdas

Lambdas are declared with the `fn` keyword and allow logic to be itself ran programatically.

```bnf
<lambda> ::=
    "fn" <multi-map>
```

```rs
fn n => n + 1
```

#### If Else

If is the classic control flow operator. It works as an expression, evaluating as the `do` branch when `cond` is true, and evaluating as the `else` branch when `cond` is false. The condition is allowed to be a partial match when using `is`.

```bnf
<elif> ::=
    "elif" <expr> ("is" <pattern>)? <do-action> ("else" <action> | <elif>)?

<if-else> ::=
    "if" <expr> ("is" <pattern>)? <do-action> ("else" <action> | <elif>)?
```

```rs
if cond
    do true_branch
    else false_branch
```

#### For In

For iterates over a collection allowing you to `break` the loop prematurely, otherwise you will enter the `end` branch if it is defined.

```bnf
<for-in> ::=
    "for" <total-pattern> "in" <expr> <do-action> ("end" <action>)?

<break> ::=
    "break" <expr>

<continue> ::=
    "continue"
```

```rb
let mut n = 0

for x in xs do {
    if x is Bad
        break None
    if x is Good m do {
        n += m
        continue
    }
    n -= 1
} end
    Some n
```

#### Match

Match is a more general purpose version of `if`. The first branch with pattern matching the expression is chosen. In the case of inductively defined data structures one may use `fold` on a sub-component to recursively apply the match statement.

```bnf
<match-with> ::=
    "match" <expr> (";" <expr>)* "with" <multi-gaurd>*
    
<fold> ::=
    "fold" <chain-expr>+
```

```ocaml
match [1, 2, 3] with
    | [] => 0
    | x >> xs => x + fold xs
    ...
```

#### Switch

Switch is a variant of `match`. It exists to handle more complicated control flow fluidly, without forcing the progrmmer to create whole new `enum` just for that purpose. It does this by defining the cases it matches on **as it defines the match rules** for those cases.

You can use `goto` when inside the expression the `switch` is matching on to route to the end of the expression and immdiately start branching!

```bnf
<switch-with> ::=
    "switch" <expr> (";" <expr>)* "with" <multi-gaurd>*
    
<goto> ::=
    "goto" <pascal-case> <chain-expr>*
    
```

```C
switch Normal $ non_zero {
    if test1 n
        goto Foo n
    if test2 n
        goto Bar
    n-1
} with
    | Foo n => ...
    | Bar   => ...
    | Normal (Some n) => ...
    | Normal None     => ...
```

#### Return

The quintessential early return keyword, routes to the end of the function.

```bnf
<return> ::=
    "return" <expr>
```

```py
def func obj = {
    if obj.is_bad
        return None
    ...
}
```

#### Throw and Try Catch

Exception handling using algebraic data types.

```bnf
<throw> ::=
    "throw" <expr>
    
<try-catch> ::=
    "try" <action> "catch" <map>
```

```c#
throw new Exception {
    .msg = "ohno"
}
```

```c#
try
    ...
catch err =>
    ...
```

#### Yield

Return an additional value for supported return types.

```bnf
<yield> ::=
    "yield" <chain-expr>+
```

```rb
for x in xs do
    yield x
```

## Blocks

Blocks are how we start doing imperative logic.

```bnf
<flow-stmt> ::=
    <if-else-stmt>
    | <for-in-stmt>
    | <match-with-stmt>
    | <switch-with-stmt>
    | <try-catch-stmt>

<block-stmt> ::=
    <let-stmt>
    | <flow-stmt>
    | <action>
    
<block-cont> ::=
    <block-stmt>
    | <flow-cont>
    
<block> ::=
    <open> (<item> <block-cont> | <close>)
```

### Statement Control Flow

If each `<item>` token in a block had to precisely correspond to a new statement then we would not be able to do the following:

```py
if cond_0 do
    yield 0
elif cond_1 do
    yield 1
else
    pass
```

```bnf
<elif-stmt> ::=
    <item"elif"> <expr> ("is" <pattern>)? <do-action> (
        "else" <action>
        | <elif>
        | <item"else"> <action>
        | <elif-stmt>
    )?

<if-else-stmt> ::=
    "if" <expr> ("is" <pattern>)? <do-action> (
        "else" <action>
        | <elif>
        | <item"else"> <action>
        | <elif-stmt>
    )?

<for-in-stmt> ::=
    "for" <total-pattern> "in" <expr> <do-action> (
        "end" <action>
        | <item"end"> <action>
    )?
    
<match-with-stmt> ::=
    "match" <expr> (";" <expr>)* (
        "with" <multi-gaurd>*
        | <item"with"> <multi-gaurd>*
    )

<switch-with-stmt> ::=
    "match" <expr> (";" <expr>)* (
        "with" <multi-gaurd>*
        | <item"with"> <multi-gaurd>*
    )

<try-catch-stmt> ::=
    "try" <action> (
        "catch" <map>
        | <item"catch"> <map>
    )
```

### Let

Let statements instantiate variables from a total pattern using an expression. These variables can then be used later in the block.

```bnf
<let-stmt> ::=
    "let" <total-pattern> <typed>? "=" <expr>
```

```rs
let Foo x y = ...
```

## Top Level Declarations

This section will describe how new constructs are introduced to the program.

### Structs

Structs hold multiple separate elements.

```cs
struct FooBar {
    .foo: Foo
    .bar: Bar
}
# record syntax
new FooBar {
    .foo = x
    .bar = y
}
# function syntax
FooBar::new x y
```

### Enums

Enum `case`s are built in the same way as `struct`s.

```C
enum Thingy {
    case Foo (x: Int) (y: Int)
    case Bar Int {
        .attr: Nat = 0
        .other_attr: Nat
    }
}

new Thingy::Foo 0 1
```

### Interfaces

Interfaces offer a method to generalize code in object oriented way that is compatible with type errasure.

```hs
pin interface Iterable[out T] {
    pin def iter[U]: ForElse[T, U] -> U

    pin def pop: Maybe[T] = {
        self.iter new ForElse {
            .give item = Exit $ Some item
            .end = None
        }
    }
}
```

### Function Definitions

Functions are defined with `def` and may contain generic arguments and matched arguments before their type.

```py
def first[T | T is Drop]: List[T] -> Maybe[T] = fn
    | []     => None
    | x +> _ => Some x
def combine(n: Nat, m: Nat): Nat = n + m + (n * m)
```

## Types

...