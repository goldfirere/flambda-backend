# Adding null to types

This document proposes a way we can add a null value to types, thus granting a
non-allocating version of `option`. This proposal builds on the *layouts*
system described in the [main proposal for unboxed types](index.md). Before
reading this document, you may wish to read up through the
[layouts](index.md#layouts) section of the main proposal.

# Adding null

Now that layouts provide a way to segment the world of types, we can
leverage this feature to provide an `option` type -- dubbed `or_null`
-- that requires no allocation. This section describes how it all
works.

## The `value_or_null` layout

The key observation powering `or_null` is that ordinary OCaml values are
never equal to the word 0. A pointer will always be non-null (and thus different
from 0), and a tagged integer always has its bottom bit set. Because 0 can
never be a valid pointer, we can safely update the garbage collector not to
traverse null pointers.

We thus want `t or_null` to be just like `t`, except now with a meaning
for 0. There is still a small problem, though: we cannot ever have `t or_null
or_null`, as that gives *two* meanings for 0. We thus say that the argument
to `or_null` must be a *non-null type*; that is, it has room for 0. However, `t
or_null` is a *with-null type*; that is, it uses 0 in its representation.

Here is the layout structure:

```
              any
               |
         value_or_null
               |
             value   (* instead of non_null_value; discussed below *)
```

Note that `value_or_null` is above `value`. This is because anything
that can be done with a with-null type can be done with a non-null one, but
non-null types have an extra capability (they can be the argument to `or_null`);
this is thus the usual case where a subtype has an extra capability over the
supertype.

Supporting non-null immediates follows naturally: as explained more fully in the
page on [kinds](kinds.md), `immediate` is just `value` with the ability to
mode-cross, and so the little layout lattice above powers a kind like

```
kind immediate_or_null = 
  value_or_null & always global & always external & always unique & always many
```

Indeed, this definition will be part of the initial environment.

The layout `value_or_null` is *concrete*: we can compile
a function that manipulates `value_or_null`s.

Along with the kind-system changes above, the following type will be in the initial
environment:

```ocaml
type ('a : value) or_null : value_or_null
```

In addition, we will allow `'a or_null` to mode-cross the same way that
`'a` does -- effectively allowing `or_null` to work nicely with `immediate`s.
This can be implemented by "looking through" the
`or_null` type constructor during mode-cross-checking. (The alternative would be to
have some degree of kind polymorphism, where we have
`type ('a : non_null_value & 'modes) or_null : value & 'modes`
or similar, but looking through
`or_null` is easy enough so that we can avoid this extra complexity.)
<!-- CR layouts v2.8: Actually we *are* doing kind polymorphism. But that's
not documented yet. We should update this after that design has settled. -->
This permits, e.g. `int or_null` to still be subject to
optimizations around immediates.

The `or_null` type has constructors `This` and `Null`, usable for both
construction and pattern-matching. The implementations are magical and do
not allocate.

## Naming

### Layouts

The key question: Which of the two layouts should have the name `value`? We
decided that the layout describing all the types in OCaml since its creation
should retain the name `value`. The new layout has the new name.

Slightly suboptimal in this choice is that users will more often have to write
`value_or_null` and not `value`. For a while, we thought it was better to have
the more-frequently-written layout have the shorter name. But in the end, we
decided to call users' attention to the novelty of `value_or_null` by making
it stand out. Slightly uncomfortable here is that less knowledgeable users might
just write `value` as an alternative to, say, `float64`, without thinking through
the with-null or no-null decision. There is no perfect answer.

Having decided to make the no-null layout `value`, what about the with-null layout?
Some have suggested `nullable_value`. We actually tried this for a while in
conversations, but kept getting confused: is a `nullable_value` one that might
be null? or is it one that is available for adding null to? We couldn't keep it
straight, so we changed the name.

### Types

Is there really a better alternative to `or_null`? Nope, we didn't think so either.

### Constructors

Choosing names for constructors also was challenging. Some other possibilities:

* `Some` and `None`. These could usefully be disambiguated the usual way from
`option` constructors. But readers of code wouldn't know what they're getting
without, say, merlin support. Seems unnecessarily dangerous.

* `S0me` and `N0ne`. Please, no. (But these were suggested multiple times!)

* `Just` and `Nothing`. These are tried and true in Haskell, but folks seem
to want names that have the same length, for easy refactoring and cleaner diffs.

* `This` and `Null`. There was a natural affinity for `Null` being one of the
constructors. So then we wanted another 4-letter word. `Just` would work, but
some (with Haskell experience) find it jarring having `Just` paired with
something other than `Nothing`. `This` seems to work pretty nicely.

## Defaults

When inferring the kind of a type, we must consider what we use as the
default layout. In general, we want make a choice low in the lattice for
covariant positions and high in the lattice for contravariant positions, as
these defaults will allow for more expressiveness. That is, if we say
`type 'a t` (with no `=`), a low-in-the-lattice layout for `t` means that `t`
can be used in more contexts, while a high-in-the-lattice layout for `'a` means
that `t` can be applied to more types. With this in mind, we will use the
following defaults:

```ocaml
type t : <<here>>                 (* default: value *)
type ('a : <<here>>) t            (* default: value *)
fun (type (a : <<here>>)) -> ...  (* default: value_or_null *)
let f : ('a : <<here>>). ...      (* default: value_or_null *)
val f : ('a : <<here>>). ...      (* default: value_or_null *)
```

Abstract types `t` should have a default layout of `value`, as this is
both low in the lattice and also the layout of any type declared with a record,
variant, extensible variant, or object type definition. It thus seems likely
that the default will work with both usages of `t` and the definition of `t`.

A parameter to an abstract type `'a` in `type 'a t` (ditto the type variables in
`type ('a, 'b) t`) also defaults to have layout `value` -- even though
this choice is low in our lattice. The reason we must do this is to maintain
backwards compatibility. Consider the following module:

```ocaml
module M : sig
  type 'a t
end = struct
  type 'a t = 'a
end
```

If the default for `'a` were anything different from the default for `t`, this
definition would not be accepted. This is unfortunate, because our other
principle of availability is sacrificed in order to maintain backward
compatibility here: defaulting `'a : value` means that e.g. `string
or_null Widget.t` is disallowed for a `Widget` module declared with no kind
annotations. (This particular choice of default is hard, and may be revisited;
defaulting `'a : any` is much more compelling here, but we would lose the
ability to write `M` here without annotations.)

In rigid type variables introduced in function types, we default to
`value_or_null`, as doing so seems like a happy compromise. Going higher in the
lattice (i.e. `any`) makes functions too hard to define (e.g. `let id : 'a. 'a
-> 'a = fun x -> x` is rejected), and going lower (i.e. `value`) makes
functions too hard to call. This, too, may end up revisited in the light of
experience.

## Sub-typing

Because any valid `t` is also a valid `t or_null`, we have a natural sub-typing
relationship: `t` is a sub-type of `t or_null` for all `t` (such that `t
or_null` is well formed). Note that this relationship is entirely notionally
separate from the sub-layout relationships described above. Here, we're talking
about term-level conversions such as in

```ocaml
let f (x : int) = (x :> int or_null)
```

## Arrays

### The Problem

A challenge arises in the interaction between `or_null` and the flat-float array
optimization (FAO). The FAO allows a `float array` to store its contents inline,
without indirection. When allocating a `float array`, the allocated memory gets
a special tag to tell the garbage collector not to scan any elements of the
array. (Scanning `float`s would be disastrous!) Other arrays invite the garbage
collector to scan.

With that in mind, consider the implementation of a function

```ocaml
val init : len:int -> (int -> 'a) -> 'a array
```

This polymorphic function must be capable both of allocating a `float array`
(with the float-array tag) and a `string array` (with the general-array tag).
Of course, the function cannot ask what `'a` is at runtime. In practice, what it
does is to produce the first element of the array first, then inspect it to
see whether it's a `float` (`float`s have a runtime tag distinct from other
types' tags), and then allocate an array with the appropriate tag.
(If the length is 0, then `init` returns a pointer to the pre-allocated empty
array, which doesn't care about its tag.) This all exists and has existed for
a long time. It works.

But now consider what happens when we are making a `float or_null array`. When
our `init` produces the first `float or_null`, what will it do? If that element
is `This 3.14`, that is identical at runtime to `3.14` (both are pointers to a
block with the `float` tag), and so `init` will allocate a `float array` with
the float-array tag. Yet now if we store a `Null` in the array, we won't be able
to tell the difference between `Null` and the floating-point number comprised of
all 0 bits. Conversely, if the first element is `Null`, then `init` just won't
know what to do. If it defaults to allocating a normal array, trying to store
a `This 3.14` in it -- inlined! -- will cause the garbage collector to crash.
This just won't work.

The key observation powering the solution below is that the problem strikes
only when an array is _created_. Operations on existing arrays can comfortably
check the tag on the allocated array. It's only functions that create arrays
that need to decide whether the array should be flat or not.

With this in mind, it's tempting to just say

```ocaml
val init : ('a : value). len:int -> (int -> 'a) -> 'a array
```

and be done with it. Note the kind annotation there, saying that `init` works
only with `value`s (excluding `float or_null`). Of course, we'd also need

```ocaml
val generic_init : len:int -> (int -> 'a) -> 'a array
```

(which defaults its `'a` to have kind `value_or_null`), except that this
function never applies the FAO.

The problem with this tempting solution is that it's not backward compatible.
Today, I can write

```ocaml
module M : sig
  val one : 'a -> 'a array
end = struct
  let one x = [| x |]
end
```

yet this won't work with the approach I've outlined here. The `'a` in the
signature has kind `value_or_null`, but `x`'s type in the structure must
have kind `value`. (Or, perhaps `[| x |]` won't apply the FAO, which is
backward-incompatible in a different, more evil, sneakily performance-changing
way.)

We could require roughly every type variable in every signature to have `:
value_or_null` annotations, or read on for another approach.

### The Solution

The initial environment will contain these 


## Examples

### A safe, non-allocating `hd`

```ocaml
(* in list.mli *)
val hd' : 'a t -> 'a or_null
  (* we'll infer that ['a] has layout [non_null_value] *)

(* somewhere else *)
let f xs default = match List.hd' xs with
| None -> default
| Some x -> x
  (* we'll infer [f : ('a : non_null_value). 'a List.t -> 'a -> 'a] *)
```

Note that nothing changes about `List.t` itself here. This means that
a data structure can be designed to hold `value`s (that might be null) but that
individual functions over that structure can be specialized to work with
`non_null_value`s.
