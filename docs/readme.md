# dynamical
Based on the dependent lenses described in [Niu and Spivak](https://topos.site/poly-book.pdf):

<figure>
  <img src="IMAGE.jpg" alt="A mermaid.js chart depicting a wiring diagram." width=50% height=50%>
  <figcaption style="text-align: justify;">A wiring diagram: &nbsp; `<span style="font-family:Courier">Plant</span> ⊗ <span style="font-family:Courier">Controller</span> → <span style="font-family:Courier">System</span>`</figcaption>
</figure>

### Modules
 - [`dynamical-fsm`](#dynamical-fsm): composable finite state machines
 - [`dynamical-fs2`](#dynamical-fs2): integration with [fs2](https://fs2.io/#/) streams

## `dynamical-fsm`
 - libarary for Scala @SCALA@ (JS, JVM, and Native platforms)
 
```scala
"com.julianpeeters" %% "dynamical-fsm" % "@VERSION@"
```

##### `Moore[P[_]]`

The most basic finite state machines are those parameterized by a polymap from
a store to a monomial:

```scala mdoc
import polynomial.`object`.Monomial.{Interface, Store}
import polynomial.morphism.~>
import dynamical.fsm.Moore

type Fsm[S, A, B] = Moore[Store[S, _] ~> Interface[A, B, _]]
```

However, in order to run them as a function `(S, A) => (S, B)`, we need the
output `B` to be a function from input to output, `A => B`. For example:

```scala mdoc:reset
import cats.implicits.given
import dynamical.fsm.Moore
import polynomial.morphism.~>
import polynomial.`object`.Monomial.{Interface, Store}

def mealified[Y]: Moore[Store[Boolean, _] ~> Interface[Int, Int => Int, _]] =
  Moore[Boolean, Int, Int => Int, Y](
    false,
    s => x => if s then x + x else x,   // if we've seen x > 1, then emit 2x
    (s, x) => if x > 1 then true else s // change state upon seeing x > 1
  )
  
val l: List[Int] = List(1, 2, 3).mapAccumulate(mealified.init)( (s, a) =>
  (mealified.update(s, a), mealified.readout(s)(a))  
)._2
```

##### `Mealy[P[_]]`

Mealy machines have a dedicated `run` method. A Moore machine forms a Mealy
machine if the output is a function from input, `A`, to output, `A => B`. For
example:

```scala
import cats.implicits.given // for `mapAccumulate`
import dynamical.fsm.{Moore, Mealy}
import polynomial.morphism.~>
import polynomial.`object`.Monomial.{Interface, Store}

def moore[Y]: Moore[Store[Boolean, _] ~> Interface[Int, Int => Int, _]] =
  Moore[Boolean, Int, Int => Int, Y](
    false,
    s => x => if s then x + x else x,   // if we've seen x > 1, then emit 2x
    (s, x) => if x > 1 then true else s // change state upon seeing x > 1
  )
val m: Mealy[Store[Boolean, _] ~> Interface[Int, Int => Int, _]] = moore.asMealy
val l: List[Int] = List(1, 2, 3).mapAccumulate(m.init)(m.run)._2
// l: List[Int] = List(1, 2, 6)
```


##### `Wiring[P[_]]`

Wirings, in contrast to state systems, are the interface systems that allow us
to represent interaction patterns.

For example, we could the compose a state system with an wiring diagram of the
following type:

```scala
((Plant ⊗ Controller) ~> System)[Y]
```

There are several aspects to the composition of state systems with wiring diagrams:
  - such a wiring is said to be "filled" (or "loaded") by composition with a state system
  - after composition, `System` is then said to "wrap" such a state system, as a "wrapper interface"
  - composition introduces no delay into the machine, since we defined the controller to emit a runnable function

```scala mdoc:reset:height=5
import cats.implicits.given
import dynamical.fsm.{Mealy, Moore, Wiring}
import polynomial.`object`.Monomial.{Interface, Store}
import polynomial.morphism.~>
import polynomial.product.⊗

type Plant[Y]      = Interface[(Byte, Byte => Char), Char, Y]
type Controller[Y] = Interface[Char, Byte => Char, Y]
type System[Y]     = Interface[Byte, Byte => Char, Y]
type ω[Y] = ((Plant ⊗ Controller) ~> System)[Y]

val w: Wiring[ω] = Wiring(
  b => a => b._2(a),
  (b, a) => ((a, b._2), b._2(a))
)

val m: Moore[(Store[Char, _] ⊗ Store[Byte => Char, _]) ~> (Plant ⊗ Controller)] =
  (Moore[Char, (Byte, Byte => Char), Char, Nothing](" ".charAt(0), identity, (s, i) => i._2(i._1))
    ⊗ Moore[Byte => Char, Char, Byte => Char, Nothing](b => b.toChar, identity, (f, i) => if i != ' ' then f else b => b.toChar.toUpper))

val fsm: Mealy[((Store[Char, _] ⊗ Store[Byte => Char, _]) ~> (Plant ⊗ Controller) ~> System)] =
  m.andThen(w).asMealy

val input = "hello world".getBytes().toList

val result: String = input.mapAccumulate(fsm.init)(fsm.run)._2.mkString
```

(Note: example adapted from [Niu and Spivak](https://topos.site/poly-book.pdf))

## `dynamical-fs2`
 - libarary for Scala @SCALA@ (JS, JVM, and Native platforms)
 - depends on fs2 @FS2@
 
```scala
"com.julianpeeters" %% "dynamical-fs2" % "@VERSION@"
```

The `dynamical-fs2` library provides fs2 integration, in the form of a stream
`transducer` pipe:

```scala mdoc:reset
import dynamical.fsm.Mealy
import dynamical.stream.transducer
import fs2.Stream
import polynomial.morphism.~>
import polynomial.`object`.Monomial.{Interface, Store}

val m: Mealy[Store[Boolean, _] ~> Interface[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)

val l: List[Int] = Stream(1, 2, 3).through(m.transducer).compile.toList
```