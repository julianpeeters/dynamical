# dynamical
Based on the dependent lenses described in [Niu and Spivak](https://topos.site/poly-book.pdf).

### Modules
 - [`dynamical-fsm`](#dynamical-fsm): composable finite state machines
 - [`dynamical-fs2`](#dynamical-fs2): integration with [fs2](https://fs2.io/#/) streams

## `dynamical-fsm`
 - libarary for Scala 3 (JS, JVM, and Native platforms)
 
```scala
"com.julianpeeters" %% "dynamical-fsm" % "0.2.0"
```

##### `Moore[P[_]]`

The most basic finite state machines are those parameterized by a polymap from
a store to a monomial:

```scala
import polynomial.`object`.Monomial
import polynomial.morphism.~>
import dynamical.fsm.Moore

type Fsm[S, A, B] = Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _]]
```

However, in order to run them as a function `(S, A) => (S, B)`, we need the
output `B` to be a function from input to output, `A => B`. For example:

```scala
import cats.implicits.given
import dynamical.fsm.Moore
import polynomial.morphism.~>
import polynomial.`object`.Monomial

def mealified[Y]: Moore[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int => Int, _]] =
  Moore[Boolean, Int, Int => Int, Y](
    false,
    s => x => if s then x + x else x,   // if we've seen x > 1, then emit 2x
    (s, x) => if x > 1 then true else s // change state upon seeing x > 1
  )
  
val l: List[Int] = List(1, 2, 3).mapAccumulate(mealified.init)( (s, a) =>
  (mealified.update(s, a), mealified.readout(s)(a))  
)._2
// l: List[Int] = List(1, 2, 6)
```

##### `Mealy[P[_]]`

Mealy machines have a dedicated `run` method. A Moore machine forms a Mealy
machine if the output is a function from input, `A`, to output, `A => B`. For
example:

```scala
import cats.implicits.given // for `mapAccumulate`
import dynamical.fsm.{Moore, Mealy}
import polynomial.morphism.~>
import polynomial.`object`.Monomial

def moore[Y]: Moore[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int => Int, _]] =
  Moore[Boolean, Int, Int => Int, Y](
    false,
    s => x => if s then x + x else x,   // if we've seen x > 1, then emit 2x
    (s, x) => if x > 1 then true else s // change state upon seeing x > 1
  )
val m: Mealy[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int => Int, _]] = moore.asMealy
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

```scala
import cats.implicits.given
import dynamical.fsm.{Mealy, Moore, Wiring}
import polynomial.`object`.Monomial
import polynomial.morphism.~>
import polynomial.product.⊗

type Plant[Y]      = Monomial.Interface[(Byte, Byte => Char), Char, Y]
type Controller[Y] = Monomial.Interface[Char, Byte => Char, Y]
type System[Y]     = Monomial.Interface[Byte, Byte => Char, Y]
type ω[Y] = ((Plant ⊗ Controller) ~> System)[Y]

val w: Wiring[ω] = Wiring(b => a => b._2(a), (b, a) => ((a, b._2), b._2(a)))
// w: Wiring[ω] = dynamical.fsm.Wiring$$anon$4@95975a8

val m: Moore[(Monomial.Store[Char, _] ⊗ Monomial.Store[Byte => Char, _]) ~> (Plant ⊗ Controller)] =
  (Moore[Char, (Byte, Byte => Char), Char, Nothing](" ".charAt(0), identity, (s, i) => i._2(i._1))
    ⊗ Moore[Byte => Char, Char, Byte => Char, Nothing](b => b.toChar, identity, (f, i) => if i != ' ' then f else b => b.toChar.toUpper))
// m: Moore[~>[⊗[[_$5 >: Nothing <: Any] => Store[Char, _$5], [_$6 >: Nothing <: Any] => Store[Function1[Byte, Char], _$6]], ⊗[Plant, Controller]]] = dynamical.fsm.Moore$$anon$19@34d2e209

val fsm: Mealy[((Monomial.Store[Char, _] ⊗ Monomial.Store[Byte => Char, _]) ~> (Plant ⊗ Controller) ~> System)] = m.andThen(w).asMealy
// fsm: Mealy[~>[~>[⊗[[_$7 >: Nothing <: Any] => Store[Char, _$7], [_$8 >: Nothing <: Any] => Store[Function1[Byte, Char], _$8]], ⊗[Plant, Controller]], System]] = dynamical.fsm.Moore$$anon$15@3d4539e8

val s: String = "hello world".getBytes().toList.mapAccumulate(fsm.init)(fsm.run)._2.mkString
// s: String = "hello WORLD"
```

(Note: example adapted from [Niu and Spivak](https://topos.site/poly-book.pdf))

## `dynamical-fs2`
 - libarary for Scala 3 (JS, JVM, and Native platforms)
 - depends on fs2 3.9
 
```scala
"com.julianpeeters" %% "dynamical-fs2" % "0.2.0"
```

The `dynamical-fs2` library provides fs2 integration, in the form of a stream
`transducer` pipe:

```scala
import dynamical.fsm.Mealy
import dynamical.stream.transducer
import fs2.Stream
import polynomial.morphism.~>
import polynomial.`object`.Monomial

val m: Mealy[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
// m: Mealy[~>[[_$9 >: Nothing <: Any] => Store[Boolean, _$9], [_$10 >: Nothing <: Any] => Interface[Int, Function1[Int, Int], _$10]]] = dynamical.fsm.Mealy$$anon$2@6debc994

val l: List[Int] = Stream(1, 2, 3).through(m.transducer).compile.toList
// l: List[Int] = List(2, 4, 6)
```