# dynamical
Based on the dependent lenses described in [Niu and Spivak](https://topos.site/poly-book.pdf)

### Modules
 - [`dynamical-fsm`](#dynamical-fsm): composable finite state machines
 - [`dynamical-fs2`](#dynamical-fs2): integration with [fs2](https://fs2.io/#/) concurrent streams

## `dynamical-fsm`
 - libarary for Scala 3 (JS, JVM, and Native platforms)
 - depends on polynomial 0.1 (and, internally, destructured 0.2)
 
```scala
"com.julianpeeters" %% "dynamical-fsm" % "0.0.0"
```

The `dynamical-fsm` library provides the components of finite state machines:
 - `Moore[P[_]]`: "Simple & Composable"
 - `Mealy[P[_]]`: "Composable & Runnable"
 - `Wiring[P[_]]`: "Adaptable & Abstractable"

##### `Moore[P[_]]`

The mose basic finite state machines are those parameterized by a polymap from
a store to a monomial:

```scala
import polynomial.`object`.{Monomial, Store}
import polynomial.morphism.~>
import dynamical.fsm.Moore

type Fsm[S, A, B] = Moore[Store[S, _] ~> Monomial[A, B, _]]
```

However, in order to run them as a function `(S, A) => (S, B)`, we need the
output `B` to be a function from input to output, `A => B`. For example:

```scala
import cats.implicits.given
import dynamical.fsm.Moore
import polynomial.morphism.~>
import polynomial.`object`.{Monomial, Store}

def mealified[Y]: Moore[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] =
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

Mealy machines have a dedicated `run` method. A Moore machine forms a Mealy machine if the output is a function from input to output,
`A => B`. For example:

```scala
import cats.implicits.given  // for `mapAccumulate`
import dynamical.fsm.{Moore}
import polynomial.morphism.~>
import polynomial.`object`.{Monomial, Store}

def moore[Y]: Moore[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] =
  Moore[Boolean, Int, Int => Int, Y](
    false,
    s => x => if s then x + x else x,   // if we've seen x > 1, then emit 2x
    (s, x) => if x > 1 then true else s // change state upon seeing x > 1
  )
val m: Moore[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = moore.asMealy
val l: List[Int] = List(1, 2, 3).mapAccumulate(m.init)(m.run)._2
// l: List[Int] = List(1, 2, 6)
```


##### `Wiring[P[_]]`

Wirings, in contrast to state systems, are the interface systems that allow us
to represent interaction patterns.

For example, the composition of a state system with an "wrapper interface" of
`((Plant ⊗ Controller) ~> System)[Y]`:
  - on one hand, it is "filled" (or "loaded") by composition with a tensored state system
  - on the other hand, it "wraps" a tensored state system in an non-tensored interface
  - note that there is no delay, since we defined the controller to emit a runnable function

```scala
import cats.implicits.given
import dynamical.fsm.{Mealy, Moore, Wiring}
import polynomial.`object`.{Monomial, Store}
import polynomial.morphism.~>
import polynomial.product.⊗

type Plant[Y]      = Monomial[(Byte, Byte => Char), Char, Y]
type Controller[Y] = Monomial[Char, Byte => Char, Y]
type System[Y]     = Monomial[Byte, Byte => Char, Y]
type ω[Y] = ((Plant ⊗ Controller) ~> System)[Y]
val w: Wiring[ω] = Wiring(b => a => b._2(a), (b, a) => ((a, b._2), b._2(a)))
// w: Wiring[ω] = dynamical.fsm.Wiring$$anon$4@27683333
val m: Moore[(Store[Char, _] ⊗ Store[Byte => Char, _]) ~> (Plant ⊗ Controller)] =
  (Moore[Char, (Byte, Byte => Char), Char, Nothing](" ".charAt(0), identity, (s, i) => i._2(i._1))
    ⊗ Moore[Byte => Char, Char, Byte => Char, Nothing](b => b.toChar, identity, (f, i) => if i != ' ' then f else b => b.toChar.toUpper))
// m: Moore[~>[⊗[[_$5 >: Nothing <: Any] => Store[Char, _$5], [_$6 >: Nothing <: Any] => Store[Function1[Byte, Char], _$6]], ⊗[Plant, Controller]]] = dynamical.fsm.Moore$$anon$18@7e95b5b
val fsm: Mealy[((Store[Char, _] ⊗ Store[Byte => Char, _]) ~> (Plant ⊗ Controller) ~> System)] = m.andThen(w).asMealy
// fsm: Mealy[~>[~>[⊗[[_$7 >: Nothing <: Any] => Store[Char, _$7], [_$8 >: Nothing <: Any] => Store[Function1[Byte, Char], _$8]], ⊗[Plant, Controller]], System]] = dynamical.fsm.Moore$$anon$14@1f588454
val s: String = "hello world".getBytes().toList.mapAccumulate(fsm.init)(fsm.run)._2.mkString
// s: String = "hello WORLD"
```

(Note: example adapted from [Niu and Spivak](https://topos.site/poly-book.pdf))

## `dynamical-fs2`
 - libarary for Scala 3 (JS, JVM, and Native platforms)
 - depends on fs2 3.9
 
```scala
"com.julianpeeters" %% "dynamical-fs2" % "0.0.0"
```

The `dynamical-fs2` library provides fs2 integration, in the form of a stream
`transducer` pipe:

```scala
import dynamical.fsm.Mealy
import dynamical.stream.transducer
import fs2.Stream
import polynomial.morphism.~>
import polynomial.`object`.{Monomial, Store}

val m: Mealy[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
// m: Mealy[~>[[_$9 >: Nothing <: Any] => Store[Boolean, _$9], [_$10 >: Nothing <: Any] => Monomial[Int, Function1[Int, Int], _$10]]] = dynamical.fsm.Mealy$$anon$2@602c4b4c
val l: List[Int] = Stream(1, 2, 3).through(m.transducer).compile.toList
// l: List[Int] = List(2, 4, 6)
```