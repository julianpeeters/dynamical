# dynamical
Based on the dependent lenses described in [Niu and Spivak](https://topos.site/poly-book.pdf)

### Modules
 - [`dynamical-fsm`](#dynamical-fsm): composable finite state machines
 - [`dynamical-fs2`](#dynamical-fs2): integration with [fs2](https://fs2.io/#/) concurrent streams

## `dynamical-fsm`
 - libarary for Scala @SCALA@ (JS, JVM, and Native platforms)
 - depends on polynomial @POLYNOMIAL@ (and, internally, destructured @DESTRUCTURED@)
 
```scala
"com.julianpeeters" %% "dynamical-fsm" % "@VERSION@"
```

The `dynamical-fsm` library provides the components of finite state machines:
 - `Moore[P[_]]`: "simple and composable"
 - `Mealy[P[_]]`: "composable and runnable"
 - `Wiring[P[_]]`: "adaptable and abstractable"

##### `Moore[P[_]]`

The mose basic finite state machines are those parameterized by a polymap from
a store to a monomial:

```scala mdoc
import polynomial.`object`.{Monomial, Store}
import polynomial.morphism.~>
import dynamical.fsm.Moore

type Fsm[S, A, B] = Moore[Store[S, _] ~> Monomial[A, B, _]]
```

However, in order to run them as a function `(S, A) => (S, B)`, we need the
output `B` to be a function from input to output, `A => B`. For example:

```scala mdoc:reset
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
```

##### `Mealy[P[_]]`

Mealy machines have a dedicated `run` method. A Moore machine forms a Mealy machine if the output is a function from input to output,
`A => B`. For example:

```scala
import cats.implicits.given
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

For example, the composition of a state system with in interface system of
`((Plant ⊗ Controller) ~> System)[Y]`:
  - on one hand, it is "filled" (or "loaded") by composition with a tensored state system
  - on the other hand, it "wraps" a tensored state system in an non-tensored interface
  - finally, delay is controlled by setting controller state to emit a runnable function

```scala mdoc:reset
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
val m: Moore[(Store[Char, _] ⊗ Store[Byte => Char, _]) ~> (Plant ⊗ Controller)] =
  (Moore[Char, (Byte, Byte => Char), Char, Nothing](" ".charAt(0), identity, (s, i) => i._2(i._1))
    ⊗ Moore[Byte => Char, Char, Byte => Char, Nothing](b => b.toChar, identity, (f, i) => if i != ' ' then f else b => b.toChar.toUpper))
val fsm: Mealy[((Store[Char, _] ⊗ Store[Byte => Char, _]) ~> (Plant ⊗ Controller) ~> System)] = m.andThen(w).asMealy
val s: String = "hello world".getBytes().toList.mapAccumulate(fsm.init)(fsm.run)._2.mkString
```

(Note: example adapted from [Niu and Spivak](https://topos.site/poly-book.pdf))

## `dynamical-fs2`
 - libarary for Scala @SCALA@ (JS, JVM, and Native platforms)
 - depends on fs2 @FS2@
 
```scala
"com.julianpeeters" %% "dynamical-fs2" % "@VERSION@"
```

The `dynamical-fs2` library provides a `transducer for `fs2 streaming integration:

```scala mdoc:reset
import dynamical.fsm.{Mealy, transducer}
import fs2.Stream
import polynomial.morphism.~>
import polynomial.`object`.{Monomial, Store}

val m: Mealy[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
val l: List[Int] = Stream(1, 2, 3).through(m.transducer).compile.toList
```