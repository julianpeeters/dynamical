# dynamical
Based on the dependent lenses described in [Niu and Spivak](https://topos.site/poly-book.pdf)

### Add the dependency:
 - libarary for Scala 3 (JS, JVM, and Native platforms)
 - depends on polynomial 0.1 (and, internally, destructured 0.2)
 
```scala
"com.julianpeeters" %% "dynamical" % "0.0.0"
```

### Modules
 - [`dynamical-fsm`](#dynamical-fsm)
 - [`dynamical-fs2`](#dynamical-fs2)

#### `dynamical-fsm`

The `dynamical-fsm` library provides the components of finite state machines:
 - `Moore[P[_]]`: "simple and composable"
 - `Mealy[P[_]]`: "composable and runnable"
 - `Wiring[P[_]]`: "adaptable and abstractable"

##### `Moore[P[_]]`

The simplest, and therefore most straightforward, finite state machines are
parameterized by a polymap from store to a monomial:

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
// w: Wiring[ω] = dynamical.fsm.Wiring$$anon$4@32af19a4
val m: Moore[(Store[Char, _] ⊗ Store[Byte => Char, _]) ~> (Plant ⊗ Controller)] =
  (Moore[Char, (Byte, Byte => Char), Char, Nothing](" ".charAt(0), identity, (s, i) => i._2(i._1))
    ⊗ Moore[Byte => Char, Char, Byte => Char, Nothing](b => b.toChar, identity, (f, i) => if i != ' ' then f else b => b.toChar.toUpper))
// m: Moore[~>[⊗[[_$5 >: Nothing <: Any] => Store[Char, _$5], [_$6 >: Nothing <: Any] => Store[Function1[Byte, Char], _$6]], ⊗[Plant, Controller]]] = dynamical.fsm.Moore$$anon$18@17e11e03
val fsm: Mealy[((Store[Char, _] ⊗ Store[Byte => Char, _]) ~> (Plant ⊗ Controller) ~> System)] = m.andThen(w).asMealy
// fsm: Mealy[~>[~>[⊗[[_$7 >: Nothing <: Any] => Store[Char, _$7], [_$8 >: Nothing <: Any] => Store[Function1[Byte, Char], _$8]], ⊗[Plant, Controller]], System]] = dynamical.fsm.Moore$$anon$14@2ee069a8
val s: String = "hello world".getBytes().toList.mapAccumulate(fsm.init)(fsm.run)._2.mkString
// s: String = "hello WORLD"
```

(Note: example adapted from [Niu and Spivak](https://topos.site/poly-book.pdf))


#### `dynamical-fs2`

