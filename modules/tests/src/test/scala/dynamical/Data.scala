package dynamical

// import dynamical.fsm.{Moore}
// // import dynamical.fsm.Mealy
// import polynomial.`object`.{Monomial, Store}

// // import dynamical.fsm.{ Moore}
// // import polynomial.functor.{Dir, Pos}
// import polynomial.morphism.{~>}
// import polynomial.`object`.Store
// import polynomial.morphism.PolyMap.{Phi, PhiSharp}
// import polynomial.functor.PolynomialFunctor
// import polynomial.functor.PolynomialFunctor
// import dynamical.fsm.Moore.{Input, Output}

// import dynamical.fsm.Moore.Input
// import dynamical.fsm.Moore.State

//


// case class Wrapper[Monomial[Int, Int => Int, _]]()

object Data
  // val inner1: Moore[Boolean, Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
  // val mealy1: Mealy[Boolean, Monomial[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
  // def moore1[S](init: S): Moore[Store[S, _] ~> Monomial[Int, Int => Int, _]] = Moore(init, s => i => i + i, (s, i) => s)
  // val outer1: Monomial[Int, Int => Int, _] = Monomial()
    
//     def moore[P[_], S, Y](
//       i: S,
//       r: Phi[Pos[Store[S, _], Y], Pos[P, Y]],
//       u: PhiSharp[Pos[Store[S, _], Y], Dir[P, Y], Dir[Store[S, _], Y]]
//     ): Moore[P, S, Y] =
//       new Moore[P, S, Y]:
//         // type State = S
//         // type In = A
//         // type Out = B
//         def η: PolyMap[Store[S, _], P, Y] =
//           new PolyMap[Store[S, _], P, Y]:
//             def φ: Phi[Pos[Store[S, _], Y], Pos[P, Y]] = r
//             // type Φ = S => Pos[P, Y]
//             // type `Φ#` = (S, Dir[P, Y]) => S
//             // def φ: Pos[Strore[S, _], Y] => Pos[P, Y] = r
//             def `φ#`:  PhiSharp[Pos[Store[S, _], Y], Dir[P, Y], Dir[Store[S, _], Y]] = u
//             // def φ: PolyMap.Phi[Pos[Store[S, _], Y], Pos[Monomial[A, B, _], Y]] = r
//             // def `φ#`: PolyMap.PhiSharp[Pos[Store[S, _], Y], Dir[Monomial[A, B, _], Y], Dir[Store[S, _], Y]] = u
//         def init: S = i
//         // def readout[Y]: State[(Store[S, _]) ~> (Monomial[A, B, _]), Y] => Input[(Store[S, _]) ~> (Monomial[A, B, _]), Y] = ???
//         def readout: Phi[Pos[Store[S, _], Y], Pos[P, Y]] = η.φ //r//.asInstanceOf[S => B]
//         def update: PhiSharp[Pos[Store[S, _], Y], Dir[P, Y], Dir[Store[S, _], Y]] = η.`φ#`

  
    // def polyMap[Y]: (Store[Boolean, _] ~> Monomial[Int, Int, _])[Y] =
    //   new PolyMap[
    //     Store[Boolean, _],
    //     Monomial[Int, Int, _],
    //     Y
    //   ] :
    //     def init: Boolean = true
    //     override def φ: Boolean => Int =
    //       b => if b then 1 else  0
    //     override def `φ#`: (Boolean, Int) => Boolean =
    //       (s, a) => if a.%(2) == 0 then s else !s

    // def moore1(
    //   i: Boolean,
    //   r: Boolean => Int => Int,
    //   u: (Boolean, Int) => Boolean
    // ): Moore[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] =
    //   new Moore[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]]:
    //     // type S = Boolean
    //     // type A = Int
    //     // type B = Int => Int
    //     def η[Y]: PolyMap[Store[Boolean, _], Monomial[Int, Int => Int, _], Y] =
    //       new PolyMap[Store[Boolean, _], Monomial[Int, Int => Int, _], Y]:
    //         def φ: PolyMap.Phi[Pos[Store[Boolean, _], Y], Pos[Monomial[Int, Int => Int, _], Y]] = r
    //         def `φ#`: PolyMap.PhiSharp[Pos[Store[Boolean, _], Y], Dir[Monomial[Int, Int => Int, _], Y], Dir[Store[Boolean, _], Y]] = u
    //     def init[Y]: Boolean = i
    //     // def readout[Y]: State[(Store[Boolean, _]) ~> (Monomial[Int, Int => Int, _]), Y] => Input[(Store[Boolean, _]) ~> (Monomial[Int, Int => Int, _]), Y] = ???
    //     def readout[Y]: Boolean => Int => Int = r
    //     def update[Y]: (Boolean, Int) => Boolean = u

    // def mealy1(
    //   i: Boolean,
    //   r: Boolean => Int => Int,
    //   u: (Boolean, Int) => Boolean
    // ): Mealy[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] =
    //   new Mealy[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]]:
    //     def η[Y]: PolyMap[Store[Boolean, _], Monomial[Int, Int => Int, _], Y] =
    //       new PolyMap[Store[Boolean, _], Monomial[Int, Int => Int, _], Y]:
    //         def φ: PolyMap.Phi[Pos[Store[Boolean, _], Y], Pos[Monomial[Int, Int => Int, _], Y]] = r
    //         def `φ#`: PolyMap.PhiSharp[Pos[Store[Boolean, _], Y], Dir[Monomial[Int, Int => Int, _], Y], Dir[Store[Boolean, _], Y]] = u
    //     def init[Y]: Boolean = i
    //     def readout[Y]: Boolean => (Int => Int) = r
    //     def update[Y]: (Boolean, Int) => Boolean = u
    //     def run[Y]: (Boolean, Int) => (Boolean, Int) = (s, a) => (u(s, a), r(s)(a))
