package pto

import cats.data.State

///////////////////////////////////

object Rand {

  def always[A](x: A): Rand[A] = 
    State.pure(x)

  val int: Rand[Int] = State( r => r.nextInt )
  def uniformInt(min: Int, max: Int): Rand[Int] = 
    State( r => RNG.uniformInt(min,max)(r) )

  val double: Rand[Double] = State( r => RNG.double(r) )
  def uniformDouble(min: Double, max: Double): Rand[Double] = 
    State( r => RNG.uniformDouble(min,max)(r) )

  /////////////////////////////////

  def sample[A](x: Rand[A], rng: RNG): A = 
    x.runA(rng).value
}

// End ///////////////////////////////////////////////////////////////
