package pto

// Adapted from "Functional Programming in Scala", 1st Edition.

///////////////////////////////////

trait RNG {
  def nextInt: (RNG, Int)
}

///////////////////////////////////

object RNG {

  final case class Simple(seed: Long) extends RNG {
    def nextInt: (RNG,Int) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
      val nextRNG = Simple(newSeed) 
      val n = (newSeed >>> 16).toInt 
      (nextRNG, n) 
    }
  }

  /////////////////////////////////

  // We need to be quite careful not to skew the generator.
  // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
  // it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.
  def nonNegativeInt(rng: RNG): (RNG,Int) = {
    val (r,i) = rng.nextInt
    (r,if (i < 0) -(i + 1) else i)
  }

  // We generate an integer >= 0 and divide it by one higher than the
  // maximum. This is just one possible solution.
  def double(rng: RNG): (RNG, Double) = {
    val (r,i) = nonNegativeInt(rng)
    (r,i / (Int.MaxValue.toDouble + 1))
  }

  def boolean(rng: RNG): (RNG, Boolean) =
    rng.nextInt match { case (rng2,i) => (rng2,i%2==0) }

  def uniformDouble(min: Double, max: Double)(rng: RNG): (RNG,Double) = {
    val (r,d) = double(rng)
    val x = min + (d * ((max - min) + 1))
    (r,x)
  }

  def uniformInt(min: Int, max: Int)(rng: RNG): (RNG,Int) = {
    val (r,x) = uniformDouble(min,max)(rng)
    (r,x.toInt)
  }
}

// End ///////////////////////////////////////////////////////////////
