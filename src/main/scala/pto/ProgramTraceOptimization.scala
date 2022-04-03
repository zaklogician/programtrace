package pto

///////////////////////////////////

import cats._
import cats.data._
import cats.implicits._
import cats.free._

import Rand._
import Matrix._

///////////////////////////////////

object ProgramTraceOptimization {

  // https://typelevel.org/cats/datatypes/freemonad.html

  sealed trait SpecF[A]

  final case class Const[A](x: A) extends SpecF[A]
  final case class IntRange(min: Int, max: Int) extends SpecF[Int]  
  final case class DoubleRange(min: Double, max: Double) extends SpecF[Double]

  type Spec[A] = Free[SpecF, A]  

  /////////////////////////////////

  val randomChoice: SpecF ~> Rand = new (SpecF ~> Rand) {
    override def apply[A](fa: SpecF[A]): Rand[A] = fa match {
      case Const(x) => Rand.always(x)
      case IntRange(min,max) => Rand.uniformInt(min,max)
      case DoubleRange(min,max) => Rand.uniformDouble(min,max)
    }
  }

  def trace(rng: RNG): SpecF ~> Trace = new (SpecF ~> Trace) {
    override def apply[A](fa: SpecF[A]): Trace[A] = {
      val chosen = Rand.sample(randomChoice(fa),rng)
      Trace.Named(chosen,Trace.uid())
    }
  }

  /////////////////////////////////

  type TraceElem = scala.collection.immutable.Map[String, Any]
  type TraceState[A] = State[TraceElem, A]

  def updateState[K,V,A](key: K, value: V): State[Map[K,V],A] =
    State.modify { (e: Map[K,V]) => e.updated( key, value ) }.inspect { (e: Map[K,V]) => e(key).asInstanceOf[A] } 

  def uniformDouble(rng: scala.util.Random)(min: Double, max: Double): Double = {
    val d = rng.nextDouble()
    min + (d * ((max - min) + 1))
  }

  def uniformInt(rng: scala.util.Random)(min: Int, max: Int): Int = 
    uniformDouble(rng)(min,max).toInt

  def trace2(rng: scala.util.Random): SpecF ~> TraceState = new (SpecF ~> TraceState) {
    override def apply[A](fa: SpecF[A]): TraceState[A] = {
      val key = Trace.uid()
      fa match {
        case Const(value) => updateState( key, value )
        case IntRange(min,max) => 
          val value = uniformInt(rng)(min,max)
          updateState( key, value )            
        case DoubleRange(min,max) => 
          val value = uniformDouble(rng)(min,max)
          updateState( key, value )
      }
    }
  }  

  /////////////////////////////////

  def intRange(min: Int, max: Int): Spec[Int] =
    cats.free.Free.liftF[SpecF, Int]( IntRange(min,max) )

  def sizedMatrix(rows: Int, cols: Int): Spec[Matrix[Int]] = {
    val m = Matrix.tabulate(rows,cols) { case _ => intRange(1,15) }
    cats.Traverse[Matrix].sequence( m )
  }

  def matrix: Spec[Matrix[Int]] = for { 
    s <- intRange(1,5)
    m <- sizedMatrix(s,s)
  } yield m

  /////////////////////////////////

  def main(args: Array[String]): Unit = {

    val seed = 0xDEADBEEF
    val rng = RNG.Simple(seed)

    val generator: Rand[Matrix[Int]] = matrix.foldMap { randomChoice } 
    println( Rand.sample(generator, rng) )

    // FIXME: use pure rng:
    val scalaRNG = new scala.util.Random(seed)
    val traced = matrix.foldMap { trace2(scalaRNG) } 
    println( traced.run(Map()).value )
  }
}

// End ///////////////////////////////////////////////////////////////
