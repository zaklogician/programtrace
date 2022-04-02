package pto

///////////////////////////////////

import cats._
import cats.implicits._
import cats.free._

import Rand._
import Matrix._

///////////////////////////////////

object ProgramTraceOptimization {

  // https://typelevel.org/cats/datatypes/freemonad.html

  sealed trait ChoiceF[A]
  final case class Const[A](x: A) extends ChoiceF[A]    
  final case class IntRange(min: Int, max: Int) extends ChoiceF[Int]  
  final case class DoubleRange(min: Double, max: Double) extends ChoiceF[Double]

  type Choice[A] = Free[ChoiceF, A]  

  /////////////////////////////////

  val randomChoice: ChoiceF ~> Rand = new (ChoiceF ~> Rand) {
    override def apply[A](fa: ChoiceF[A]): Rand[A] = fa match {
      case Const(x) => Rand.always(x)
      case IntRange(min,max) => Rand.uniformInt(min,max)
      case DoubleRange(min,max) => Rand.uniformDouble(min,max)      
    }
  }

  /////////////////////////////////

  sealed trait Trace[A]
  final case class Named[A](choice: ChoiceF[A], name: String) extends Trace[A]
  // ^ naive attempt to generalize the standard PTO naming scheme

  val trace: ChoiceF ~> Trace = new (ChoiceF ~> Trace) {
    def apply[A](fa: ChoiceF[A]): Trace[A] = {
      val st = new Throwable().getStackTrace()
      Named(fa,st.mkString(","))
    }
  }

  /////////////////////////////////

  def intRange(min: Int, max: Int): Choice[Int] =
    cats.free.Free.liftF[ChoiceF, Int]( IntRange(min,max) )

  def matrixSized(rows: Int, cols: Int): Choice[Matrix[Int]] = 
    Matrix.tabulate(rows,cols) { case _ => intRange(1,15) }.sequence

  def matrix: Choice[Matrix[Int]] = for { 
    s <- intRange(1,5)
    m <- matrixSized(s,s)
  } yield m

  /////////////////////////////////

  def main(args: Array[String]): Unit = {

    import cats.arrow.FunctionK
    import cats.{Id, ~>}

    val rng = RNG.Simple(0xDEADBEEF)

    val generator = matrix.foldMap { randomChoice } 
    println( Rand.sample(generator, rng) )

    // Can't do this because Trace is not yet a monad:
    // val traced = matrix.foldMap { trace } 
    // println( traced )
  }
}

// End ///////////////////////////////////////////////////////////////
