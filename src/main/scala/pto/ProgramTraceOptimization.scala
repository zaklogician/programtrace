package pto

///////////////////////////////////

import cats._
import cats.data._
import cats.free._

import Matrix._
import cats.instances.UUIDInstances

import FigaroImplicits._
import com.cra.figaro.language._
import com.cra.figaro.library._

///////////////////////////////////

final case class UID(value: Int)

object UID {
  def next(x: UID): UID = UID(x.value+1)
}

///////////////////////////////////

object ProgramTraceOptimization {

  // https://typelevel.org/cats/datatypes/freemonad.html

  sealed trait SpecF[+A]
  final case class Const[A](x: A) extends SpecF[A]
  final case class IntRange(min: Int, max: Int) extends SpecF[Int]  

  type Spec[A] = Free[SpecF, A]

  /////////////////////////////////

  val initialValue: SpecF ~> Id = new (SpecF ~> Id) {
    override def apply[A](fa: SpecF[A]): Id[A] = fa match {
      case Const(x) => x
      case IntRange(min,max) => ( min + max ) / 2
    }
  }

  val pureElement: SpecF ~> Element = new (SpecF ~> Element) {
    override def apply[A](fa: SpecF[A]): Element[A] = fa match {
      case Const(x) => Constant(x)
      case IntRange(min,max) => atomic.discrete.Uniform((min to max):_*)
      // case DoubleRange(min,max) => com.cra.figaro.library.atomic.continuous.Uniform(min,max)
    }
  }

  /////////////////////////////////

  def uniformDouble(rng: scala.util.Random)(min: Double, max: Double): Double = {
    val d = rng.nextDouble()
    min + (d * ((max - min) + 1))
  }

  def uniformInt(rng: scala.util.Random)(min: Int, max: Int): Int = 
    uniformDouble(rng)(min,max).toInt

  def impureRandom(rng: scala.util.Random): SpecF ~> Id = new (SpecF ~> Id) {
    override def apply[A](fa: SpecF[A]): Id[A] = fa match { 
      case Const(value) => value
      case IntRange(min,max) => uniformInt(rng)(min,max)
    }
  }  

  /////////////////////////////////

  final case class NamedSpecF[A](uid: UID, spec: Spec[A]) { 
    def map[B](f: A => B): NamedSpecF[B] = 
      NamedSpecF(uid, spec.map { f })
  }

  type NamedSpecM[A] = Free[NamedSpecF, A]

  val impureNameSpec: SpecF ~> NamedSpecM = new (SpecF ~> NamedSpecM) {

    var uid_ = UID(0)

    override def apply[A](fa: SpecF[A]): NamedSpecM[A] = {
      uid_ = UID.next(uid_)
      Free.liftF( NamedSpecF(uid_,Free.liftF(fa) )  )
    }
  }  

  /////////////////////////////////

  final case class TraceValue(value: Element[Any], spec: NamedSpecF[Any])  
  type Trace = Map[UID, TraceValue]
  type TraceStateM[A] = cats.data.State[Trace,A]

  /////////////////////////////////  

  val makeTrace: NamedSpecF ~> TraceStateM = new (NamedSpecF ~> TraceStateM) {
    override def apply[A](fa: NamedSpecF[A]): TraceStateM[A] = {
      val value = fa.spec.foldMap { initialValue }      
      val element = fa.spec.foldMap { pureElement }.asInstanceOf[Element[Any]]
      for { 
        uid <- State.get[Trace].map { x => UID( x.size + 1 ) }
        _ <- State.modify[Trace] { x => x.updated( uid, TraceValue(element, fa.asInstanceOf[NamedSpecF[Any]]) ) }
        current <- State.get[Trace]
      } yield { value }
    }
  }    

  /////////////////////////////////

  def playback(trace: Trace): NamedSpecF ~> Element = new (NamedSpecF ~> Element) {
    override def apply[A](fa: NamedSpecF[A]): Element[A] = {
      val x = trace.get(fa.uid)
      x match {
        case Some(TraceValue(value, _)) => value.asInstanceOf[Element[A]]
        case None => fa.spec.foldMap { pureElement } // FIXME: should output the updated trace also
      }
    }
  }

  def impureMutate(t: Trace, rng: scala.util.Random): Trace = { 
    val l = t.toList
    val (uid,traceValue) = l(uniformInt(rng)(0, l.size-1))
    val newValue = traceValue.spec.spec.foldMap { initialValue }
    
    t.updated(uid, traceValue.copy(value=Constant(newValue) ) )
  }

  /////////////////////////////////

  def intRange(min: Int, max: Int): Spec[Int] =
    cats.free.Free.liftF[SpecF, Int]( IntRange(min,max) )

  def sizedMatrix(rows: Int, cols: Int): Spec[Matrix[Int]] = {
    val m = Matrix.tabulate(rows,cols) { case _ => intRange(1,15) }
    cats.Traverse[Matrix].sequence( m )
  }

  val myMatrixSpec: Spec[Matrix[Int]] = for { 
    s <- intRange(1,5)
    m <- sizedMatrix(s,s)
  } yield m

  /////////////////////////////////

  def mapletFormat[T,U](x: (T,U)): String =
    s"${x._1} -> ${x._2}"

  def main(args: Array[String]): Unit = {

    val seed = 0xDEADBEEF
    val rng = new scala.util.Random(seed)

    ///////////////////////////////

    val namedSpec = myMatrixSpec.foldMap { impureNameSpec }

    println(namedSpec) 

    for ( i <- 0 until 4 ) {
      val (trace,matrix) = namedSpec.foldMap { makeTrace }.run(Map()).value
      println( trace.toList.sortBy { _._1.value }.map { mapletFormat }.mkString("\n") )
      println( matrix )    

      val elem = namedSpec.foldMap { playback( trace ) }
      println( elem.generateValue( elem.generateRandomness() ) )

      (0 until 3).foldLeft(trace) { case (t,_) =>
        val newTrace = impureMutate(t, rng)
        val elem = namedSpec.foldMap { playback( newTrace ) }
        println( elem.generateValue( elem.generateRandomness() ) )
        newTrace
      }
    }
  }
}

// End ///////////////////////////////////////////////////////////////
