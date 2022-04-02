package pto

// import breeze.linalg._
import breeze.stats.distributions._
import cats._
import cats.implicits._

///////////////////////////////////

case class Matrix[T](elems: List[List[T]]) { 
  def toList = elems.flatten

  def map[U](f: T => U): Matrix[U] = ???
}

object Matrix { 

  def tabulate[T](rows: Int, cols: Int)( f: (Int,Int) => T): Matrix[T] = ???

  /////////////////////////////////

  implicit val matrixFoldable: cats.Foldable[Matrix] = new cats.Foldable[Matrix] {  
    override def foldLeft[A, B](fa: Matrix[A],b: B)(f: (B, A) => B): B =
      fa.toList.foldLeft(b) { f }

    override def foldRight[A, B](fa: Matrix[A],lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = 
      fa.toList.foldRight(lb) { f }
  }

  implicit val matrixTraverse: Traverse[Matrix] = new Traverse[Matrix] {
    override def traverse[G[_]: Applicative, A, B](fa: Matrix[A])(f: A => G[B]): G[Matrix[B]] = {
      val zss = fa.map { f }.elems
      val yy = cats.Traverse[List].sequence( zss.map { zs => cats.Traverse[List].sequence(zs) } )
      cats.Applicative[G].fmap( yy ) { Matrix(_) }
    }

    override def foldLeft[A, B](fa: Matrix[A],b: B)(f: (B, A) => B): B = 
      matrixFoldable.foldLeft(fa,b)(f)

    override def foldRight[A, B](fa: Matrix[A],lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = 
      matrixFoldable.foldRight(fa,lb)(f)
  }  
}

///////////////////////////////////

trait Trace[T] {
  def value: T
}

object Trace {
  val traceInt: Trace[Int] = ???
}

///////////////////////////////////

object ProgramTraceOptimization {

  /*****
  implicit val randApplicative = new cats.Applicative[Rand] {
    override def pure[A](x: A): Rand[A] = Rand.always(x)
    override def ap[A, B](ff: Rand[A=>B])(fa: Rand[A]): Rand[B] =
      for { f <- ff; a <- fa } yield f(a)
  }

  /////////////////////////////////

  def generator: Rand[Matrix[Int]] =
    Rand.randInt(1,5).flatMap { s => 
      Matrix.tabulate(s,s) { case _ => Rand.randInt(1,15) }.sequence
    }

  def main(args: Array[String]): Unit = {
    println( generator.sample() )
  }
  ************/  
}

// End ///////////////////////////////////////////////////////////////
