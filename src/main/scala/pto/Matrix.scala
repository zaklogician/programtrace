package pto

///////////////////////////////////

import cats._

///////////////////////////////////

case class Matrix[T](elems: List[List[T]]) { 
  def toList = elems.flatten

  def map[U](f: T => U): Matrix[U] = 
    Matrix(elems.map(xs => xs.map{f}))

  override def toString(): String = 
    elems.map { _.mkString("[",",","]") }.mkString("\n[","\n","]")
}

///////////////////////////////////

object Matrix { 

  def tabulate[T](rows: Int, cols: Int)( f: (Int,Int) => T): Matrix[T] =
    Matrix( List.tabulate(rows,cols) { f } )

  /////////////////////////////////

  implicit val matrixFoldable: cats.Foldable[Matrix] = new cats.Foldable[Matrix] {  
    override def foldLeft[A, B](fa: Matrix[A],b: B)(f: (B, A) => B): B =
      fa.toList.foldLeft(b) { f }

    override def foldRight[A, B](fa: Matrix[A],lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = 
      fa.toList.foldRight(lb) { f }
  }

  implicit val matrixTraverse: Traverse[Matrix] = new Traverse[Matrix] {
    override def traverse[G[_]: Applicative, A, B](fa: Matrix[A])(f: A => G[B]): G[Matrix[B]] = {
      val yy = cats.Traverse[List].sequence( fa.map( f ).elems.map { zs => cats.Traverse[List].sequence(zs) } )
      cats.Applicative[G].fmap( yy ) { Matrix(_) }
    }

    override def foldLeft[A, B](fa: Matrix[A],b: B)(f: (B, A) => B): B = 
      matrixFoldable.foldLeft(fa,b)(f)

    override def foldRight[A, B](fa: Matrix[A],lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = 
      matrixFoldable.foldRight(fa,lb)(f)
  }  
}

// End ///////////////////////////////////////////////////////////////
