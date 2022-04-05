package pto

import cats._
import com.cra.figaro.language._

///////////////////////////////////

object FigaroImplicits {

  implicit val elementMonad = new Monad[Element] { 
    override def pure[A](x: A): Element[A] = Constant(x)
  
    override def flatMap[A, B](fa: Element[A])(f: A => Element[B]): Element[B] = 
      fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Element[Either[A,B]]): Element[B] = f(a).flatMap { 
      case Left(a) => tailRecM(a)(f)
      case Right(b) => Constant(b)
    }
  }    
}

// End ///////////////////////////////////////////////////////////////
