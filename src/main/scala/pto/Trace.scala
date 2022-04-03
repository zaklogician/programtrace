package pto

sealed trait Trace[A]

///////////////////////////////////

object Trace {

  import ProgramTraceOptimization._

  final case class Named[A](
    value: A, 
    uid: String
  ) extends Trace[A]

  /////////////////////////////////

  private var uid_ = 0

  def uid(): String = { 
    val result = uid_
    uid_ += 1
    return s"uid${result}"
  }

  /////////////////////////////////  

  implicit val traceMonad = new cats.Monad[Trace] {

    override def pure[A](x: A): Trace[A] = Trace.Named(x,uid())

    override def flatMap[A, B](fa: Trace[A])(f: A => Trace[B]): Trace[B] = fa match {
      case Trace.Named(a,name) => f(a)
      // FIXME: not really monadic - sort this out
    }

    @scala.annotation.tailrec    
    override def tailRecM[A, B](a: A)(f: A => Trace[Either[A,B]]): Trace[B] = f(a) match { 
      case Trace.Named(Right(b),name) => Trace.Named(b,name)
      case Trace.Named(Left(a),name) => tailRecM(a)(f)
    }
  }
}

// End ///////////////////////////////////////////////////////////////
