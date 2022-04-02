package pto

import cats._
import cats.free._

///////////////////////////////////

object FreeMonadTest {

  // https://typelevel.org/cats/datatypes/freemonad.html

  sealed trait KVStoreA[A]
  case class Put[T](key: String, value: T) extends KVStoreA[Unit]
  case class Get[T](key: String) extends KVStoreA[Option[T]]
  case class Delete(key: String) extends KVStoreA[Unit]

  type KVStore[A] = Free[KVStoreA, A]  

  /////////////////////////////////

  def put[T](key: String, value: T): KVStore[Unit] =
    cats.free.Free.liftF[KVStoreA, Unit]( Put[T](key, value) )

  def get[T](key: String): KVStore[Option[T]] =
    cats.free.Free.liftF[KVStoreA, Option[T]]( Get[T](key) )

  def delete(key: String): KVStore[Unit] =
    cats.free.Free.liftF( Delete(key) )

  def update[T](key: String, f: T => T): KVStore[Unit] = for {
    vMaybe <- get[T](key)
    _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
  } yield ()  

  /////////////////////////////////

  import cats.arrow.FunctionK
  import cats.{Id, ~>}
  import scala.collection.mutable

  import cats.data.State

  type KVStoreState[A] = State[Map[String, Any], A]

  val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
    def apply[A](fa: KVStoreA[A]): KVStoreState[A] = fa match {
      case Put(key, value) => State.modify( _.updated(key, value) )
      case Get(key) => State.inspect( _.get(key).map(_.asInstanceOf[A]) )
      case Delete(key) => State.modify( _ - key )
    }
  }

  /////////////////////////////////
    
  def main(args: Array[String]): Unit = {

    def program: KVStore[Option[Int]] = for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", (_ + 12))
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n

    val result: (Map[String, Any], Option[Int]) = 
      program.foldMap(pureCompiler).run(Map.empty).value

    println( result )
    println( "All done." )
  }
}


// End ///////////////////////////////////////////////////////////////
