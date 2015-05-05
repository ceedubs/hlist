package hlist

import HList.HNil

sealed abstract class HList

case object HNil extends HList

final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
  override def toString = s"$head :: $tail"
}

object HList {
  type HNil = HNil.type

  implicit final class HListOps[L <: HList](l: L) {
    def ::[H](h: H): HCons[H, L] = HCons(h, l)

    def map[P <: Poly](implicit f: Mapper[P, L]): L = f(l)
  }
}

trait Mapper[P <: Poly, L <: HList] {
  def apply(l: L): L
}

object Mapper {
  implicit def mapperHNil[P <: Poly]: Mapper[P, HNil] = new Mapper[P, HNil] {
    def apply(l: HNil) = HNil
  }

  implicit def mapperHCons[P <: Poly, H, T <: HList](implicit caseH: Case[P, H], mapperT: Mapper[P, T]): Mapper[P, H :*: T] = new Mapper[P, H :*: T] {
    def apply(l: H :*: T) = caseH(l.head) :: mapperT(l.tail)
  }
}

trait Case[P <: Poly, A] {
  def apply(a: A): A
}

object Case {
  def apply[P <: Poly, A](f: A => A): Case[P, A] = new Case[P, A] {
    def apply(a: A) = f(a)
  }
}

trait Poly

object Foo extends Poly {
  implicit val caseInt: Case[Foo.type, Int] = Case(_ + 1)

  implicit val caseString: Case[Foo.type, String] = Case(_ + "-modified")
}

object Identity extends Poly {
   implicit def atA[A]: Case[Identity.type, A] = Case(identity)
}
