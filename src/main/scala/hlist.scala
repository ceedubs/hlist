package hlist

import HList.HNil

sealed abstract class HList

case object HNil extends HList

final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
  override def toString = s"$head :: $tail"
}

object HList {
  type HNil = hlist.HNil.type

  final implicit class HListOps[L <: HList](val l: L) extends AnyVal {
    def ::[H](h: H): HCons[H, L] = HCons(h, l)

    def map[F <: HFunction](implicit m: Mapper[F, L]): m.Out = m.apply(l)
  }
}

trait Mapper[F <: HFunction, L <: HList] {
  type Out <: HList

  def apply(l: L): Out
}

object Mapper {
  type Aux[F <: HFunction, L <: HList, O <: HList] = Mapper[F, L] {
    type Out = O
  }

  implicit def mapperHNil[F <: HFunction]: Aux[F, HNil, HNil] = new Mapper[F, HNil] {
    type Out = HNil

    def apply(l: HNil) = HNil
  }

  implicit def mapperHCons[F <: HFunction, H, T <: HList](implicit caseH: Case[F, H], mapperT: Mapper[F, T]): Aux[F, H :: T, caseH.Out :: mapperT.Out] = new Mapper[F, H :: T] {
    type Out = caseH.Out :: mapperT.Out

    def apply(l: H :: T) = caseH(l.head) :: mapperT(l.tail)
  }
}

trait Case[F <: HFunction, A] {
  type Out

  def apply(a: A): Out
}

object Case {
  type Aux[F <: HFunction, A, B] = Case[F, A] {
    type Out = B
  }

  def apply[F <: HFunction, A, B](f: A => B): Aux[F, A, B] = new Case[F, A] {
    type Out = B

    def apply(a: A) = f(a)
  }
}

trait HFunction {
  def at[A, B](f: A => B): Case.Aux[this.type, A, B] = Case(f)
}

object Foo extends HFunction {
  implicit val atInt = at[Int, Double](_ * 2.5)
  implicit val atString = at[String, Option[Char]](_.headOption)
}
