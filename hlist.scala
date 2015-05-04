import HList.HNil
import scala.util.{Either, Left, Right}

sealed abstract class HList

case object HNil extends HList

final case class ::[H, T <: HList](head: H, tail: T) extends HList {
  override final def toString = s"$head :: $tail"
}

object HList {
  type HNil = HNil.type

  implicit final class BedazzledHList[L <: HList](l: L) {

    def ::[H](h: H): H :: L = new ::(h, l)

    def map[P <: Poly](p: P)(implicit mapper: Mapper[P, L]): mapper.Out =
      mapper(l)

    def foldLeft[Z](z: Z, p: Poly)(implicit folder: LeftFolder[p.type, Z, L]): folder.Out =
      folder(z, l)

    def foldRight[Z](z: Z, p: Poly)(implicit folder: RightFolder[p.type, L, Z]): folder.Out =
      folder(l, z)

    def reduceRight(p: Poly)(implicit rr: RightReducer[p.type, L]): rr.Out =
      rr(l)
  }

}

trait Case[P <: Poly, A] {
  type Out

  def apply(a: A): Out
}

object Case {
  type Aux[P <: Poly, A, _Out] = Case[P, A] { type Out = _Out }

  final class PartiallyAppliedCase[P <: Poly, A] {
    def apply[B](f: A => B): Aux[P, A, B] = new Case[P, A] {
      type Out = B

      def apply(a: A) = f(a)
    }
  }
}

trait Poly {
  import Case.PartiallyAppliedCase

  def apply[A](a: A)(implicit c: Case[this.type, A]): c.Out = c(a) 

  def at[A]: PartiallyAppliedCase[this.type, A] = new PartiallyAppliedCase[this.type, A]

}

trait Mapper[P <: Poly, L <: HList] {
  type Out <: HList

  def apply(l: L): Out
}

object Mapper {
  type Aux[P <: Poly, L <: HList, _Out <: HList] = Mapper[P, L] { type Out = _Out }

  implicit def hnilMapper[P <: Poly]: Aux[P, HNil, HNil] = new Mapper[P, HNil] {
    type Out = HNil

    def apply(l: HNil) = HNil
  }

  implicit def hconsMapper[P <: Poly, H, T <: HList](implicit headCase: Case[P, H], tailMapper: Mapper[P, T]): Aux[P, H :: T, headCase.Out :: tailMapper.Out] =
    new Mapper[P, H :: T] {
      type Out = headCase.Out :: tailMapper.Out

      def apply(l: H :: T) = headCase(l.head) :: tailMapper(l.tail)
    }
}

trait LeftFolder[P <: Poly, Z, L <: HList] {
  type Out

  def apply(z: Z, l: L): Out
}

object LeftFolder {
  type Aux[P <: Poly, Z, L <: HList, _Out] = LeftFolder[P, Z, L] {
    type Out = _Out
  }

  implicit def hnilLeftFolder[P <: Poly, Z]: Aux[P, Z, HNil, Z] =
    new LeftFolder[P, Z, HNil] {
      type Out = Z

      def apply(z: Z, l: HNil) = z
    }

  implicit def hconsFolder[P <: Poly, Z, H, H2, T <: HList](implicit combine: Case.Aux[P, (Z, H), H2], tailFolder: LeftFolder[P, H2, T]): Aux[P, Z, H :: T, tailFolder.Out] =
    new LeftFolder[P, Z, H :: T] {
      type Out = tailFolder.Out

      def apply(z: Z, l: H :: T) = tailFolder(combine(z -> l.head), l.tail)
    }
}

object AsInt extends Poly {
  implicit def numeric[A](implicit A: Numeric[A]) = at[A](A.toInt)
  implicit val bool = at[Boolean](b => if (b) 1 else 0)
  implicit val string = at[String](_.size)
}

object AddSize extends Poly {
  implicit def default[A](implicit c: Case.Aux[AsInt.type, A, Int]) =
    at[(Int, A)](t => t._1 + c(t._2))
}

object Fun extends Poly {
  implicit val first = at[(Int, Int)](t => t._1)
  implicit val second = at[(Int, String)](t => s"${t._2}-${t._1}")
  implicit val third = at[(String, Boolean)](t => if (t._2) t._1.toList else List.empty)
  implicit val fourth = at[(List[Char], Boolean)](t => t._1)
}

trait Semigroup[A] {
  def combine(a1: A, a2: A): A
}

object Semigroup {
  implicit def listSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    def combine(la1: List[A], la2: List[A]) = la1 ::: la2
  }
}

object SequenceEithers extends Poly {
  implicit def default[A, B <: HList, H](implicit semiA: Semigroup[A]) =
    at[(Either[A, B], Either[A, H])]{ case (acc, cur) =>
      acc.fold[Either[A, H :: B]](
        accErr => Left(cur.fold(
          curErr => semiA.combine(accErr, curErr),
          _ => accErr)),
        accSucc => cur.right.map(_ :: accSucc))
    }
}

object SequenceEithersRight extends Poly {
  implicit def default[A, T <: HList, H](implicit semiA: Semigroup[A]) =
    at[(Either[A, H], Either[A, T])]{ case (cur, acc) =>
      acc.fold[Either[A, H :: T]](
        accErr => Left(cur.fold(
          curErr => semiA.combine(curErr, accErr),
          _ => accErr)),
        accSucc => cur.right.map(_ :: accSucc))
    }
}

trait RightReducer[P <: Poly, L <: HList] {
  type Out

  def apply(l: L): Out
}

object RightReducer {
  type Aux[P <: Poly, L <: HList, _Out] = RightReducer[P, L] { type Out = _Out }

  implicit def hsingleRR[P <: Poly, H]: Aux[P, H :: HNil, H] = new RightReducer[P, H :: HNil] {
    type Out = H

    def apply(l: H :: HNil) = l.head
  }

  implicit def hconsRR[P <: Poly, H, T <: HList, TOut](implicit tailRR: Aux[P, T, TOut], c: Case[P, (H, TOut)]): Aux[P, H :: T, c.Out] =
    new RightReducer[P, H :: T] {
      type Out = c.Out

      def apply(l: H :: T) = c(l.head, tailRR(l.tail))
    }
}

trait RightFolder[P <: Poly, L <: HList, Z] {
  type Out

  def apply(l: L, z: Z): Out
}

object RightFolder {
  type Aux[P <: Poly, L <: HList, Z, _Out] = RightFolder[P, L, Z] { type Out = _Out }

  implicit def hnilRf[P <: Poly, Z]: Aux[P, HNil, Z, Z] =
    new RightFolder[P, HNil, Z] {
      type Out = Z

      def apply(l: HNil, z: Z) = z
    }

  implicit def hconsRf[P <: Poly, T <: HList, H, Z, TOut](implicit  rf: Aux[P, T, Z, TOut], c: Case[P, (H, TOut)]): Aux[P, H :: T, Z, c.Out] =
    new RightFolder[P, H :: T, Z] {
      type Out = c.Out

      def apply(l: H :: T, z: Z) = c(l.head, rf(l.tail, z))
    }
}


object Playing {
  val boop = (1 :: "foo" :: HNil).map(AsInt)

  val beep = (1 :: "foo" :: true :: false :: HNil).foldLeft(0, AddSize)

  val beep3 = (1 :: "foo" :: true :: false :: HNil).foldLeft(0, Fun)

  type ErrorsOr[A] = Either[List[String], A]
  val e1: ErrorsOr[Int] = Right(3)
  //val e2: ErrorsOr[String] = Right("foo")
  val e2: ErrorsOr[String] = Left(List("blargh!"))
  //val e3: ErrorsOr[Boolean] = Right(false)
  val e3: ErrorsOr[Boolean] = Left(List("bleeeeergh!"))
  val e4: ErrorsOr[Boolean :: HNil] = Left(List("bleeeeergh!"))

  val hnilSucc: ErrorsOr[HNil] = Right(HNil)

  val blop = (e1 :: e2 :: e3 :: HNil).foldLeft(hnilSucc, SequenceEithers)

  val blopR = (e1 :: e2 :: e3 :: HNil).foldRight(hnilSucc, SequenceEithersRight)

  val blop2 = (e1 :: e2 :: e4 :: HNil).reduceRight(SequenceEithersRight)

  val barg = (1 :: 2 :: "bloop" :: HNil).reduceRight(AddSize)
}
