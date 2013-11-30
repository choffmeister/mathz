package de.choffmeister.mathz

trait MonoidElement[T <: MonoidElement[T]] {
  def +(e2: T): T

  def monoid: Monoid[T]
}

trait Monoid[T <: MonoidElement[T]] {
  val zero: T
}

trait GroupElement[T <: GroupElement[T]] extends MonoidElement[T] {
  def -(e2: T): T = this + e2.inverse
  def inverse: T
}

trait Group[T <: GroupElement[T]] extends Monoid[T]

trait CyclicGroup[T <: GroupElement[T]] extends Group[T] {
  val generator: T

  def decompose(e: T): BigInt =
    if (e == generator) 1
    else decompose(e - generator) + 1
}

trait RingElement[T <: RingElement[T]] {
  def +(e2: T): T
  def *(e2: T): T
  def -(e2: T): T = this + e2.inverseadd

  def inverseadd: T

  def ring: Ring[T]
}

trait Ring[T <: RingElement[T]] {
  val zero: T
}

case class Polynomial[T <: FieldElement[T]](coefficients: List[T]) extends RingElement[Polynomial[T]] {
  def +(e2: Polynomial[T]) =
    Polynomial[T](coefficients.map(Some(_)).zipAll(e2.coefficients.map(Some(_)), None, None).map(c => c match {
      case (Some(c1), Some(c2)) => c1 + c2
      case (Some(c1), None) => c1
      case (None, Some(c2)) => c2
      case _ => throw new Exception("Impossible case")
    }))

  def *(e2: Polynomial[T]) = throw new Exception("Not yet implemented")

  def inverseadd = Polynomial[T](coefficients.map(_.inverseadd))
  def derive = coefficients match {
    case first :: rest => Polynomial[T](rest.zipWithIndex.map(c => c._1 * (c._2 + 1)))
    case _ => ring.zero
  }

  def ring = PolynomialRing[T]

  override def toString() = coefficients match {
    case Nil => "0x^0"
    case _ => coefficients.zipWithIndex.map(c => s"${c._1}x^${c._2}").mkString(" + ")
  }
}

case class PolynomialRing[T <: FieldElement[T]]() extends Ring[Polynomial[T]] {
  val zero = Polynomial[T](Nil)
}

trait FieldElement[T <: FieldElement[T]] extends RingElement[T] {
  def /(e2: T): T = this * e2.inversemul
  def *(alpha: Int): T = alpha match {
    case alpha if alpha > 0 => this + this * (alpha - 1)
    case alpha if alpha < 0 => this.inverseadd + this * (alpha + 1)
    case alpha => field.zero
  }
  def inversemul: T

  def field: Field[T]
}

trait Field[T <: FieldElement[T]] extends Ring[T] {
  val zero: T
  val one: T
}

case class Integer(value: BigInt) extends GroupElement[Integer] {
  def +(e2: Integer) = Integer(value + e2.value)
  def inverse = Integer(-value)

  def monoid = Integers
}

case object Integers extends Group[Integer] with CyclicGroup[Integer] {
  val zero = Integer(0)
  val generator = Integer(1)
}

case class RealNumber(value: Double) extends FieldElement[RealNumber] {
  def +(e2: RealNumber) = RealNumber(value + e2.value)
  def *(e2: RealNumber) = RealNumber(value * e2.value)

  def inverseadd = RealNumber(-value)
  def inversemul = RealNumber(1.0 / value)

  def ring = RealNumbers
  def field = RealNumbers
}

case object RealNumbers extends Field[RealNumber] {
  val zero = RealNumber(0.0)
  val one = RealNumber(1.0)
}

case class Function[A](value: A => A) extends MonoidElement[Function[A]] {
  def +(e2: Function[A]) = Function[A](a => this(e2(a)))

  def apply(a: A) = value(a)

  def monoid = Functions[A]
}

case class Functions[A]() extends Monoid[Function[A]] {
  val zero = Function[A](a => a)
  def id = zero
}

object Main extends App {
  println("Mathz")

  val f = Function[Int](_ * 2)
  val g = Function[Int](_ + 10)

  println("f(2) = " + f(2))
  println("g(2) = " + g(2))
  println("(f + g)(2) = " + (f + g)(2))
  println("(g + f)(2) = " + (g + f)(2))
  println("(g + f + id)(2) = " + (g + f + Functions[Int].zero)(2))

  val p1 = Polynomial[RealNumber](List(RealNumber(1.0)))
  val p2 = Polynomial[RealNumber](List(RealNumber(0.0), RealNumber(4.0), RealNumber(10.0)))
  println("p1: " + p1)
  println("p2: " + p2)
  println("p1 + p2: " + (p1 + p2))
  println("p2': " + p2.derive)
  println("p2'': " + p2.derive.derive)
  println("p2''': " + p2.derive.derive.derive)
}
