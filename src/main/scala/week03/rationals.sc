val x = new Rational(1, 3)
x.numer
x.denom

val y = new Rational(5, 7)

x + y

-x

x - y

val z = new Rational(3, 2)

x - y - z

y.+(y)
y + y

x.<(y)
x < y

x.max(y)
x max y

new Rational(2)

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int) : Int = if ( b == 0 ) a else gcd(b, a % b)
  private def g = gcd(x, y)

  val numer = x / g
  val denom = y / g

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this < that) that else this

  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def unary_- : Rational =
    new Rational(-numer, denom)

  def - (that: Rational) = this + -that

  override def toString: String = numer + "/" + denom
}