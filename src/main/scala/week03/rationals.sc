val x = new Rational(1, 3)
x.numer
x.denom

val y = new Rational(5, 7)

x.add(y)

x.neg()

x.subtract(y)

val z = new Rational(3, 2)

x.subtract(y).subtract(z)

y.add(y)

x.less(y)

x.max(y)

new Rational(2)

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int) : Int = if ( b == 0 ) a else gcd(b, a % b)
  private def g = gcd(x, y)

  val numer = x / g
  val denom = y / g

  def less(that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this.less(that)) that else this

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def neg():Rational =
    new Rational(-numer, denom)

  def subtract(that: Rational) = add(that.neg())

  override def toString: String = numer + "/" + denom
}