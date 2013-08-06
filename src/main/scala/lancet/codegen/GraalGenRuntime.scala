package lancet.codegen

object Math {
  final def abs(d: Double): Double = java.lang.Math.abs(d)
}

object LancetSystem {
  def arraycopy(a: Any, b: Int, c: Any, d: Int, e: Int) = System.arraycopy(a,b,c,d,e)
}

object Conversions {
  final def l2s(l: Long): String = java.lang.Long.toString(l)
  final def i2s(i: Int): String = java.lang.Integer.toString(i)
  final def d2s(d: Double): String = java.lang.Double.toString(d)
  final def f2s(f: Float): String = java.lang.Float.toString(f)
  final def b2s(b: Boolean): String = java.lang.Boolean.toString(b)
}