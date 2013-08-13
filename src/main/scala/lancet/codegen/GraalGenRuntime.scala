package lancet.codegen

object Math {
  final def abs(d: Double): Double = java.lang.Math.abs(d)
}

object LancetSystem {
  final def arraycopy(a: Any, b: Int, c: Any, d: Int, e: Int) = System.arraycopy(a,b,c,d,e)
}

object LancetString {
  final def valueOf(s: String) = java.lang.String.valueOf(s)
}
