package lancet.codegen

import org.objectweb.asm._

class FunctionTemplate(
  className: String,
  argMfs: List[Manifest[_]],
  val retMf: Manifest[_],
  val locals: Int,
  val stackSize: Int
) extends Opcodes {
  val args = argMfs.map(_.toString)

  val ret  = retMf.toString

  private def javaType(tpString: String) = tpString match {
    case "Byte"      => "Ljava/lang/Byte;"
    case "Short"     => "Ljava/lang/Short;"
    case "Int"       => "Ljava/lang/Int;"
    case "Long"      => "Ljava/lang/Long;"
    case "Double"    => "Ljava/lang/Double;"
    case "Float"     => "Ljava/lang/Float;"
    case "Char"      => "Ljava/lang/Char;"
    case "Boolean"   => "Ljava/lang/Boolean;"
    case _           => "Ljava/lang/Object;"
  }

  private def shortType(tpString: String): String = {
    val Arr = "Array\\[(.*)\\]".r
    tpString match {
      case "Byte"      => "B"
      case "Short"     => "S"
      case "Int"       => "I"
      case "Long"      => "J"
      case "Double"    => "D"
      case "Float"     => "F"
      case "Char"      => "C"
      case "Boolean"   => "Z"
      case "Unit"      => "V"
      case Arr(v)      => "[" + javaType(v)
      case _           => "O"
    }
  }

  private def unboxType(tpString: String) = "unboxTo" + tpString
  private def boxType(tpString: String) = "boxTo" + tpString

  def bytecode(): Array[Byte] = {
    val cw = new ClassWriter(0)
    var mv: MethodVisitor = null


    cw.visit(Opcodes.V1_6, Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_SUPER,
      className,
      "Lscala/runtime/AbstractFunction" + args.size + "<" + (args ::: List(ret)).map(x => javaType(x)) + ">;",
      "scala/runtime/AbstractFunction" + args.size,
      null
    )

    {
      mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)
      mv.visitCode()
      mv.visitVarInsn(Opcodes.ALOAD, 0)
      mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "scala/runtime/AbstractFunction" + args.size, "<init>", "()V")
      mv.visitInsn(Opcodes.RETURN)
      mv.visitMaxs(1, 1)
      mv.visitEnd()
    }

    {
      mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "apply", "("+args.map(x => shortType(x)).mkString+")" + shortType(ret), null, null)
      mv.visitCode()
      mv.visitInsn(Opcodes.ICONST_1)
      mv.visitInsn(Opcodes.IRETURN)
      mv.visitMaxs(stackSize, locals)
      mv.visitEnd()
    }

    {
      mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_BRIDGE + Opcodes.ACC_SYNTHETIC, "apply", "(" + ("Ljava/lang/Object;" * args.size) + ")Ljava/lang/Object;", null, null)
      mv.visitCode()
      mv.visitVarInsn(Opcodes.ALOAD, 0)
      args.zipWithIndex
        .map{case (x, y) => (x, y+1)}
        .foreach { case (x, y) =>
          mv.visitVarInsn(Opcodes.ALOAD, y)
          if (shortType(x).startsWith("[")) {
            mv.visitTypeInsn(Opcodes.CHECKCAST, shortType(x));
          } else if(x != "O") {
            mv.visitMethodInsn(Opcodes.INVOKESTATIC, "scala/runtime/BoxesRunTime", unboxType(x), "(Ljava/lang/Object;)" + shortType(x))
          }
        }
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className, "apply", "("+args.map(x => shortType(x)).mkString + ")" + shortType(ret))
      mv.visitMethodInsn(Opcodes.INVOKESTATIC, "scala/runtime/BoxesRunTime", boxType(ret), "(" + shortType(ret) + ")" + javaType(ret))
      mv.visitInsn(Opcodes.ARETURN)
      mv.visitMaxs(stackSize, locals)
      mv.visitEnd()
    }

    cw.visitEnd()

    cw.toByteArray()
  }
}
