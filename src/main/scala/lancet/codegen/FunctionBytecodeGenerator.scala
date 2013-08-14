package lancet.codegen

import org.objectweb.asm._

class FunctionTemplate(className: String, argMfs: List[Manifest[_]], val retMf: Manifest[_]) extends Opcodes {
  val args = argMfs.map(_.toString)

  val ret  = retMf.toString

  private def javaType(tpString: String) = tpString match {
    case "Byte"      => "java/lang/Byte"
    case "Short"     => "java/lang/Short"
    case "Int"       => "java/lang/Int"
    case "Long"      => "java/lang/Long"
    case "Double"    => "java/lang/Double"
    case "Float"     => "java/lang/Float"
    case "Char"      => "java/lang/Char"
    case "Boolean"   => "java/lang/Boolean"
    case _           => "java/lang/Object"
  }

  private def shortType(tpString: String) = tpString match {
    case "Byte"      => "B"
    case "Short"     => "S"
    case "Int"       => "I"
    case "Long"      => "L"
    case "Double"    => "D"
    case "Float"     => "F"
    case "Char"      => "C"
    case "Boolean"   => "Z"
    case "Unit"      => "V"
    case _           => "O"
  }

  private def unboxType(tpString: String) = "unbox" + tpString
  private def boxType(tpString: String) = "box" + tpString

  def bytecode(): Array[Byte] = {
    val cw = new ClassWriter(0)
    var mv: MethodVisitor = null


    cw.visit(Opcodes.V1_6, Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_SUPER,
      className,
      "Lscala/runtime/AbstractFunction" + args.size + "<" + (args ::: List(ret)).map(x => "L" + javaType(x) + ";") + ">;",
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
      mv.visitMaxs(1, 6)// TODO
      mv.visitEnd()
    }

    {
      mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_BRIDGE + Opcodes.ACC_SYNTHETIC, "apply", "(" + ("Ljava/lang/Object;" * args.size) + ")Ljava/lang/Object;", null, null)
      mv.visitCode()
      mv.visitVarInsn(Opcodes.ALOAD, 0)
      args.zipWithIndex.foreach { case (x, y) =>
        mv.visitVarInsn(Opcodes.ALOAD, y)
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "scala/runtime/BoxesRunTime", unboxType(x), "(Ljava/lang/Object;)" + shortType(x))
      }
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "s/TemplateFunctionS", "apply", "("+args.map(x => shortType(x)).mkString + ")" + shortType(ret));
      mv.visitMethodInsn(Opcodes.INVOKESTATIC, "scala/runtime/BoxesRunTime", boxType(ret), "(" + shortType(ret) + ")L" + javaType(ret)+ ";")
      mv.visitInsn(Opcodes.ARETURN)
      mv.visitMaxs(6, 4)// TODO
      mv.visitEnd()
    }

    cw.visitEnd()

    cw.toByteArray()
  }
}
