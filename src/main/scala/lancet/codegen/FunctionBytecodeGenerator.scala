package lancet.codegen

import org.objectweb.asm._

class FunctionTemplate(
  className: String,
  argMfs: List[Manifest[_]],
  val retMf: Manifest[_],
  val locals: Int,
  val stackSize: Int,
  val methods: List[(String, String, List[Manifest[_]], Manifest[_])]
) extends Opcodes {
  val args = argMfs.map(_.toString)
  val ret  = retMf.toString

  private def boxedType(tpString: String) = tpString match {
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
  private val Arr = "Array\\[(.*)\\]".r
  private def bcType(tpString: String): String = {
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
      case Arr(v)      => "[" + bcType(v)
      case _           => "Ljava/lang/Object;"
    }
  }

  private def unboxType(tpString: String) = "unboxTo" + tpString
  private def boxType(tpString: String) = "boxTo" + tpString
  private def push(mv: MethodVisitor, tp: String): Unit = tp match {
    case "Boolean"   => mv.visitInsn(Opcodes.ICONST_1);
    case "Byte"      => mv.visitInsn(Opcodes.ICONST_1);
    case "Char"      => mv.visitInsn(Opcodes.ICONST_1);
    case "Short"     => mv.visitInsn(Opcodes.ICONST_1);
    case "Int"       => mv.visitInsn(Opcodes.ICONST_1);
    case "Long"      => mv.visitInsn(Opcodes.LCONST_1);
    case "Double"    => mv.visitInsn(Opcodes.DCONST_1);
    case "Float"     => mv.visitInsn(Opcodes.FCONST_1);
    case Arr(_)      => mv.visitInsn(Opcodes.ACONST_NULL);
    case "Unit"      =>
    case _           => mv.visitInsn(Opcodes.ACONST_NULL);

      // cast to type "Ljava/lang/Object;"
  }

  def bytecode(): Array[Byte] = {
    val cw = new ClassWriter(0)
    var mv: MethodVisitor = null


    cw.visit(Opcodes.V1_6, Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_SUPER,
      className,
      "Lscala/runtime/AbstractFunction" + args.size + "<" + (args ::: List(ret)).map(x => boxedType(x)) + ">;",
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
      mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "apply", "("+args.map(x => bcType(x)).mkString+")" + bcType(ret), null, null)
      mv.visitCode()
      for ((cls, method, types, retType) <- methods) {
        mv.visitInsn(Opcodes.ACONST_NULL);
        for (arg <- types) {
          push(mv, arg.toString)
        }
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, cls, method, types.map(_.toString).map(bcType).mkString("(", "," ,")") + bcType(retType.toString))
      }
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
          if (bcType(x).startsWith("[")) {
            mv.visitTypeInsn(Opcodes.CHECKCAST, bcType(x));
          } else if(x != "Ljava/lang/Object;") {
            mv.visitMethodInsn(Opcodes.INVOKESTATIC, "scala/runtime/BoxesRunTime", unboxType(x), "(Ljava/lang/Object;)" + bcType(x))
          }
        }
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className, "apply", "("+args.map(x => bcType(x)).mkString + ")" + bcType(ret))
      mv.visitMethodInsn(Opcodes.INVOKESTATIC, "scala/runtime/BoxesRunTime", boxType(ret), "(" + bcType(ret) + ")" + boxedType(ret))
      mv.visitInsn(Opcodes.ARETURN)
      mv.visitMaxs(stackSize, locals)
      mv.visitEnd()
    }

    cw.visitEnd()

    cw.toByteArray()
  }
}
