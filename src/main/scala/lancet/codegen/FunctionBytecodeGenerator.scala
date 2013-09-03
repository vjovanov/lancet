package lancet.codegen

import org.objectweb.asm._
import scala.tools.nsc.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader

case class JVMMethodCall(opcode: Int, clazz: String, method: String, args: List[Class[_]], ret: Class[_])

object FunctionTemplate {
  def generateAndLoadFunction(
    className: String,
    args: List[Manifest[_]],
    resTp: Manifest[_],
    methods: List[JVMMethodCall]): Class[_] = {
    val fTemplate = new FunctionTemplate(className,
      args,
      resTp,
      600,
      200,
      methods
    )
    val classBytes = fTemplate.bytecode
    // debugging
    import java.io._
    val bos = new BufferedOutputStream(new FileOutputStream("/tmp/classfile"))
    bos.write(classBytes)
    bos.flush()
    bos.close()
    //
    val vfs = new VirtualDirectory("<vfs>", None)
    val file = vfs.fileNamed(className + ".class")
    val bout = file.bufferedOutput
    bout.write(classBytes, 0, classBytes.length)
    bout.flush
    bout.close()

    val loader = new AbstractFileClassLoader(vfs, this.getClass.getClassLoader)
    loader.loadClass(className)
  }
}


class FunctionTemplate(
  className: String,
  argMfs: List[Manifest[_]],
  val retMf: Manifest[_],
  val locals: Int,
  val stackSize: Int,
  val methods: List[JVMMethodCall]
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
      case "Byte"    | "byte"    => "B"
      case "Short"   | "short"   => "S"
      case "Int"     | "int"     => "I"
      case "Long"    | "long"    => "J"
      case "Double"  | "double"  => "D"
      case "Float"   | "float"   => "F"
      case "Char"    | "char"    => "C"
      case "Boolean" | "boolean" => "Z"
      case "Unit"    | "void"    => "V"
      case Arr(v)                => "[" + bcType(v)
      case str                   => "L" + str.replaceAll("\\.", "/") + ";"
    }
  }

  private def returnInst(tpString: String): Int = {
    tpString match {
      case "Byte"    | "byte"    => Opcodes.IRETURN
      case "Short"   | "short"   => Opcodes.IRETURN
      case "Int"     | "int"     => Opcodes.IRETURN
      case "Long"    | "long"    => Opcodes.LRETURN
      case "Double"  | "double"  => Opcodes.DRETURN
      case "Float"   | "float"   => Opcodes.FRETURN
      case "Char"    | "char"    => Opcodes.IRETURN
      case "Boolean" | "boolean" => Opcodes.IRETURN
      case "Unit"    | "void"    => throw new RuntimeException("Wrong usage!!! Should not return void.")
      case Arr(v)                => Opcodes.ARETURN
      case str                   => Opcodes.RETURN
    }
  }

  /*
  * NOTE: Graal IR generators depend on the fact that the size of
  * byte code instructions in each branch is exactly 1 byte..
  */
  private def push(mv: MethodVisitor, tp: String): Unit = tp match {
    case "Boolean"  | "boolean" => mv.visitInsn(Opcodes.ICONST_1);
    case "Byte"     | "byte"    => mv.visitInsn(Opcodes.ICONST_1);
    case "Char"     | "char"    => mv.visitInsn(Opcodes.ICONST_1);
    case "Short"    | "short"   => mv.visitInsn(Opcodes.ICONST_1);
    case "Int"      | "int"     => mv.visitInsn(Opcodes.ICONST_1);
    case "Long"     | "long"    => mv.visitInsn(Opcodes.LCONST_1);
    case "Double"   | "double"  => mv.visitInsn(Opcodes.DCONST_1);
    case "Float"    | "float"   => mv.visitInsn(Opcodes.FCONST_1);
    case "Unit"     | "unit"    => mv.visitInsn(Opcodes.NOP); // easier to calculate
    case Arr(_)                 => mv.visitInsn(Opcodes.ACONST_NULL);
    case _                      => mv.visitInsn(Opcodes.ACONST_NULL);
  }

  def bytecode(): Array[Byte] = {
    val cw = new ClassWriter(0)
    var mv: MethodVisitor = null
    def unbox(x: String) = {
      def unboxType(tpString: String) = "unboxTo" + tpString
      if (bcType(x).startsWith("[")) {
        mv.visitTypeInsn(Opcodes.CHECKCAST, bcType(x));
      } else if(!x.startsWith("L")) {
        println(bcType(x))
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "scala/runtime/BoxesRunTime", unboxType(x), "(Ljava/lang/Object;)" + bcType(x))
      }
    }

    def box(x: String) = {
      def boxTo(tpString: String) = "boxTo" + tpString
      if(!(bcType(x).startsWith("L") || bcType(x).startsWith("[")))
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "scala/runtime/BoxesRunTime", boxTo(x), "(" + bcType(x) + ")Ljava/lang/Object;")
    }


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
      for (JVMMethodCall(opcode, cls, method, types, retType) <- methods) {
        if (opcode == Opcodes.INVOKEVIRTUAL)
          mv.visitInsn(Opcodes.ACONST_NULL)
        else
          mv.visitInsn(Opcodes.NOP)
        for (arg <- types) push(mv, arg.getName)
        mv.visitMethodInsn(opcode, cls, method, types.map(_.getName).map(bcType).mkString("(", "" ,")") + bcType(retType.getName))
        bcType(retType.getName) match {
          case "V"       => mv.visitInsn(Opcodes.NOP)
          case "D" | "J" => mv.visitInsn(Opcodes.POP2)
          case _         => mv.visitInsn(Opcodes.POP)
        }
      }
      push(mv, ret)
      mv.visitInsn(returnInst(ret))
      mv.visitMaxs(stackSize, locals)
      mv.visitEnd()
    }

    {
      mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_BRIDGE + Opcodes.ACC_SYNTHETIC, "apply", "(" + ("Ljava/lang/Object;" * args.size) + ")Ljava/lang/Object;", null, null)
      mv.visitCode()
      mv.visitVarInsn(Opcodes.ALOAD, 0)
      args.zipWithIndex
        .map { case (x, y) => (x, y+1) }
        .foreach { case (x, y) =>
          mv.visitVarInsn(Opcodes.ALOAD, y)
          unbox(x)
        }
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className, "apply", "("+args.map(x => bcType(x)).mkString + ")" + bcType(ret))
      box(ret)
      mv.visitInsn(Opcodes.ARETURN)
      mv.visitMaxs(stackSize, locals)
      mv.visitEnd()
    }

    cw.visitEnd()

    cw.toByteArray()
  }
}