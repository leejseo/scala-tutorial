import scala.util.parsing.combinator._

sealed trait AE

case class Num(val n: Int) extends AE
case class BinOp(val op: String, val e0: AE, val e1: AE) extends AE

object AEParser extends RegexParsers {
    def wrap[T](e: Parser[T]): Parser[T] = "(" ~> e <~ ")"
    lazy val int: Parser[Int] = "\\d+".r ^^ ((x: String) => x.toInt)
    lazy val expr: Parser[AE] = 
    int                        ^^ { case (n) => Num(n) } |
    wrap((expr <~ "+") ~ expr) ^^ { case l ~ r => BinOp("+", l, r) } |
    wrap((expr <~ "-") ~ expr) ^^ { case l ~ r => BinOp("-", l, r) }

    def apply(str: String): AE =
        parseAll(expr, str).getOrElse(throw new Exception)
}

object AEInterpreter{
    def interpret(expr: AE): Int = expr match {
        case Num(n) => n
        case BinOp("+", e0, e1) =>
            interpret(e0) + interpret(e1)
        case BinOp("-", e0, e1) => 
            interpret(e0) - interpret(e1)
        case _ => throw new Exception
    }
}

/* Example TestCases
// #1. Parser

println(AEParser("(7-((2+3)+5))"))
    // BinOp(-,Num(7),BinOp(+,BinOp(+,Num(2),Num(3)),Num(5)))
println(AEParser("1")) // Num(1)
println(AEParser("1+2")) // Error
println(AEParser("(1+2+3)")) // Error

// #2. Interpreter

println(AEInterpreter.interpret(AEParser("(7-((2+3)+5))")))
    // -3
println(AEInterpreter.interpret(AEParser("(7+((2+3)+5))")))
    // 17
println(AEInterpreter.interpret(Num(3))) // 3
println(AEInterpreter.interpret(BinOp("+", Num(3), Num(2))))
    // 5
println(AEInterpreter.interpret(BinOp("-", Num(3), 5)))
    // Error
*/
