import argonaut._, Argonaut._


sealed trait BooleanExpression
case object True extends BooleanExpression
case object False extends BooleanExpression
case class Variable(symbol: String) extends BooleanExpression
{
  def SymbolVar(symbol:String): String =
  symbol match {
    case "||" => "OR"
    case "&&" => "AND"
    case "!"=> "NOT"
  }
}
case class Not(e: BooleanExpression) extends BooleanExpression
{
  def notOp(BoolExp: BooleanExpression): BooleanExpression =
  BoolExp match {
    case Not(True) => False
    case Not(False) => True
  }
}
case class Or(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression
{
  def orOp(BoolExp1: BooleanExpression, BoolExp2: BooleanExpression): BooleanExpression =
  (BoolExp1, BoolExp2) match {
    case Or(True, False) => True
    case Or(False, True) => True
    case Or(True, True) => True
    case Or(False, False) => False
    
  }
}
case class And(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression
{
  def andOp(BoolExp1: BooleanExpression, BoolExp2: BooleanExpression): BooleanExpression =
  (BoolExp1, BoolExp2) match {
    case And(False, True) => False
    case And(False, False) => False
    case And(True, False) => False
    case And(True, True) => True
  }
}


case class LogicalExp(variable: Variable, not: Not, and:And, or:Or )

val variable= Variable("&&")
val not = Not(True)
val and=And(True, False)
val or=Or(True, False)
val logicExp=(variable,not, and, or)


val varJson=logicExp.asJson

implicit def ExpEncodeJson: EncodeJson[LogicalExp] =
  EncodeJson((exp: LogicalExp) =>
    ("symbol" := exp.variable.symbol) ->:
    ("not" := exp.not.notOp()) ->:
    ("and" := exp.and.andOp()) ->:
    ("or" := exp.or.orOp()) ->: jEmptyObject)

 implicit def ExpDecodeJson: DecodeJson[LogicalExp] =
  DecodeJson(c => for {

   symbol <- (c --\ "symbol").as[String]
   not <- (c --\ "not").as[BooleanExpression]
    and <- (c --\ "and").as[BooleanExpression]
    or <- (c --\ "or").as[BooleanExpression]
} yield LogicalExp(Variable(symbol), Not(not), And(and), Or(or)))


def main(args: Array[String]) {
    // running a sample
   val variable= Variable("&&")
val not = Not(True)
val and=And(True, False)
val or=Or(True, False)
val logicExp=(variable,not, and, or)
   
  // convert the person to json
    val json = logicExp.asJson
    var content = json.toString()

    println (content)

    // we should get a person instance here
    var expdecoded : LogicalExp = content.decodeOption[LogicalExp].get

    println (expdecoded)
  }




