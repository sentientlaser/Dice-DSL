


private object RollDecl {
  private lazy val srand = java.security.SecureRandom.getInstance("NativePRNGBlocking")
  private lazy val prand = new java.util.Random(srand.nextLong)

  def apply(faces:Int, count:Int):RollResults = srand.ints(count.asInstanceOf[Long], 1, faces+1).toArray.toList.map { x=> RollResult(faces, x) }
  def apply(faces:Int):RollResult = apply(faces, 1).head
}

implicit class RollDecl(count:Int) { // rename

  def d(faces:Int):RollResults = RollDecl(faces, count)
}

case class RollResult(faces:Int, value:Int) {
  def toTuple = (faces, value)
  override def toString = s"$value($faces)"
}
object RollResult {
  def apply(x:(Int,Int)):RollResult = RollResult(x(0), x(1))
}

// implicit def tuple2roll(x:(Int, Int)):RollResult = Roll(x._1, x._2)

type RollResultTuple = (Int, Int)
type RollResults = List[RollResult]


abstract class Rule[T <: RollResults | Int] extends Function1[RollResult => Boolean, RollResults => T] {
  type RuleFunction = RollResults => T
  type ResultsTransformer = RollResults => RollResults
  type RollPredicate = RollResult => Boolean

  override def apply(predicate: RollPredicate):RuleFunction
  final def where = apply
  final def where(predicate: Int => Boolean):RuleFunction = apply( (x:RollResult) => predicate(x.value) )
  final def on(values:Int*) = where(values.contains(_))
  final def over(value:Int)  = where(_ >= value)
  final def under(value:Int) = where(_ <= value)
}

abstract class RerollRule extends Rule[RollResults]{
  type RetentionFunctionTransform = RollPredicate => RollPredicate


  val retain: RetentionFunctionTransform
  val generate: ResultsTransformer

  final val retainAll:RetentionFunctionTransform = (x) => {x => true}
  final val dropSelected:RetentionFunctionTransform = (x) => (x.andThen(!_))


  final val generateForSameFace:ResultsTransformer = (r) => r.map {x=> RollDecl(x.faces)}

  final def applyrule(rolls:RollResults, predicate: RollPredicate):RollResults = {
    val filteredRolls = rolls.filter(predicate) //list.filter(predicate)
    val retainedRolls = rolls.filter(retain(predicate)) //list.filter(predicate.andThen(!_))
    val generatedRolls = generate(filteredRolls)

    // println(s"$filteredRolls ||| $retainedRolls ||| $generatedRolls")
    if (filteredRolls.isEmpty) {rolls} else {retainedRolls :++ applyrule(generatedRolls, predicate)}
  }

  override def apply(predicate: RollPredicate) = (roll:RollResults) => applyrule(roll, predicate)
}

case object Explode extends RerollRule{
  override val retain = retainAll
  override val generate = generateForSameFace
}
case object Reroll extends RerollRule {
  override val retain = dropSelected
  override val generate = generateForSameFace
}

abstract class ResultRule extends Rule[Int] {
}

case object Total extends ResultRule {
  def all = apply( x => true)

  override def apply(predicate: RollPredicate) = (x:RollResults) => x.filter(predicate).map(_.value).foldLeft(0) {(x,y) => x+y}
}

class Count extends ResultRule{
  override def apply(predicate: RollPredicate) = (x:RollResults) => x.count(predicate)
}

case object Count extends Count
case object Successes extends Count
case object Failures extends Count

implicit class UnpackRolls(val results:RollResults) {

  def & [T] (rule:RollResults => T):T = {
    println(results)
    rule(results)
  }
}
//
// implicit class Lookup(table:Map[List[Int], Any]) {
//
// }
//
object DiceFiddle {
  import scala.language.postfixOps

  def main(args:Array[String]) = {

    val roll = {6 d 6} & Explode.on(6) & Reroll.on(1,2) & Successes.where { _ < 5}


    println(roll)
    // println(roll >> Total.all)
    // println(roll & Explode.on(6) )
    // println(roll & Reroll.on(1,2))
  }
}
