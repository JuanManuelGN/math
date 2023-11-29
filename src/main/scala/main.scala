import scala.annotation.tailrec
import scala.language.postfixOps

@main
def main(n: Long): Unit = {
  def print(s: String): Unit = {
    println(s"---------------------------")
    println(s)
    println(s"---------------------------")
  }

  print(s"Calculando Conjetura de Collatz para $n")

  @tailrec
  def loop(n: Long, acc: List[Long]): List[Long] = n match {
    case x if x == 4L => acc :+ 4L :+ 2L :+ 1L
    case x if x % 2L == 0L => loop(x / 2L, acc :+ x)
    case x => loop(n * 3L + 1L, acc :+ x)
  }

  type C = (Long, Int)
  implicit object AgeOrdering extends Ordering[C] {
    def compare(a: C, b: C): Int = a._2 compare b._2
  }
  type G = (Long, Long)
  implicit object LOrdering extends Ordering[G] {
    def compare(a: G, b: G): Int = a._1 compare b._1
  }

  val resp: List[List[Long]] = (4L to n).toList.map(x => loop(x, List.empty[Long]))
  val max = resp.flatten.max
//  val mode =  {
//    val grouped = resp.flatMap(_.dropRight(5)).groupBy(x => x)
//    grouped.map{ case (k, ls) => (k, ls.size)}
//      .max(AgeOrdering)._1
//  }

//List(
//  List(5, 16, 8, 4, 2, 1),
//  List(8, 4, 2, 1)
//) -->
//  List(
//    (16, List(5, 16, 8, 4, 2, 1)),
//    (8, List(8, 4, 2, 1))
//  ) -->
//    List(
//      16/5,
//      8/8
//    )
  val maxs = resp.map(ls => (ls.max, ls.head))



//  print(s"Conjetura de Collatz para $n es ${resp.mkString("\n")}")
  print(s"Número mayor de las progresiones = $max")
//  maxs.foreach(m => println(s"Ratio entre el mayor número de una progresión(${m._1}) y el inicio(${m._2}) = ${m._1 / m._2}"))
  print(s"Mayor ratio = ${maxs.max(LOrdering) match {case(m, i) => m / i}}")

}