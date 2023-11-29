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

  type T = (Long, Long)
  implicit object LOrdering extends Ordering[T] {
    def compare(a: T, b: T): Int = a._1 compare b._1
  }

  val resp: List[List[Long]] = (1L to n).toList.map(x => loop(x, List.empty[Long]))
  val max = resp.flatten.max
  val pathMax = resp.map(_.size).max
  val maxs = resp.map(ls => (ls.max, ls.head))



//  print(s"Conjetura de Collatz para $n es \n${resp.mkString("\n")}")
  print(s"Mayor número en una de las prograsiones = $max")
  print(s"Camino mas largo = $pathMax")
//  maxs.foreach(m => println(s"Ratio entre el mayor número de una progresión(${m._1}) y el inicio(${m._2}) = ${m._1 / m._2}"))
  print(s"Mayor ratio entre el tamaño de una progresión y el número de inicio = ${maxs.max(LOrdering) match {case(m, i) => m / i}}")

}