package q

import cats.data.NonEmptyList
import cats.data.Validated.Valid
import cats.Eval

import scala.util._

object Q {

  class Q1 {
    val file = 1500
    val cluster = 512

    def f(file: Int, cluster: Int): Int = {
      val mod = file % cluster
      val add1cluster = if (mod != 0) cluster else 0
      val r = (file / cluster) * cluster + add1cluster
      r
    }
  }

  class Q2 {
    def f(ls: List[Int], d: Int) = {
      val m = ls.max
      println(m)
      val disc = m * (d.toFloat / 100)
      println(disc)
      val t = ls.sum - disc
      t
    }
  }

  class Q3 {
    def rLeaves(w: Int, h: Int, l: Array[Array[Int]], winds: String) = {
      // case R, mover todas las hoojas a derecha
      l.map(c => c.tail)
    }
  }

  val matrix = Array(Array(1, 2, 3), Array(4, 1, 1), Array(7, 1, 1))
  val matrixSize = matrix.length
  val matrixF = matrix.flatten
  //  val subMatrix1 = (0 until matrixSize * matrixSize).map(n => matrixF.slice(n, n+1))
  //  val subMatrix2 = (0 until matrixSize * matrixSize).map(n => matrixF.slice(n, n+1))
  //
  //  def subMatrix(cont: Int, size: Int, matrix: List[Int], acc: List[List[Int]]): List[List[Int]] = {
  //    if (cont == size)
  //      acc
  //    else {
  //      val subM = (cont to size).map(n => matrix.slice(cont, cont + n))
  //      subMatrix(cont +1, size, matrix, acc ++ subM.toList)
  //    }
  //  }
  //
  //  // quiero todas las submatrices de tamaño n
  //  val allSubMs = (0 to 3).map(n => subMatrix(0,n,matrixF.toList, List.empty[List[Int]]))
  //  allSubMs.foreach(println(_))
  //  println(subMatrix1)
  //  var result = List.empty[Array[Array[Int]]]
  //
  //  for {
  //    startRow <- 0 until matrixSize
  //    endRow <- startRow until matrixSize
  //    startCol <- 0 until matrixSize
  //    endCol <- startCol until matrixSize
  //  } {
  //    val subMatrix = Array.tabulate(endRow - startRow + 1, endCol - startCol +1) {
  //      (i,j) => matrix(startRow+i)(startCol+j)
  //    }
  //    if (isSquare(subMatrix))
  //      result = subMatrix :: result
  //    else result
  //  }
  //  val filtered = result.filter(subM => isOnesSubM(subM))
  //  val biggest = filtered.maxBy(subM => subM.length).length
  //  println(biggest)
  //  biggest.foreach{
  //    subm =>
  //      println(subm.map(_.mkString(", ")).mkString("\n"))
  //      println("----")
  //  }

  def isSquare(array: Array[Array[Int]]) =
    array.length == array.head.length

  def isOnesSubM(array: Array[Array[Int]]): Boolean =
    array.flatten.foldLeft(true)((acc, elem) => if (elem == 1) acc else false)
  //  val subMatrixs =
  //    (1 to 1).toList
  //      .flatMap(n =>
  //        (0 to 3).toList
  //          .map(y => matrix.slice(n, n + y).map(_.slice(n, n + y))))

  //  val matrixT = matrix.transpose
  //  val subMatrixR =
  //    (0 to 3).toList
  //      .flatMap(n =>
  //        (0 to 3).toList
  //          .map(y => matrixT.slice(n, n + y).map(_.slice(n, n + y))))
  //
  //  val subMatrixs = (0 to 3).map(row => matrix.slice(row, row+1))
  //  println(subMatrixs.distinct)
  //  println(subMatrixR)


  type Macaron = List[List[String]]
  val macarons: List[String] = List(
    "APPLE",
    "APPLE",
    "BANANA",
    "BANANA",
    "BANANA",
    "CHERRY",
    "APPLE",
    "STRAWBERRY",
    "CHOCOLATE",
    "STRAWBERRY",
    "CHOCOLATE",
    "APPLE",
    "CHERRY"
  )

  def addToNonCompletePartition(
                                 partitionedLists: List[Set[String]],
                                 elem: String
                               ): List[Set[String]] = {
    def r(ls: List[Set[String]], elem: String, checked: List[Set[String]], assigned: Boolean): List[Set[String]] = {
      if (assigned) {
        println(s"PR ${ls ++ checked}")
        val result = ls ++ checked //.filter(_.nonEmpty)
        println(s"result $result")
        result
      } else {
        if (ls.isEmpty)
          r(List(Set.empty[String]), elem, checked ++ List(Set(elem)), true)
        else {
          val biggerSet = ls.maxBy(_.size)
          val newLs = ls.filter(_ != biggerSet)
          println(s"new list = $newLs")
          if (!biggerSet.contains(elem)) {
            val newSet = biggerSet + elem
            r(newLs, elem, checked ++ List(newSet), true)
          } else r(newLs, elem, checked ++ List(biggerSet), false)
        }
      }
    }

    r(partitionedLists, elem, List(Set.empty[String]), false)
  }

  def partition(ls: List[String]): List[Set[String]] = {
    ls.foldLeft(List(Set.empty[String]))((partitionedLists, elem: String) => {
      println(s"nonCompletePartition $partitionedLists")
      addToNonCompletePartition(partitionedLists, elem)
      //      if(partitionedLists.last.contains(elem)) {
      //        Set(elem) :: partitionedLists
      //      } else {
      //        (partitionedLists.head + elem) :: partitionedLists.tail
      //      }
    })
  }

  println(s"final result ${partition(macarons)}")

  //  val q = new Q2
  //  println(q.f(List(100, 400, 200), 20))
  //  val q1 = new Q1
  //  println(q1.f(4, 3))
  ////  val ls = List(-8, 3, 4, 8, -4, 6)
  //  val ls = List(-8)
  ////  val ls = List()
  //  val nel = NonEmptyList.fromList(ls)
  //  def f(ls: Option[NonEmptyList[Int]]): Int =
  //    ls match {
  //      case None => 0
  //      case Some(NonEmptyList(x, List())) => x
  //      case Some(ls) => ls.foldRight(Eval.now(ls.head))((y, acc) => {
  //        (y.abs, acc.value.abs) match {
  //          case (ya, acca) if ya == acca => acc
  //          case (ya, acca) if ya < acca => Eval.now(y)
  //          case (_, acca)  => acc
  //        }
  //      } ).value
  //    }
  //
  //  println(f(nel))
  ////  val
  ////  println(
  ////    ls.foldRight(ls.head)((x, acc) => {
  ////      (x.abs, acc.abs) match {
  ////        case (x, acc) if x == acc => acc
  ////        case (x, acc) if x < acc  => x
  ////        case (_, acc)             => acc
  ////      }
  ////    })
  ////  )

}
