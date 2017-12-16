package org.romeo.adventofcode.day1

import org.romeo.adventofcode.common.Puzzle

object DayOne {
  def main(args: Array[String]): Unit = {
    new DayOne().run()
  }
}

class DayOne extends Puzzle("http://adventofcode.com/2017/day/1/input") {

  override def validateInput(input: String): Unit = {
    assert(input.map(_.asDigit).forall(i => i >= 0 && i <= 9))
  }

  override def solvePart1(input: String): String = {
    val inputInts = input.map(_.asDigit)
    val inputWrapped = inputInts.toList ++ List(inputInts.head)
    val inputGroups = inputWrapped.sliding(2)
    val doubles = inputGroups.filter(list => list.forall(i => i == list.head)).map(_.head)
    doubles.sum.toString
  }

  def rotateList[A](n: Int, list: List[A]): List[A] = {
    list.drop(n) ++ list.take(n)
  }

  override def solvePart2(input: String): String = {
    val inputInts = input.map(_.asDigit).toList
    val inputPairs = inputInts.zip(rotateList(input.length / 2, inputInts))
    val doubles = inputPairs.filter(x => x._1 == x._2).map(_._1)
    doubles.sum.toString
  }
}
