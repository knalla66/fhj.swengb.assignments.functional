package fhj.swengb.assignments.functional.mknaller

/**
  * This is the first assignment for swengb 2015.
  *
  */
case class FunctionalAssignment(lowerBound: Int = 1,
                                upperBound: Int = 1000,
                                factor: Int = 2) {
  require(lowerBound < upperBound)

  /**
    * implement a sequence of ints, which start with lowerBound and end with
    * upperbound.
    *
    * for all elements following is true:
    *
    * xs(i) < xs(i+1)
    * xs(i) + 1 == xs(i + 1) (for i > 0 and i <= 1000)
    *
    */
  val xs: Seq[Int] = Seq.tabulate(upperBound)(_ + lowerBound)

  /**
    * provide the sum of all elements defined in xs
    */
  val sumXs = xs.reduce(_+_)

  /**
    * This sequence contains a list with the following elements:
    *
    * x(0)   = (upperbound) * factor
    * x(1)   = (upperbound - 1) * factor
    * x(2)   = (upperbound - 2) * factor
    *
    * ... goes on and on ...
    *
    * x(998) = factor
    * x(999) = lowerbound
    *
    */
  val ys: Seq[Int] = xs.reverse.map(x => x*factor)

  /**
    * this sequence contains pairs of xs and ys. the ys should
    * be in reversed order.
    */
  val zs: Seq[(Int, Int)] = xs zip ys.reverse

  /**
    * Function should divide the second operand with the first.
    */

  def f(ab: (Int, Int)): Int = ab._2/ab._1

  /**
    * apply function f defined above to all elements of zs, return the sequence
    */
  val result: Seq[Int] = for (z <- zs) yield f(z)
  // yield is die Rückgabe und gibt z von zs aus und das für alle in der Sequence (for)

}

