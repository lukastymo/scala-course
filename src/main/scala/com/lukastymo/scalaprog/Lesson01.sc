import scala.collection.immutable
// =================================================================
// Welcome to the Programming Tutorial
// =================================================================

/*

==========================================
= PLAN
==========================================

1. Composition: R1
2. Types: R1
3. Functions: R1
4. Pure-ness: R2
5. Algebra: R2
6. ADT: R2
7. For comprehension, R3
8.

==========================================
= RESOURCES
==========================================

R1 Category Theory for Programmers
R2 FP Simplified
R3 FP Mortals
R4 JVM Scala book
R5 Scala with cats

 */

//==========================================
//= Lesson 1 - 10 exercises
//= Only 328026 programmers in the world solved it correctly
//==========================================

/*
In this tutorial we'll jump straight into problem solving
 */

// Q1.1. https://projecteuler.net/problem=1
(1 until 1000).filter(x => x % 3 == 0 || x % 5 == 0).sum

// Q1.2. https://projecteuler.net/problem=2
def fib(n: Int): Int =
  n match {
    case 0 => 0
    case 1 => 1
    case _ => fib(n - 1) + fib(n - 2)
  }

fib(10) // should be 50
fib(40) // 102334155

LazyList
  .from(1)
  .map(fib)
  .filter(x => x % 2 == 0)
  .takeWhile(x => x <= 4000000)
  .sum

// Q1.3
def largestPrimeFactor(x: Long): Int = {
  val limit = Math.sqrt(x).toInt

  val isPrime = Array.fill(limit + 1)(true)
  isPrime(0) = false
  isPrime(1) = false

  (2 to limit).foreach { a =>
    if (isPrime(a))
      LazyList
        .from(2)
        .map(_ * a)
        .takeWhile(_ < limit)
        .foreach(y => isPrime(y) = false)
  }

  (limit to 2 by -1)
    .find(y => isPrime(y) && x % y == 0)
    .getOrElse(-1)
}

largestPrimeFactor(600851475143L)

// Q1.4

// Q1.5

// Q1.6

// Q1.7

// Q1.8

// Q1.9

// Q1.10. https://projecteuler.net/problem=10
