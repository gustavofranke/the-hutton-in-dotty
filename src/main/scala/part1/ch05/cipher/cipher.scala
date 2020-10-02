package part1.ch05.cipher

import common.count

def lowers(xs: String): Int =
  xs.filter(x => x >= 'a' && x <= 'z').length

def positions[A](x: A, xs: List[A]): List[Int] =
  for
    (x0, i) <- xs.zipWithIndex
    if x == x0
  yield i

//-- 5.5 The Caesar part1.ch05.cipher
def let2int(c: Char): Int = c.toInt - 'a'.toInt

def int2let(n: Int): Char = ('a'.toInt + n).toChar

def shift(n: Int, c: Char): Char = c match
  case c0 if c0.isLower => int2let((let2int(c0) + n) % 26)
  case _                => c

def encode (n: Int, xs: String): String =
  for
    x <- xs
  yield shift(n, x)  

//--- cracking
val table: List[Double] =                                   
  List(8.1f, 1.5f, 2.8f, 4.2f, 12.7f, 2.2f, 2.0f, 6.1f, 7.0f,
       0.2f, 0.8f, 4.0f, 2.4f, 6.7f, 7.5f, 1.9f, 0.1f, 6.0f,
       6.3f, 9.0f, 2.8f, 1.0f, 2.4f, 0.2f, 2.0f, 0.1f)
                                   
def percent(n: Int, m: Int): Double = (n.toDouble / m.toDouble) * 100

def freqs(xs: String): List[Double] = {
  val n = lowers(xs)
  for 
    x <- ('a' to 'z').toList
  yield percent(count(x, xs), n)
}

def chisqr(os: List[Double], es: List[Double]): Double =
  (for 
    (o, e) <- os.zip(es)
  yield (math.pow((o - e), 2) / e)).sum.toDouble

def rotate[A](n: Int, xs: List[A]): List[A] = xs.drop(n) ++ xs.take(n)

def crack(xs: String): String = {
  val table0: List[Double] = freqs(xs)
  val chitab: List[Double] = (0 to 25).toList.map(n => chisqr(rotate(n, table0), table))
  val factor: Int = positions(chitab.min, chitab).head
  encode(-factor, xs)
}

val test1 = crack("kdvnhoo lv ixq")
val test2 = crack("vscd mywzboroxcsyxc kbo ecopev")
val test3 = crack(encode(3, "haskell"))
val test4 = crack(encode(3, "boxing wizards jump quickly"))

//scala> let2int('a')
//val res1: Int = 0
//
//scala> int2let(0)
//val res2: Char = a
//
//scala> shift(3,'a')
//val res3: Char = d
//
//scala> shift(3,'z')
//val res4: Char = c
//
//scala> shift(-3,'z')
//val res5: Char = w
//
//scala> shift(-3,'c')
//val res6: Char = `
//
//scala> res6
//val res7: Char = `
//
//scala> shift(3,'')
//1 |shift(3,'')
//|        ^
//  |        empty character literal
//
//scala> shift(3,' ')
//val res8: Char =
//
//  scala> encode(3,"haskell is fun")
//  val res9: String = kdvnhoo lv ixq
//
//  scala> encode(-3,"kdvnhoo lv ixq")
//  val res10: String = haskell is fun
//
//  scala> percent(5,15)
//  val res11: Double = 0.0
//
//scala> rotate(3, List(1,2,3,4,5))
//val res5: List[Int] = List(4, 5, 1, 2, 3)
