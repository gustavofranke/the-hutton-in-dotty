package part1.ch07.transmitter

import common.count

//-- 7.7 Voting algorithms
val votes = List("Red","Blue","Green","Blue","Blue","Red")

def count1[A](x: A, xs: List[A]): Int = xs.filter(_ == x).length

val test6 = count1("Red", votes) //-- 2

//rmdups :: Eq a => [a] -> [a]
//rmdups [] = []
//rmdups (x:xs) = x : filter (/= x) (rmdups xs)
def rmdups[A](as: List[A]): List[A] = as match
  case Nil      => Nil
  case x :: xs => x :: rmdups(xs).filter(_ != x)

val test7 = rmdups(votes) //-- ["Red","Blue","Green"]

//result :: Ord a => [a] -> [(Int,a)]
//result vs = sort [(count1 v vs, v) | v <- rmdups vs]
def result[A](vs: List[A]): List[(Int, A)] =
  rmdups(vs)
    .map(v => (count1(v, vs), v))
    .sortBy(_._1)
//  (for
//  v <- rmdups(vs)
//yield (count1(v, vs), v)).sorted

val test8 = result(votes) //-- [(1,"Green"),(2,"Red"),(3,"Blue")]

//winner :: Ord a => [a] -> a
//  winner = snd . last . result
def winner[A](as: List[A]): A = result(as).last._2

val test9 = winner(votes) //-- "Blue"

//-- Alternative vote
val ballots: List[List[String]] = List(
  List("Red", "Green"),
  List("Blue"),
  List("Green", "Red", "Blue"),
  List("Blue", "Green", "Red"),
  List("Green"))

//rmempty :: Eq a => [[a]] -> [[a]]
//rmempty = filter (/= [])
def rmempty[A](ass: List[List[A]]): List[List[A]] =
  ass.filter(_ != Nil)

//elim :: Eq a => a -> [[a]] -> [[a]]
//elim x = map (filter (/= x))
def elim[A](x: A, ass: List[List[A]]): List[List[A]] =
  ass.map(_.filter(_ != x))

//rank :: Ord a => [[a]] -> [a]
//rank = map snd . result . map head
def rank[A](ass: List[List[A]]): List[A] =
  ass.map { as =>
    result(as).map(x => x._2).head
  }

//test10 :: [String]
val test10 = rank(ballots) //-- ["Red","Blue","Green"]

//winner' :: Ord a => [[a]] -> a
//  winner' bs = case rank (rmempty bs) of
//  [c] -> c
//  (c:cs) -> winner' (elim c bs)
def winner0[A](bs: List[List[A]]): A = rank(rmempty(bs)) match
  case c :: Nil => c
  case c :: cs  => winner0 (elim(c, bs))

val test11: String = winner0(ballots)