package part1.ch09.countdown

trait Show[A]:
  def show(a: A): String

enum Op:
  case Add, Sub, Mul, Div

given Show[Op]:
  def show(a: Op): String = a match
    case Op.Add => "+"
    case Op.Sub => "-"
    case Op.Mul => "*"
    case Op.Div => "/"

def valid(op: Op, x: Int, y: Int): Boolean = op match
  case Op.Add => true
  case Op.Sub => x > y
  case Op.Mul => true
  case Op.Div => x % y == 0

def apply(op: Op, x: Int, y: Int): Int = op match
  case Op.Add => x + y
  case Op.Sub => x - y
  case Op.Mul => x * y
  case Op.Div => x / y

// 9.3 Numeric expressions
enum Expr:
  case Val(i: Int)
  case App(op: Op, e1: Expr, e2: Expr)

given Show[Expr]:
  def show(a: Expr): String = a match
    case Expr.Val(n)       => summon[Show[Int]].show(n)
    case Expr.App(o, l, r) =>
      def brak(e: Expr) = e match
        case Expr.Val(n) => summon[Show[Int]].show(n)
        case e           => "(" + show(e) + ")"
      brak(l) + summon[Show[Op]].show(o) + brak(r)

given Show[Int]:
  def show(a: Int): String = a.toString

//-- For example, 1 + (2 ∗ 3) can be represented as:
val test0 = summon[Show[Expr]].show(Expr.App(Op.Add, Expr.Val(1), Expr.App(Op.Mul, Expr.Val(2), Expr.Val(3)))) //-- "1+(2*3)"

def values(e: Expr): List[Int] = e match
  case Expr.Val(n)       => List(n)
  case Expr.App(_, l, r) => values(l) ++ values(r)

def eval(e: Expr): List[Int] = e match
  case Expr.Val(n)       => List(n).filter(_ > 0)
  case Expr.App(o, l, r) =>
    for
      x <- eval(l)
      y <- eval(r) if valid(o, x, y)
    yield apply(o, x, y)

val test1 = eval(Expr.App(Op.Add, Expr.Val(2), Expr.Val(3))) //-- [5]
val test2 = eval(Expr.App(Op.Sub, Expr.Val(2), Expr.Val(3))) //-- []

// 9.4 Combinatorial functions
def subs[A](la: List[A]): List[List[A]] = la match
  case Nil     => List(List())
  case x :: xs =>
    val yss: List[List[A]] = subs(xs)
    yss ++ yss.map(y => x :: y)

def interleave[A](a: A, la: List[A]): List[List[A]] = (a, la) match
  case (x, Nil)     => List(List(x))
  case (x, y :: ys) => (x :: y :: ys) :: interleave(x, ys).map(is => y :: is)

def perms[A](la: List[A]): List[List[A]] = la match
  case Nil     => List(List())
  case x :: xs => perms(xs).map(as => interleave(x, as)).flatten

val test3 = subs(List(1, 2, 3)) // [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
val test4 = interleave(1, List(2, 3, 4)) // [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
val test5 = perms(List(1, 2, 3)) // [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

def choices[A](la: List[A]): List[List[A]] = subs(la).map(slas => perms(slas)).flatten

val test6 = choices(List(1,2,3)) // --[[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

def solution(e: Expr, ns: List[Int], n: Int): Boolean = choices(ns).contains(values(e)) && eval(e) == List(n)

val e = Expr.App(Op.Add, Expr.Val(1), Expr.Val(50))
val test7 = solution(e, List(1,3,7,10,25,50), 765)

//-- Brute force solution
def split[A](la: List[A]): List[(List[A], List[A])] = la match {
  case Nil     => Nil
  case List(_) => Nil
  case x :: xs => (List(x), xs) :: (for
    (ls,rs) <- split(xs)
    yield (x :: ls, rs))
}
//split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls,rs) <- split xs]
val test8 = split(List(1,2,3,4)) // -- [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

def exprs(is: List[Int]): List[Expr] = is match
  case Nil     => Nil
  case List(n) => List(Expr.Val(n))
  case ns      => 
    for
      (ls,rs) <- split(ns)
      l       <- exprs(ls)
      r       <- exprs(rs)
      e       <- combine(l, r)
    yield e

def combine(l: Expr, r: Expr): List[Expr] = ops.map(o => Expr.App(o, l, r))

def ops = List(Op.Add, Op.Sub, Op.Mul, Op.Div)

def solutions(ns: List[Int], n: Int): List[Expr] =
  for
    ns0 <- choices(ns)
      e <- exprs(ns) if eval(e) == List(n)
  yield e

//-- Combining generation and evaluation
type Result = (Expr, Int)

def results(is: List[Int]): List[Result] = is match
  case Nil      => Nil
  case n :: Nil => if n > 0 then List((Expr.Val(n), n)) else Nil
  case ns       =>
    for
      (ls, rs) <- split(ns)
      lx       <- results(ls)
      ry       <- results(rs)
      res      <- combine0(lx, ry)
    yield res

def combine0(r1: Result, r2: Result): List[Result] = (r1, r2) match
  case ((l, x), (r, y)) =>
    for
      o <- ops if valid0(o, x, y)
    yield (Expr.App(o, l, r), apply(o, x, y))

def solutions0(ns: List[Int], n: Int): List[Expr] =
  for
    ns0   <- choices(ns)
    (e,m) <- results(ns0) if m == n
  yield e

//-- Exploring algebraic properties
def valid0(op: Op, x: Int, y: Int): Boolean = op match
  case Op.Add => x <= y
  case Op.Sub => x > y
  case Op.Mul => x != 1 && y != 1 && x <= y
  case Op.Div => y != 1 && x % y == 0

@main def program: Unit =
//  println(s"test0 $test0")
//  println(s"test1 $test1")
//  println(s"test2 $test2")
//  println(s"test3 $test3")
//  println(s"test4 $test4")
//  println(s"test5 $test5")
//  println(s"test6 $test6")
//  println(s"test7 $test7")
//  println(s"test8 $test8")
//  println(s"main ${solutions(List(1,3,7,10,25,50), 765)}")
//  println(solutions0(List(1,3,7,10,25,50), 765))
  println(solutions0(List(1,3,7,10,25,50), 765).map(summon[Show[Expr]].show))
