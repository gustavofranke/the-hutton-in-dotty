package part2.ch17

enum Expr:
  case Val(i: Int)
  case Add(e1: Expr, e2: Expr)

def eval0(e: Expr): Int = e match
  case Expr.Val(n)    => n
  case Expr.Add(x, y) => eval0(x) + eval0(y)

val a = eval0(Expr.Add(Expr.Val(1), Expr.Val(2))) // applying eval0
val b = eval0(Expr.Val(1)) + eval0(Expr.Val(2)) // applying the first eval
val c = 1 + eval0(Expr.Val(2)) // applying the first eval
val d = 1 + 2 // applying  eval
val e = 3

// 17.3 Adding a stack
type Stack = List[Int]

def eval1(e: Expr, s: Stack): Stack = e match
  case Expr.Val(n)    => push(n, s)
  case Expr.Add(x, y) => add(eval1(y, eval1(x, s)))

def push(n: Int, s: Stack): Stack = n :: s

def add(s: Stack): Stack = s match
  case n :: m :: s => (n + m) :: s

// property: eval' e s = eval e : s
// substituting the empty stack s = [] into the equation eval' e s = eval e : s
def eval(e: Expr): Int = eval1(e, Nil).head

val f = eval(Expr.Add(Expr.Val(1), Expr.Val(2))) // applying eval
val g = eval1(Expr.Add(Expr.Val(1), Expr.Val(2)), Nil).head // applying eval1
val h = add(eval1(Expr.Val(2), eval1(Expr.Val(1), Nil))).head // applying inner eval1
val i = add(eval1(Expr.Val(2), push(1, Nil))).head // applying push
val j = add(push(2, 1 :: Nil)).head // applying push
val k = add(2 :: 1 :: Nil).head // applying add
val l = (2 + 1 :: Nil).head // applying head
val m = 3
