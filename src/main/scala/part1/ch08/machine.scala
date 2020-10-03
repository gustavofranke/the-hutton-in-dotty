package part1.ch08

//import part1.ch07.transmitter.Expr

//-- 8.7 Abstract machine
//data Expr = Val Int | Add Expr Expr
enum Expr:
  case Val(i: Int)
  case Add(e1: Expr, e2: Expr)

//value :: Expr -> Int
//value (Val n)= n
//value (Add x y) = value x + value y
def value(e: Expr): Int = e match
  case Expr.Val(n)    => n
  case Expr.Add(x, y) => value(x) + value(y)
//
type Cont = List[Op]
//
//data Op = EVAL Expr | ADD Int
enum Op:
  case EVAL(e: Expr)
  case  ADD(i: Int)

//
//eval0 :: Expr -> Cont -> Int
//eval0 (Val n)   c = exec c n
//eval0 (Add x y) c = eval0 x (EVAL y : c)
def eval0(e: Expr, c: Cont): Int = e match
  case Expr.Val(n)    => exec(c, n)
  case Expr.Add(x, y) => eval0(x, (Op.EVAL(y) :: c))

//exec :: Cont -> Int -> Int
//exec []           n = n
//exec (EVAL y : c) n = eval0 y (ADD n : c)
//exec (ADD n : c)  m = exec c (n+m)
def exec(c: Cont, n: Int): Int = c match
  case Nil             => n
  case Op.EVAL(y) :: c => eval0(y, (Op.ADD(n) :: c))
  case Op.ADD(n0) :: c => exec(c, (n0 + n))
//
//value0 :: Expr -> Int
//value0 e = eval0 e []
//
val test19 = eval0(
  (Expr.Add((Expr.Val(2)), (Expr.Val(3)))),
  List(Op.EVAL(Expr.Val(4)))
)