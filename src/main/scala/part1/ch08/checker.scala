package part1.ch08

import part1.ch07.transmitter.rmdups

type Assoc[K, V] = List[(K, V)]
//find0 :: Eq k => k -> Assoc k v -> v
//find0 k t = head [v | (k',v) <- t, k == k']
def find0[K, V](k: K, t: Assoc[K, V]) =
  t.flatMap { case (k0, v) => if k == k0 then List(v) else Nil }.head

//-- 8.6 Tautology checker
//data Prop = Const Bool
//  | Var Char
//| Not Prop
//| And Prop Prop
//  | Imply Prop Prop
enum Prop:
  case Const(b: Boolean)
  case Var(c: Char)
  case Not(p: Prop)
  case And(p1: Prop, p2: Prop)
  case Imply(p1: Prop, p2: Prop)

//
//-- A & -A
//p1 :: Prop
val p1 = Prop.And(Prop.Var('A'), Prop.Not(Prop.Var('A')))
//
//-- (A & B) => A
//p2 :: Prop
val p2 = Prop.Imply(Prop.And(Prop.Var('A'), Prop.Var('B')), Prop.Var('A'))
//
//-- A => (A & B)
//p3 :: Prop
val p3 = Prop.Imply(Prop.Var('A'), Prop.And(Prop.Var('A'), Prop.Var('B')))
//
//--
//p4 :: Prop
val p4 = Prop.Imply(
  Prop.And(
    Prop.Var('A'), Prop.Imply(Prop.Var('A'), Prop.Var('B'))
  ),
  Prop.Var('B')
)

//
//type Subst = Assoc Char Bool
type Subst = Assoc[Char, Boolean]
//type Subst = (Char, Boolean)
//
//eval :: Subst -> Prop -> Bool
//eval _ (Const b)   = b
//eval s (Var x)     = find0 x s
//eval s (Not p)     = not (eval s p)
//eval s (And p q)   = eval s p && eval s q
//eval s (Imply p q) = eval s p <= eval s q
def eval(s: Subst, p: Prop): Boolean = p match
  case Prop.Const(b)    => b
  case Prop.Var(x)      => find0(x, s)
  case Prop.Not(p)      => !eval(s, p)
  case Prop.And(p, q)   => eval(s, p) && eval(s, q)
  case Prop.Imply(p, q) => eval(s, p) <= eval(s, q)
//
//vars :: Prop -> [Char]
//vars (Const _)   = []
//vars (Var x)     = [x]
//vars (Not p)     = vars p
//vars (And p q)   = vars p ++ vars q
//vars (Imply p q) = vars p ++ vars q
def vars(p: Prop): List[Char] =  p match
  case Prop.Const(_)    => Nil
  case Prop.Var(x)      => List(x)
  case Prop.Not(p)      => vars(p)          
  case Prop.And(p, q)   => vars(p) ++ vars(q)
  case Prop.Imply(p, q) => vars(p) ++ vars(q)

val test13 = bools(3)
//-- [[False,False,False],
//  --  [False,False,True],
//  --  [False,True,False],
//  --  [False,True,True],
//  --  [True,False,False],
//  --  [True,False,True],
//  --  [True,True,False],
//  --  [True,True,True]]
//
//bools0 :: Int -> [[Bool]]
//bools0 n = map (reverse . map conv . make n . int2bin) range
//where
//range     = [0..(2^n)-1]
//make n bs = take n (bs ++ repeat 0)
//conv 0    = False
//conv 1    = True
//
//bools :: Int -> [[Bool]]
//bools 0 = [[]]
//bools n = map (False:) bss ++ map (True:) bss
//where bss = bools (n-1)
def bools(i: Int): List[List[Boolean]] =
  def bss(n: Int) = bools(n - 1)
  i match
    case 0 => Nil
    case n => ???


//substs :: Prop -> [Subst]
//substs p = map (zip vs) (bools (length vs))
//where vs = rmdups (vars p)
def substs(p: Prop): List[Subst] =
  def vs = rmdups(vars(p))
  ???

//
val test14 = substs(p2) //-- [('A',False),('B',False)],[('A',False),('B',True)],[('A',True),('B',False)],[('A',True),('B',True)]]
//
//isTaut :: Prop -> Bool
//isTaut p = and [eval s p | s <- substs p]
def isTaut(p: Prop): Boolean =
  substs(p)
    .map(s => eval(s, p))
    .foldRight(true)(_ && _)

val test15 = isTaut(p1) //-- False
val test16 = isTaut(p2) //-- True
val test17 = isTaut(p3) //-- False
val test18 = isTaut(p4) //-- True