package part2.ch10.life

import zio._
import zio.Task
import zio.URIO
import zio.console._

// Screen utilities
val cls: zio.URIO[zio.console.Console, Unit] = putStr("\033[2J")
type Pos = (Int, Int)

def writeat(p: Pos, xs: String): zio.ZIO[zio.console.Console, Nothing, Unit] =
  for
    _ <- goto(p)
    _ <- putStr(xs)
  yield ()

import part1.ch09.countdown.Show
import part1.ch09.countdown.{ given Show[Int] }
def goto(p: Pos): URIO[zio.console.Console, Unit] = p match
  case (x, y) => putStr("\033[" + s"${summon[Show[Int]].show(y)};${summon[Show[Int]].show(x)}H")
  
// Game of part2.ch10.life
val width: Int = 10
val height: Int = 10

type Board = List[Pos]

val glider: Board = List((4,2),(2,3),(4,3),(3,4),(4,4))

def showcells(b: Board): zio.ZIO[zio.Has[zio.console.Console.Service], Nothing, Unit] =
  ZIO.collectAll(for
    p <- b
  yield writeat(p, "O")).map(_ => ())

def isAlive(b: Board, p: Pos): Boolean = b.contains(p)

def isEmpty(b: Board, p: Pos): Boolean = ! isAlive(b, p)

def neighbs(p: Pos): List[Pos] = p match
  case (x, y) =>
    List(
      (x - 1, y - 1), (x, y - 1),
      (x + 1, y - 1), (x - 1, y),
      (x + 1, y), (x - 1, y + 1),
      (x, y + 1), (x + 1, y + 1)).map(wrap)

def wrap(p: Pos): Pos = p match
  case (x, y) => (((x - 1) % width) + 1, ((y - 1) % height) + 1)

def liveneighbs(b: Board, p: Pos): Int = neighbs(p).filter(np => isAlive(b, np)).length

def survivors(b: Board): List[Pos] =
  for
    p <- b
    if List(2, 3).contains(liveneighbs(b, p))
  yield p

def births(b: Board): List[Pos] =
  for
    p <- rmdups(b.map(neighbs).flatten)
    if isEmpty(b, p)
    if liveneighbs(b, p) == 3
  yield p

def rmdups[A](la: List[A]): List[A] = la match
  case Nil     => Nil
  case x :: xs => x :: rmdups(xs.filter(_ != x))

def nextgen(b: Board): Board = survivors(b) ++ births(b)

def life(b: Board): zio.ZIO[zio.console.Console, Throwable, Unit] =
  for
    _ <- cls
    _ <- showcells(b)
    _ <- wait1(500000)
    _ <- life(nextgen(b))
  yield ()
  

def wait1(n: Int): Task[Unit] =
  ZIO.collectAll(for
    _ <- (1 to n).toList
  yield ZIO.succeed(())).map(_ => ())

object Main extends zio.App:
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] =
    (for
      _ <- life(glider)
    yield ExitCode.success) orElse ZIO.succeed(ExitCode.failure)
