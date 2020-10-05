package part2.ch10.hangman

import zio._
import zio.Task
import zio.console._

def putChar(a: => Char): Task[Unit] = Task.succeed(print(a))
//def getChar: Task[Char] = Task.succeed(scala.io.StdIn.readChar)

val hangman: ZIO[Console, Throwable, Unit] =
  for
    _    <- putStrLn("Think of a word:")
    word <- getStrLn
//    word <- sgetLine
    _    <- putStrLn("Try to guess it:")
    _    <- play(word)
  yield ()  

//val sgetLine: Task[String] =
//  for
//    x <- getChar
//    r <- if x == '\n'
//         then (for
//                 _ <- putChar(x) 
//               yield "")
//         else (for
//                 _  <- putChar('+')
//                 xs <- sgetLine
//               yield s"$x$xs")
//  yield r
         
//val getCh: Task[Char] = getChar

def play(word: String): ZIO[Console, Throwable, Unit] =
  for
    _     <- putStrLn("? ")
    guess <- getStrLn
    _ <- if guess == word then putStrLn("You got it!!")
         else (for
                _ <- putStrLn(match0(word, guess)) 
                _ <- play(word)
              yield ())
  yield ()

def match0(xs: String, ys: String): String =
  for
    x <- xs
  yield if ys.contains(x) then x else '-'

object Main extends zio.App:
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] =
    hangman.map(_ => ExitCode.success) orElse ZIO.succeed(ExitCode.failure)