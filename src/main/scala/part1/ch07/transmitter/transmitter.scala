package part1.ch07.transmitter

//-- 7.6 Binary string part1.ch07.transmitter
//-- Base conversion
type Bit = Int

//bin2int0 bits = sum [w*b | (w,b) <- zip weights bits]
//                where weights = iterate (*2) 1
def bin2Int0(bits: List[Bit]): Int = {
  def weights = ???
//  for
//    (w, b) <- weights.zip(bits)
//  yield w * b
  ???
}

//val test0 = bin2Int0(List(1,0,1,1))

def bin2int(bits: List[Bit]): Int = bits.foldRight(0)((x, y) => x + 2 * y)

def int2bin: Int => List[Bit] = (i: Int) => i match //
  case 0 => Nil
  case n => n % 2 :: int2bin(n / 2)

val test1 = int2bin(13)

def make8: List[Bit] => List[Bit] = (bits: List[Bit]) => (bits ++ LazyList.continually(0)).take(8)

val test2 = make8(List(1,0,1,1))

//-- Transmission

def encode1: String => List[Bit] = //s.toList.flatMap(c => make8(int2bin(c.toInt)))
  (s: String) => s.toList.flatMap(c => make8.compose(int2bin)(c.toInt))

//encode1 = concat . map (make8 . int2bin . ord)
val test3 = encode1("abc") //-- [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

//chop8 :: [Bit] -> [[Bit]]
//chop8 [] = []
//chop8 bits = take 8 bits : chop8 (drop 8 bits)
def chop8(bs: List[Bit]): List[List[Bit]] = bs match
  case Nil  => Nil
  case bits => (bits :: chop8(bits.drop(8))).take(8)
//
//decode :: [Bit] -> String
//  decode = map (chr . bin2int) . chop8
def chr = (i: Int) => i.toChar 
def decode: List[Bit] => String = (bs: List[Bit]) => ???
//  bs.map(chr compose bin2int.compose(chop8))

//
//test4 :: String
val test4 = decode(List(1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0)) //-- "abc"
//
//transmit :: String -> String
//transmit = decode . channel . encode1
def transmit: String => String = decode.compose(channel).compose(encode1)

//channel :: [Bit] -> [Bit]
//channel = id
def channel: List[Bit] => List[Bit] = (bs: List[Bit]) => identity(bs)

//test5 :: String
val test5 = transmit("higher-order functions are easy") //-- "higher-order functions are easy"
