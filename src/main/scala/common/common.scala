package common

def count(x: Char, xs: String): Int =
  xs.toList.filter(x0 => x == x0).length