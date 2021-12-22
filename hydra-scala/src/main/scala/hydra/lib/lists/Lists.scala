package hydra.lib.lists


def concat[A](lists: Seq[Seq[A]]): Seq[A] = lists.fold(Seq.empty)((x, y) => x ++ y)

def head[A](l: Seq[A]): A = l.head

def intersperse[A](sep: A, l: Seq[A]): Seq[A] = (sep, l) match {
  case (_, Nil) => Nil
  case (_, Seq(x)) => Seq(x)
  case (s, y::ys) => y+:s+:intersperse(s, ys)
}

def intercalate[A](sep: Seq[A], l: Seq[Seq[A]]): Seq[A] = concat(intersperse(sep, l))

def last[A](l: Seq[A]): A = l.last

def length[A](l: Seq[A]): Int = l.length

def map[A, B](f: A => B, l: Seq[A]): Seq[B] = l.map(f)
