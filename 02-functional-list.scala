import scala.annotation.tailrec

trait List
case object Nil extends List
case class Cons(head: Int, tail: List) extends List

def f(l : List) = l match{
    case Nil => "The empty list"
    case Cons(h, t) => "A pair of an integer and a list"
}

// println(f(Cons(1, Cons(2, Nil))))

def incl(l: List): List = l match{
    case Nil => Nil
    case Cons(h, t) => Cons(h+1, incl(t))
}

val l = Cons(0, Cons(1, Cons(2, Cons(3, Nil))))
//println(incl(l))

def square(l: List): List = l match{
    case Nil => Nil
    case Cons(h, t) => Cons(h*h, square(t))
}

//println(square(l))

def odd(l: List): List = l match{
    case Nil => Nil
    case Cons(h, t) => if (h % 2 == 1) Cons(h, odd(t)) else odd(t)
}

//println(odd(l))

def positive(l: List): List = l match{
    case Nil => Nil
    case Cons(h, t) => if (h > 0) Cons(h, positive(t)) else positive(t)
}

//println(positive(l))

def length(l: List): Int = l match{
    case Nil => 0
    case Cons(h, t) => 1 + length(t)
}

//println(length(l))

def sum(l: List): Int = l match{
    case Nil => 0
    case Cons(h, t) => h + sum(t)
}

//println(sum(l))

def product(l: List): Int = l match{
    case Nil => 1
    case Cons(h, t) => h * product(t)
}

//println(product(l))

def addBack(l: List, x: Int): List = l match{
    // Inefficient; O(length)
    case Nil => Cons(x, Nil)
    case Cons(h, t) => Cons(h, addBack(t, x))
}

//println(addBack(l, 4))

def factorial(n: Int, inter: Int): Int =
    if (n <= 0) inter else factorial(n-1, inter*n)

//println(factorial(10, 1))

def length_tail(l: List): Int = {
    /*@tailrec*/ def aux(l: List, inter: Int): Int = l match{
        case Nil => inter
        case Cons(h, t) => aux(t, inter+1)
    }
    aux(l, 0)
}

//println(length_tail(l))

def sum_tail(l: List): Int = {
    def aux(l: List, inter: Int): Int = l match{
        case Nil => inter
        case Cons(h, t) => aux(t, inter+h)
    }
    aux(l, 0)
}

//println(sum_tail(l))

def reverse(l: List): List = {
    @tailrec def aux(l: List, inter: List): List = l match{
        case Nil => inter
        case Cons(h, t) => aux(t, Cons(h, inter))
    }
    aux(l, Nil)
}

def incl_tail(l: List): List = {
    @tailrec def aux(l: List, inter: List): List = l match{
        case Nil => inter
        case Cons(h, t) => aux(t, Cons(h+1, inter))
    }
    reverse(aux(l, Nil))
}

//println(incl_tail(l))

