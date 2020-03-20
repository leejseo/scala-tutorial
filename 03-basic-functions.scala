import scala.annotation.tailrec

trait List
case object Nil extends List
case class Cons(head: Int, tail: List) extends List

// Example 1
def inc1(x: Int): Int = {
    x+1
}

// val pp = inc1 // error
val pp = inc1 _

//println(pp(0), pp(1), pp(2))

// Example 2

def id(x: Int): Int = x

def y_intercept(g: Int => Int): Int = {
    g(0)
}

//println(y_intercept(id), y_intercept(inc1))

// Example 3

def compose(f: Int => Int, g: Int => Int): Int => Int = {
    ((x: Int) => f(g(x)))
}

//println(compose(inc1, inc1)(0), compose(inc1, id)(0))

// Example 4
val mult = (_: Int) * (_: Int)

//println(mult(2, 3))

// Example 5
def list_modify(l: List, f: Int => Int): List = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), list_modify(t, f))
}

val l = Cons(0, Cons(1, Cons(2, Cons(3, Nil))))

val list_inc1 = (l: List) => list_modify(l, (h: Int) => h+1)
val list_sq = (l: List) => list_modify(l, (h: Int) => h*h)
val list_incN = (l: List, x: Int) => list_modify(l, (h: Int) => h+x)

//println(list_inc1(l), list_sq(l))
//println(list_incN(l, 1), list_incN(l, 5))

// Example 6
def list_filter(l: List, f: Int => Boolean): List = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, list_filter(t, f)) else list_filter(t, f)
}

val filter_odd = (l: List) => list_filter(l, _ % 2 == 1)
val filter_positive = (l: List) => list_filter(l, _ > 0)

//println(filter_odd(l), filter_positive(l))

// Example 7
def list_fold(l: List, id: Int, commutative_ftn: (Int, Int) => Int): Int = {
    @tailrec def aux(l: List, inter: Int): Int = l match {
        case Nil => inter
        case Cons(h, t) => aux(t, commutative_ftn(inter, h))
    }
    aux(l, id)
}

val list_sum = (l: List) => list_fold(l, 0, _+_)
val list_prod = (l: List) => list_fold(l, 1, _*_)
val list_len = (l: List) => list_fold(l, 0, (x, y) => x+1)

//println(list_sum(l))
//println(list_prod(l), list_prod(filter_positive(l)))
//println(list_len(l), list_len(filter_odd(l)))

// Example 8
trait Option
case object None extends Option
case class Some(n: Int) extends Option

def list_access_option(l: List, idx: Int): Option = {
    if (idx < 0) None
    else l match {
        case Nil => None
        case Cons(h, t) =>
            if (idx == 0) Some(h)
            else list_access_option(t, idx-1)
    }
}

//println(list_access_option(l, 0))
//println(list_access_option(l, 1))
//println(list_access_option(l, -1))
//println(list_access_option(l, 4))

def option_map(o: Option, f: Int => Int): Option = o match{
    case None => None
    case Some(n) => Some(f(n))
}

val int_sq = (x: Int) => x*x

//println(option_map(list_access_option(l, -1), int_sq))
//println(option_map(list_access_option(l, 2), int_sq))

