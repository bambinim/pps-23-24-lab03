package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u02.AlgebraicDataTypes.Person

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()
    
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = first match
      case Nil() => Nil()
      case Cons(h1, t1) => second match
        case Nil() => Nil()
        case Cons(h2, t2) => Cons((h1, h2), zip(t1, t2))

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = n match
      case 0 => Nil()
      case _ => l match
        case Cons(h, t) => Cons(h, take(t)(n - 1))
        case _ => Nil()
    
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Cons(h, t) => t match
        case Cons(_, _) => Cons(h, concat(t, l2))
        case _ => Cons(h, l2)
      case _ => l2
    
    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(h, t) => mapper(h) match
        case Cons(h, _) => Cons(h, flatMap(t)(mapper))
        case _ => flatMap(t)(mapper)
      case _ => Nil()

    def min(l: Sequence[Int]): Optional[Int] =
      def getMin(l: Sequence[Int], currentMin: Optional[Int]): Optional[Int] = l match
        case Cons(h, t) => currentMin match
          case Optional.Just(x) if x < h => getMin(t, Optional.Just(x))
          case _ => getMin(t, Optional.Just(h))
        case _ => currentMin
      getMin(l, Optional.Empty())

    extension (l: Sequence[Person])
      def teachersCourses(): Sequence[String] =
        flatMap(l)(_ match
          case Person.Teacher(_, c) => Cons(c, Nil())
          case _ => Nil()
        )

    extension [A](l: Sequence[A])
      def foldLeft[B](acc: B)(operator: (B, A) => B): B = l match
        case Cons(h, t) => t.foldLeft(operator(acc, h))(operator)
        case _ => acc
      

@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
