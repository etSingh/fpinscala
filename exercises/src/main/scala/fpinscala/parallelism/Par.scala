package fpinscala.parallelism

import java.util.concurrent._

import scala.annotation.tailrec
import scala.language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there
  // are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => choices(run(es)(n).get)(es)

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(Par.map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => choices(run(es)(key).get())(es)

  // AKA flatmap
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(run(es)(pa).get())(es)

  // Reimplementing using chooser

  def choiceV2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(boolean => if (boolean) t else f)

  def choiceNV2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(index => choices(index))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    chooser(key)(key => choices(key))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(a).get()(es)

  def joinUsingFlatmap[A](a: Par[Par[A]]): Par[A] =
    chooser(a)(a => { // or simply a => a (identity?)
      es =>
        run(es)(a)
    })

  def flatmapUsingJoin[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    join(map(pa)(choices))

  def map2UsingPrimitives[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    val pbc: Par[B => C] = chooser(a)(a => unit(f(a, _)))

    //chooser(b)(b => map(pbc)(fn => fn(b))) // map uses map2 so this isn't ideal

    chooser(b)(b => {
      es => unit(run(es)(pbc).get()(b))(es)
    })
  }


  // My solution
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    (es: ExecutorService) =>

      @tailrec
      def compute(p: List[Par[A]], acc: List[A]): List[A] = p match {
        case x :: xs => compute(xs, x(es).get() :: acc)
        case Nil => acc
      }

      UnitFuture(compute(ps, Nil))
  }

  // Using other primitives
  def sequence_better[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(Nil))((head, tail) => map2(head, tail)((x: A, xs: List[A]) => x :: xs))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val list: List[Par[B]] = ps.map(asyncF(f)) // asyncF forks parallel computations
    sequence(list)
  }

  // Tough
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      as.map(a => asyncF((x: A) => if (f(x)) List(x) else Nil)(a))

    val seq: Par[List[List[A]]] = sequence(pars)

    map(seq)(_.flatten)
  }

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = {
    val intermediateFunction: Par[C => D] = map2(pa, pb)((a, b) => f(a, b, _))
    map2(intermediateFunction, pc)((fn, c) => fn(c))
  }

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }

}

object Examples {

  import Par._

  def sum(ints: IndexedSeq[Int]): Int = { // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    println(ints) // Just to see what happens in main when this is called :)
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }
  }

  def sumPar(ints: IndexedSeq[Int]): Par[Int] = operateOnSeqPar(ints)(sum)(_ + _)

  // How do you make this parallel?
  def maxValueSeq(ints: IndexedSeq[Int]): Int = {

    @tailrec
    def loop(seq: IndexedSeq[Int], maxValue: Int): Int = {
      if (seq.isEmpty) maxValue
      else if (seq.head > maxValue) loop(seq.tail, seq.head)
      else loop(seq.tail, maxValue)
    }

    if (ints.isEmpty) 0
    else loop(ints.tail, ints.head)
  }

  // This can be generalized into maxValueParGeneral
  def maxValuePar(ints: IndexedSeq[Int]): Par[Int] = {
    val (l, r) = ints.splitAt(ints.length / 2)
    Par.map2(Par.asyncF(maxValueSeq)(l), Par.asyncF(maxValueSeq)(r))((a, b) => if (a > b) a else b)
  }

  def maxValueParGeneral(ints: IndexedSeq[Int]): Par[Int] = operateOnSeqPar(ints)(maxValueSeq)((a, b) => if (a > b) a else b)

  def operateOnSeqPar[A, B](ints: IndexedSeq[A])(compute: IndexedSeq[A] => B)(result: (B, B) => B): Par[B] = {
    val (l, r) = ints.splitAt(ints.length / 2)
    Par.map2(Par.asyncF(compute)(l), Par.asyncF(compute)(r))(result)
  }

  def countWordsSeq(paragraphs: List[String]): Int =
    paragraphs.foldLeft(0)((totalWords, paragraph) => totalWords + paragraph.split(" ").length)

  def countWordsPar(paragraphs: List[String]): Par[Int] =
    paragraphs.splitAt(paragraphs.length / 2) match {
      case (left, right) => Par.map2(Par.asyncF(countWordsSeq)(left), Par.asyncF(countWordsSeq)(right))(_ + _)
    }


  def main(args: Array[String]): Unit = {
    println(Par.run(new ForkJoinPool())(sumPar(IndexedSeq(1, 2, 3, 4))))
    println("---------")
    println(sum(IndexedSeq(1, 2, 3, 4)))
    val a = lazyUnit(42 + 1)
    deadlockMe(10, a)

    def deadlockMe[A](threads: Int, computation: Par[A]): Unit = {
      val wrapInFork: Par[A] = (1 to threads).foldLeft(computation)((c, _) => {
        fork(c)
      })
      println("Starting deadlock")
      Par.equal[A](Executors.newFixedThreadPool(threads))(computation, wrapInFork)
      println("deadlock over")
    }
  }


}
