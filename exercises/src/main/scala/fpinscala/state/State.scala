package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  /**
    * Anything returning this type alias will take RNG as input even if it's
    * not specified as an input variable in the definition of the function.
    */
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num1, rng1) = rng.nextInt
    if (num1 > 0) (num1, rng1)
    else if (num1 == Integer.MIN_VALUE) nonNegativeInt(rng1)
    else (-num1, rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (num1, rng1) = nonNegativeInt(rng)
    (toDoubleLessThanOne(num1), rng1)
  }

  def doubleElegant: Rand[Double] = map(nonNegativeInt)(toDoubleLessThanOne)

  // Converting an Int to a Double
  def toDoubleLessThanOne(n: Int): Double = {
    def length(n: Int)(l: Int): Double = {
      if (n <= 0) Math.pow(10, l)
      else length(n / 10)(l + 1)
    }
    n / length(n)(0)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int1, rng1) = nonNegativeInt(rng)
    val (double1, rng2) = double(rng1)
    ((int1, double1), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((int1, double1), rng1) = intDouble(rng)
    ((double1, int1), rng1)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, rng1) = double(rng)
    val (double2, rng2) = double(rng1)
    val (double3, rng3) = double(rng2)
    ((double1, double2, double3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (Nil, rng)
    else {
      val (i, rng1) = rng.nextInt
      val (res, rngN) = ints(count - 1)(rng1)
      (List(i) ::: res, rngN)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rnd => {
      val (v1, rnd1) = ra(rnd)
      val (v2, rnd2) = rb(rnd1)
      (f(v1, v2), rnd2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = {
    map2(ra, rb)((a, b) => (a, b)) // In this case the return type of map2 'C' is (A, B): Rand[]
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs match {
      case Nil => rnd => (Nil, rnd)
      case x :: xs => map2(x, sequence(xs))((x, xs) => x :: xs)
    }

  def sequenceWithFold[A](fs: List[Rand[A]]): Rand[List[A]] = {
    val unit: Rand[List[A]] = rnd => (Nil, rnd)
    fs.foldLeft(unit)((remainingList, head) => map2(head, remainingList)((x, xs) => x :: xs))
  }

  def intsBetter(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def mapUsingFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2UsingFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => {
      rng => {
        val (b, rng_b) = rb(rng)
        (f(a, b), rng_b)
      }
    })
}

case class State[S,+A](run: S => (A, S)) {

  /**
    * if state was instead a type alias
    * type State[S, A] = S => (A, S)
    * the fun signature would have been different
    * see page 88
    */
  def map[B](f: A => B): State[S, B] =
    State(input => {
      // Notice how the run method is called here
      // if State was a type alias, it would have to
      // be passed in the method as (f: S => (A, S))
      val (a, output) = run(input)
      (f(a), output)
    })


  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(
      input => {
        val (b, output1) = sb.run(input)
        val (a, output2) = run(output1)
        (f(a, b), output2)
      }
    )


  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      input => {
        val (a, output1) = run(input)
        f(a).run(output1)
      }
    )
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(input => (a, input))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldLeft(unit[S, List[A]](List()))((remainingList, head) => remainingList.map2(head)((x, xs) => xs :: x))
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def areCandiesLeft: Boolean = candies > 0
}

object CandyMachine {

  // Check out the answers for the super functional version of this
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(
    machine => {
      inputs match {
        case Nil => ((machine.coins, machine.candies), machine)
        case x :: xs =>
          val nextMachineState = getNextCandyMachineState(x)(machine)
          // Notice how .run takes in a machine as input! It's because run
          // is just a lambda from S => (A, S) so it needs to take S as it's
          // input
          simulateMachine(xs).run(nextMachineState)
      }
    }
  )

  def getNextCandyMachineState(input: Input): Machine => Machine =
    machine => {
      input match {
        case Coin =>
          if (machine.locked && machine.areCandiesLeft)
            Machine(locked = false, machine.candies, machine.coins + 1)
          else
            machine
        case Turn =>
          if (!machine.locked && machine.areCandiesLeft)
            Machine(locked = true, machine.candies - 1, machine.coins)
          else
            machine
      }
    }

}

object Main extends App {

  import fpinscala.state.RNG.{Rand, flatMap, nonNegativeInt, toDoubleLessThanOne}


  def double2: Rand[Double] = RNG.mapUsingFlatMap(nonNegativeInt)(toDoubleLessThanOne)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {
      nonNegativeInt => { rnd =>
        val mod = nonNegativeInt % n
        if (nonNegativeInt + (n - 1) - mod >= 0) (mod, rnd)
        else nonNegativeLessThan(n)(rnd)
      }
    }

  def rollDice: Rand[Int] = nonNegativeLessThan(6)

  println(RNG.doubleElegant(RNG.Simple(2L)))
  println(double2(RNG.Simple(2L)))
  println(nonNegativeLessThan(88)) // will return a lambda because this still needs an input RND
  println(nonNegativeLessThan(88)(RNG.Simple(76575)))

  val candyMachine = Machine(locked = true, 5, 10)

  val input = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

  println(CandyMachine.simulateMachine(input).run(candyMachine))
}