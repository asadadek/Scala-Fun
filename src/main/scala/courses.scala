import scala.collection.mutable.ListBuffer
import scala.io.StdIn
case class DayOfWeek(val abbr: String) extends AnyVal
case class Course(name: String, offeredOn: Set[DayOfWeek])

case class IO[+A](val ra: () => A){
  def unsafePerformIO() : A = ra();
  def map[B](f: A => B) : IO[B] = IO[B]( () => f(unsafePerformIO()))
  def flatMap[B](f: A => IO[B]) : IO[B] = {
    IO( () =>  f(ra()).unsafePerformIO())
  }
}
case class CourseCombo(courses: Set[Course], days: Set[DayOfWeek])
object Main extends App {
  def readLine() = IO[String](() => StdIn.readLine())

  def writeLine(msg: String) = IO[Unit](() => println(msg))

  val courses = List(
    Course("OOPJ", Set(DayOfWeek("MON"), DayOfWeek("TUE"), DayOfWeek("WED"))),
    Course("DSAL", Set(DayOfWeek("TUE"), DayOfWeek("THU"), DayOfWeek("FRI"))),
    Course("WEBP", Set(DayOfWeek("MON"), DayOfWeek("TUE"), DayOfWeek("THU"))),
    Course("NETW", Set(DayOfWeek("MON"), DayOfWeek("TUE"), DayOfWeek("FRI")))
  )


  def combinations(courses: List[Course], comboSize: Int): List[CourseCombo] = {

    def genCombos(lst: List[Course], acc: List[CourseCombo]): List[CourseCombo] =
      lst match {
        case x :: xs => genCombos(xs, combos(x :: Nil, xs, acc))
        case Nil => acc
      }


    def combos(first: List[Course], second: List[Course], acc: List[CourseCombo]): List[CourseCombo] =
       if (first.size == comboSize) {
        CourseCombo(first.toSet,first.foldLeft(Set.empty[DayOfWeek])((days,crs) => days.union(crs.offeredOn))) :: acc
      } else if (second.isEmpty){
        acc
      } else {
          combos(first,second.tail,combos(first ::: (second.head :: Nil), second.tail, acc))
      }

    genCombos(courses, Nil)
  }

  val combos = combinations(courses, 2).reverse

  combos map {
    combo => combo.courses map { c : Course => print(c.name+",")}; println(combo.days)
  }
}