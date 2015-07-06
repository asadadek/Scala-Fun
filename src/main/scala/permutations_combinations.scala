import scala.annotation.tailrec

object Permutations {

  def permutations[T](list: List[T]): List[List[T]] = {

    def permute[T](list:  List[T], start : Int, acc: List[List[T]]): List[List[T]] = {
      @tailrec
      def loop(list: List[T], index: Int, acc: List[List[T]]): List[List[T]] = {
        if (index == list.length ) {
          acc
        } else if (start == list.length - 1) {
          list :: acc
        } else {
          loop(list, index + 1, permute(swap(list, start, index), start + 1, acc))
        }
      }
      loop(list,start,acc)
    }

    permute(list,0,Nil).reverse
  }

  def swap[T](list: List[T], i : Int, j: Int):List[T] =
  if (list.isEmpty)
    list
  else if( i > list.size -1 || j > list.size -1 || i == j)
    list
  else {
    val f = Math.min(i,j)
    val s = Math.max(i,j)
    val leading = list.take(f)
    val trailing = list.drop(s+1)
    val middle = list.slice(f+1,s)
    leading:::(list(s)::middle):::(list(f)::trailing)
  }

}

object PnC extends App {
  println(Permutations.permutations((1 until 15 ).toList).size)
}