import scala.annotation.tailrec

@tailrec
def last(theList : List[Int]) : Int = {
  theList match  {
    case x :: Nil => x
    case _ => last(theList.tail)
  }
}