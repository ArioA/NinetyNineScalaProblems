import scala.annotation.tailrec

/* Problem 1
*
*  Returns last element of a list,
*  or None if list is empty.
* */
@tailrec
def last[T](theList : List[T]) : Option[T] = {
  theList match  {
    case Nil => None
    case x :: Nil => Some(x)
    case _ => last(theList.tail)
  }
}

last(List(1,2,3,4,5,6))
last(List('a','b'))
last(List())

/* Problem 2
*
*  Returns the penultimate element of a list,
*  returns None if no such element exists -
*  i.e. if list contains 0 or 1 elements.
* */
@tailrec
def penultimate[T](theList : List[T]) : Option[T] = {
  theList match {
    case Nil => None
    case _ :: Nil => None
    case x :: _ :: Nil => Some(x)
    case _ => penultimate(theList.tail)
  }
}

penultimate(List(1,2,3,4,5))
penultimate(List('a','b'))
penultimate(List("singleton"))
penultimate(List())

/* Problem 3
*
*  Returns the Kth element of a list.
*  Returns None if K > list.length
* */
@tailrec
def kth[T](n : Int, theList : List[T]) : Option[T] = {
  if (theList.isEmpty) return None
  n match {
    case i:Int if i < 1 => None
    case i:Int if i == 1 => Some(theList.head)
    case i:Int if i > 1 => kth(n-1, theList.tail)
  }
}

kth(1, List(4,5,6))
kth(4, List('a','b','c','d'))
kth(1,List())
kth(10, List("one", "two"))

/*  Problem 4
*
*   Returns number of elements in a list.
* */

def length[T](theList : List[T]) : Int = {
  @tailrec
  def lengthTailRec[T](sublist : List[T], lengthSoFar : Int) : Int = {
    sublist match {
      case Nil => lengthSoFar
      case _ => lengthTailRec(sublist.tail, lengthSoFar + 1)
    }
  }
  lengthTailRec(theList, 0)
}

length(List(1,2,3))
length(List())

/* Problem 5
*
*  Returns the reverse of it's argument
* */
def reverse[T](theList : List[T]) : List[T] = {
  @tailrec
  def reverseTailRec[T](argList : List[T], accList : List[T]) : List[T] =
  argList match {
    case Nil => accList
    case x :: _ => reverseTailRec(argList.tail, x :: accList)
  }

  reverseTailRec(theList, List())
}

reverse(List(1,2,3,4))
reverse(List())
reverse(List('a'))

def isPalindrome[T](theList: List[T]) : Boolean = {
  val reversedList: List[T] = reverse(theList)
  reversedList match {
    case `theList` => true
    case _ => false
  }
}

isPalindrome(List(1, 2, 3, 2, 1))
isPalindrome(List('a','b','c'))

def flatten(listOfLists: List[Any]): List[Any] = {


  def flattenTailRec(lists: List[Any], accumulatorList: List[Any]): List[Any] = {
      lists match {
        case Nil => accumulatorList
        case (x:List[Any]):: xs => flattenTailRec(xs, flattenTailRec(x, accumulatorList))
        case x :: xs => flattenTailRec(xs, x :: accumulatorList)
      }
    }

  flattenTailRec(listOfLists, List.empty).reverse
  }

flatten(List(List(1,2,List(3)),List(4,5,6)))
