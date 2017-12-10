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

/*
Problem 6 -
Returns true if list is in palindrome. False, otherwise.

 */

def isPalindrome[T](theList: List[T]) : Boolean = {
  val reversedList: List[T] = reverse(theList)
  reversedList match {
    case `theList` => true
    case _ => false
  }
}

isPalindrome(List(1, 2, 3, 2, 1))
isPalindrome(List('a','b','c'))
isPalindrome(List('l','l'))
isPalindrome(List('a','l','l','a'))
isPalindrome(List('l'))

/*
Problem 7 -
Flattens a nested list structure.
Returns a single list, where all nested lists in the input list are
removed and all elements of those nested lists are in the returned list,
with their order of preserved.
 */

def flatten(listOfLists: List[Any]): List[Any] = {


  def flattenTailRec(lists: List[Any], accumulatorList: List[Any]): List[Any] = {
      lists match {
        case Nil => accumulatorList
        case (head:List[Any]):: tail => flattenTailRec(tail, flattenTailRec(head, accumulatorList))
        case head :: tail => flattenTailRec(tail, head :: accumulatorList)
      }
    }

  flattenTailRec(listOfLists, List.empty).reverse
  }

flatten(List(List(1,2,List(3)),List(4,5,6)))

/*
Problem 8 -
Eliminates consecutive elements in the input list.
 */

def compress[T](theList: List[T]) : List[T] = {
  if(theList.isEmpty)
    return theList

  @tailrec
  def compressTailRec(duplicateList: List[T], accList: List[T]) : List[T] = {
    duplicateList match {
      case Nil => accList
      case head :: tail if head == accList.head => compressTailRec(tail, accList)
      case head :: tail => compressTailRec(tail, head :: accList)
    }
  }

  compressTailRec(theList, List(theList.head)).reverse
}

compress(List('a','b','c','d','e'))
compress(List(1,1,1,2,2,2,3,3,4,4,5,5,6,7,8,8,8,8,8,9))
compress(List.empty)
compress(List(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,3,3,3,3,1,1,3))

/*
Problem 9 -
Pack consecutive duplicates into sub-lists.
 */

def pack[T](theList: List[T]) : List[List[T]] = {
  if(theList isEmpty)
    return List.empty

  @tailrec
  def packTailRec(originalList: List[T], accList: List[List[T]]) : List[List[T]] = {
    originalList match {
      case Nil => accList
      case head :: tail if head == accList.head.head => packTailRec(tail, (head :: accList.head) :: accList.tail)
      case head :: tail => packTailRec(tail, List(head) :: accList)
    }
  }

  packTailRec(theList.tail, List(List(theList.head))).reverse
}

pack(List('a','b','c','d','e'))
pack(List(1,1,1,2,2,2,3,3,4,4,5,5,6,7,8,8,8,8,8,9))
pack(List.empty)
pack(List(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,3,3,3,3,1,1,3))

/*
Problem 10 -
Encode consecutive elements of the input list as ( E, N ) tuples where E is the element and
N is the number of consecutive duplicates of E.
 */

def encode[T](theList: List[T]) : List[(Int, T)] = {

  val packedList : List[List[T]] = pack(theList)

  @tailrec
  def encodeTailRec(packed: List[List[T]], accList: List[(Int, T)]): List[(Int, T)] = {
    packed match {
      case Nil => accList
      case head :: tail => encodeTailRec(tail, (head.length, head.head) :: accList)
    }
  }

  encodeTailRec(packedList, List.empty).reverse
}

encode(List('a','b','c','d','e'))
encode(List(1,1,1,2,2,2,3,3,4,4,5,5,6,7,8,8,8,8,8,9))
encode(List.empty)
encode(List(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,3,3,3,3,1,1,3))



def encodeModified[T](theList: List[T]) : List[Any] = {

  val packedList : List[List[T]] = pack(theList)

  @tailrec
  def encodeModifiedTailRec(remainingList : List[List[T]], accList: List[Any]) : List[Any] = {
    remainingList match {
      case Nil => accList
      case head :: tail  if head.length == 1 => encodeModifiedTailRec(tail, head.head :: accList)
      case head :: tail => encodeModifiedTailRec(tail, (head.length, head.head) :: accList)
    }
  }

  encodeModifiedTailRec(packedList, List.empty).reverse
}

encodeModified(List('a','b','c','d','e'))
encodeModified(List(1,1,1,2,2,2,3,3,4,4,5,5,6,7,8,8,8,8,8,9))
encodeModified(List.empty)
encodeModified(List(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,3,3,3,3,1,1,3))

def decode[T](theList : List[(Int, T)]) : List[T] = {

  @tailrec
  def decodeTailRec(remainingList : List[(Int, T)], accList: List[T]) : List[T] = {
    remainingList match {
      case Nil => accList
      case (1, element) :: tail => decodeTailRec(tail, element :: accList)
      case (num, element) :: tail => decodeTailRec((num - 1, element) :: tail, element :: accList)
    }
  }

  decodeTailRec(theList, List.empty).reverse
}

decode(encode(List('a','b','c','d','e')))
decode(encode(List(1,1,1,2,2,2,3,3,4,4,5,5,6,7,8,8,8,8,8,9)))
decode(encode(List.empty))
decode(encode(List(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,3,3,3,3,1,1,3)))

def encodeDirect[T](theList: List[T]) : List[(Int, T)] = {

  if(theList.isEmpty)
    return List.empty

  @tailrec
  def encodeDirectTailRec(remainingList: List[T], accList: List[(Int, T)]): List[(Int, T)] = {
    remainingList match {
      case Nil => accList
      case head :: tail if head == accList.head._2 => encodeDirectTailRec(tail, (accList.head._1 + 1, head) :: accList.tail)
      case head :: tail => encodeDirectTailRec(tail, (1, head) :: accList)
    }
  }

  encodeDirectTailRec(theList.tail, List((1, theList.head))).reverse
}

encodeDirect(List('a','b','c','d','e'))
encodeDirect(List(1,1,1,2,2,2,3,3,4,4,5,5,6,7,8,8,8,8,8,9))
encodeDirect(List.empty)
encodeDirect(List(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,3,3,3,3,1,1,3))

def duplicate[T](theList: List[T]) : List[T] = {

  @tailrec
  def duplicateTailRec(remainingList: List[T], accList: List[T]): List[T] = {
    remainingList match {
      case Nil => accList
      case head :: tail => duplicateTailRec(tail, head :: head :: accList)
    }
  }

  duplicateTailRec(theList, List.empty).reverse
}

duplicate(List("Silly", "Billy"))
duplicate(List(1,2,3,4,5))
duplicate(List.empty)
duplicate(List('a','b','a','a'))


def duplicateN[T](num: Int, theList: List[T]) : List[T] = {

  @tailrec
  def duplicateNTailRec(countDown: Int, remainingList: List[T], accList: List[T]) : List[T] = {
    remainingList match {
      case Nil => accList
      case head :: tail => countDown match {
        case 0 => duplicateNTailRec(num, tail, accList)
        case n => duplicateNTailRec(n-1, remainingList, head :: accList)
      }
    }
  }

  duplicateNTailRec(num, theList, List.empty).reverse
}

duplicateN(3, List('a','b','c','d'))
duplicateN(0, List(1,2,3,4,5))
duplicateN(5, List.empty)

def drop[T](num: Int, theList: List[T]): List[T] = {
  theList.foldLeft((List.empty[T], 1))((acc, element) => {
    if(acc._2 == num) {
      (acc._1, 1)
    }
    else {
      (element :: acc._1, acc._2 + 1)
    }
  })._1.reverse
}

drop(3, List(1,2,3,4,5,6))
drop(5, List.empty[Char])

def split[T](num: Int, theList: List[T]) : (List[T], List[T]) = {
  val tripple = theList.foldLeft((List.empty[T], List.empty[T], 0))((acc, element) => {
    if(acc._3 < num) {
      (element :: acc._1, acc._2, acc._3 + 1)
    }
    else
      (acc._1, element :: acc._2, acc._3)
  })

  (tripple._1.reverse, tripple._2.reverse)
}

split(3, List(1,2,3,4,5,6,7,8))


