package gnieh.coroutines.test

import scala.util.continuations._
import gnieh._

object Test2 {

  case class Node(key: Int, left: Node, right: Node)

  /*
    original code
    def preorder(node: Node): Int = {
      if (node != null) {
        preorder(node.left)
        coroutines.yld(node.key)
        preorder(node.right)
      }
    }
    def preorder_iterator(tree: Node) = {
      val co = new wrap {
        (_: Unit) => {
          preorder(tree)
          -1
        }
      }
    }
   */
  def preorder(node: Node)(implicit co: coroutines.Coroutine[Unit, Int]): Unit @cpsParam[Int, Int] = {
    if (node != null) {
      preorder(node.left)(co)
      co.yld(node.key)
      preorder(node.right)(co)
    }
  }

  def preorder_iterator(tree: Node) = {
    val co = new coroutines.Coroutine[Unit, Int] {
      fun = (_: Unit) => {
        reset[Unit, Int] {
          preorder(tree)
          this.ret(-1)
        }
      }
    }
    () => co.resume()
  }

  def merge(t1: Node, t2: Node) {
    val it1 = preorder_iterator(t1)
    val it2 = preorder_iterator(t2)
    var v1 = it1()
    var v2 = it2()

    while (v1 != -1 || v2 != -1) {
      if (v1 != -1 && (v2 == -1 || v1 < v2)) {
        println(v1)
        v1 = it1()
      } else {
        println(v2)
        v2 = it2()
      }

    }
  }

  def main(args: Array[String]) {
    val t1 = Node(4, Node(2, Node(1, null, null), null), Node(7, null, null))
    val t2 = Node(5, Node(0, null, Node(3, null, null)), null)
    merge(t1, t2)
  }

}