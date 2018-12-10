package scalap.tree
import Ordering.Implicits._
import Numeric.Implicits._
import java.util

import scala.collection.mutable
import scala.collection.mutable.Queue
import scala.collection.LinearSeq


class KDTree[T: Numeric](var points: Array[Array[T]])
{
    private def dimensions: Int = points(0).length
    this.checkDimentions(points)
    var root: Node = build(points, 0)

    class Node( var axis: Int = 0,
                var point: Array[T] = null,
                var left: Node = null,
                var right: Node = null,
                var parent: Node = null
              ) {
        override def toString(): String = {
            if (point != null) "Node((" + point.mkString(", ") + ") , axis:" + axis + ", parent:" + parent + ")"
            else "EMPTY-NODE"
        }

        def die(): Unit = {
            if (this.parent.left == this) this.parent.left = null
            else if (this.parent.right == this) this.parent.right = null
            this.point = null
        }

        def isLeaf(): Boolean = {
            this.right == null && this.left == null
        }

        def hasRightChild(): Boolean = {
            this.right != null
        }

        def hasLeftChild(): Boolean = {
            this.left != null
        }
    }

    def build(points:  Array[Array[T]], axis: Int, parent: Node = null): Node = {
        if (points.isEmpty) {
            return null
        }

        val node = new Node(axis)

        val sorted_points = points.sortWith(_(axis) < _(axis))
        val median = sorted_points.length / 2

        node.point = sorted_points(median)
        node.parent = parent
        node.left  = build(sorted_points.slice(0,median), nextAxis(axis), node)
        node.right = build(sorted_points.slice(median+1, sorted_points.length), nextAxis(axis), node)

        return node
    }

    def insert(point: Array[T]): Node = {
        def insertTo(node: Node, parent: Node = null): Node = {
            node.point = point
            node.parent = parent
            return node
        }

        def insertToLeft(node: Node): Node = {
            node.left = new Node(nextAxis(node.axis), point, null, null, node)
            return node.left
        }

        def insertToRight(node: Node): Node = {
            node.right = new Node(nextAxis(node.axis), point, null, null, node)
            return node.right
        }

        def insertFrom(node: Node, parent: Node = null): Node = {
                if (node.point==null) return insertTo(node)
                if (point(node.axis) < node.point(node.axis)) {
                    if (node.left == null) insertToLeft(node)
                    else insertFrom(node.left)
                }
                else {
                    if (node.right == null) insertToRight(node)
                    else insertFrom(node.right)
                }
        }

        checkDimentions(point)
        insertFrom(root)
    }

    def search(point: Array[T]): Node = {
        def searchFrom(node: Node): Node = {
            if (node.point sameElements point) return node
            if (point(node.axis) < node.point(node.axis)) {
                if (node.left == null) null
                else searchFrom(node.left)
            }
            else {
                if (node.right == null) null
                else searchFrom(node.right)
            }
        }

        searchFrom(root)
    }

    def delete(point: Array[T]): Unit = {
        def removeNode(node: Node): Unit = {
            if (node.isLeaf()) node.die()
            else replace(node)
        }

        def replace(node: Node): Unit = {
            val replacement = findReplacementNode(node)
            node.point = replacement.point
            removeNode(replacement)
        }

        def findReplacementNode(node: Node): Node = {
            if (node.hasRightChild()) {
                getNodes(node.right)
                    .min(Ordering.by((n: Node) => n.point(node.axis)))
            }
            else {
                getNodes(node.left)
                    .max(Ordering.by((n: Node) => n.point(node.axis)))
            }
        }

        val node: Node = search(point)

        if (node == null) throw new Exception("No Such Point!")
        removeNode(node)
    }

    def getNodes(node: Node = root): List[Node] = {
        if (node.left != null && node.right != null)
            node :: getNodes(node.left) ::: getNodes(node.right)
        else if (node.left != null) {
            node :: getNodes(node.left)
        }
        else if (node.right != null) {
            node :: getNodes(node.right)
        }
        else
            List(node)
    }



    private def nextAxis(axis: Int): Int = (axis+1) % dimensions

    private def checkDimentions(points:  Array[Array[T]]): Unit = {
        for (point <- points) {
            this.checkDimentions(point)
        }
    }

    private def checkDimentions(point: Array[T]): Unit = {
        if (point.length != this.dimensions) {
            throw new Exception("All Points must have the same dimensionality!")
        }
    }
}