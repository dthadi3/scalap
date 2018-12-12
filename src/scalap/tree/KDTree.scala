package scalap.tree
import scala.collection.mutable
import datatypes.KDNode

import Ordering.Implicits._
import Numeric.Implicits._

/**
  * KD-Tree implementation
  * Info: https://en.wikipedia.org/wiki/K-d_tree
  *
  * @param items (array point, data)
  * @tparam T Numeric, point datatype
  * @tparam D data datatype
  */
class KDTree[T: Numeric, D](var items: List[(Array[T], D)])
{
    private def dimensions: Int = items.head._1.length
    var points: List[Array[T]] = items.map(n => n._1)
    this.checkDimentions(points)
    var root: KDNode[T, D] = build(items, 0)

    /**
      * Builds KD-Tree recursively.
      *
      * var int axis := depth mod k;
      * select median by axis from pointList;
      * node.location := median;
      * node.leftChild := kdtree(points in pointList before median, depth+1);
      * node.rightChild := kdtree(points in pointList after median, depth+1);
      * return node;
      *
      * @param items (array point, data)
      * @param axis start axis
      * @param parent parent node
      * @return KD-Node root of subtree
      */
    def build(items:  List[(Array[T], D)], axis: Int, parent: KDNode[T, D] = null): KDNode[T, D] = {
        if (items.isEmpty) {
            return null
        }

        val sorted_points = items.sortWith((x, y) => x._1(axis) < y._1(axis))
        val median = sorted_points.length / 2

        val point = sorted_points(median)._1
        val data = sorted_points(median)._2
        val node = new KDNode[T, D](point, data, axis, parent)
        node.left  = build(sorted_points.slice(0,median), nextAxis(axis), node)
        node.right = build(sorted_points.slice(median+1, sorted_points.length), nextAxis(axis), node)

        node
    }

    /**
      * Adds an item to KDTree.
      *
      * @param item (array point, data)
      * @return
      */
    def insert(item: (Array[T], D)): KDNode[T, D] = {
        var point = item._1
        var data = item._2

        def insertTo(node: KDNode[T, D], parent: KDNode[T, D] = null): KDNode[T, D] = {
            node.point = point
            node.data = data
            node.axis = node.axis
            node.parent = parent
            node
        }

        def insertToLeft(node: KDNode[T, D]): KDNode[T, D] = {
            node.left = new KDNode(point, data,  nextAxis(node.axis), node)
            node.left
        }

        def insertToRight(node: KDNode[T, D]): KDNode[T, D] = {
            node.right = new KDNode(point, data,  nextAxis(node.axis), node)
            node.right
        }

        def insertFrom(node: KDNode[T, D], parent: KDNode[T, D] = null): KDNode[T, D] = {
            if (node.point == null) return insertTo(node)
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

    /**
      * Searches for a point.
      *
      * @param point array
      * @return node or null (if not exist)
      */
    def search(point: Array[T]): KDNode[T, D] = {
        def searchFrom(node: KDNode[T, D]): KDNode[T, D] = {
            if (node.point.sameElements(point)) return node
            if (point(node.axis) < node.point(node.axis)) {
                if (!node.hasLeftChild()) null
                else searchFrom(node.left)
            }
            else {
                if (!node.hasRightChild()) null
                else searchFrom(node.right)
            }
        }
        checkDimentions(point)
        searchFrom(root)
    }

    /**
      * Deletes an element that matches the given point and then
      * finds a replacement, replaces point with replacement and
      * recursively deletes replacement
      *
      * @param point array
      */
    def delete(point: Array[T]): Unit = {
        def removeNode(node: KDNode[T, D]): Unit = {
            if (node.isLeaf()) node.die()
            else replace(node)
        }

        def replace(node: KDNode[T, D]): Unit = {
            val replacement = findReplacementNode(node)
            node.point = replacement.point
            removeNode(replacement)
        }

        def findReplacementNode(node: KDNode[T, D]): KDNode[T, D] = {
            if (node.hasRightChild()) getNodes(node.right).min(Ordering.by((n: KDNode[T, D]) => n.point(node.axis)))
            else getNodes(node.left).max(Ordering.by((n: KDNode[T, D]) => n.point(node.axis)))
        }

        val node: KDNode[T, D] = search(point)
        if (node == null) throw new Exception("No Such Point!")
        removeNode(node)
    }

    /**
      * Searches for all points that min < point < max.
      *
      * @param min array minimum point
      * @param max array maximum point
      * @return
      */
    def rangeSearch(min: Array[T], max: Array[T]): List[KDNode[T, D]] = {
        checkDimentions(min)
        checkDimentions(max)
        var nodes = List[KDNode[T, D]]()
        var counter = 0

        def nodeInRange(node: KDNode[T, D]): Boolean = {
            for (i <- node.point.indices) {
                if (!(min(i) <= node.point(i) && node.point(i) <= max(i))) return false
            }
            true
        }

        def searchFrom(node: KDNode[T, D]): Unit = {
            counter = counter + 1
            if (nodeInRange(node)) nodes = node :: nodes
            if (min(node.axis) < node.point(node.axis) && node.hasLeftChild()) searchFrom(node.left)
            if (max(node.axis) > node.point(node.axis) && node.hasRightChild()) searchFrom(node.right)
        }

        searchFrom(root)
        println("Run times: " + counter)
        nodes
    }

    /**
      * Searches for nearest neighbor
      *
      * @param point array point
      * @return (node, distance)
      */
    def nearest(point: Array[T]): (KDNode[T, D], Double) = {
        kNN(point, 1).head
    }

    /**
      * Searches for k nearest neighbors.
      *
      * @param point array point
      * @param k how many results
      * @return list of (node, distance)
      */
    def kNN(point: Array[T], k: Int): List[(KDNode[T, D], Double)] = {
        checkDimentions(point)
        val proned = mutable.Set[KDNode[T, D]]()
        var candidates = mutable.Set[(KDNode[T, D], Double)]()
        var searchCounter = 0
        var backtrackCounter = 0

        def euclideanDistance(p: Array[T]): Double =
            Math.sqrt(p.zip(point).map { case (x,y) => Math.pow((y - x).toDouble, 2) }.sum)

        def maxDistance: Double = candidates.max(Ordering.by((pair: (KDNode[T, D], Double)) => pair._2))._2

        def proneCandidates(): Unit = {
            // :_* forces factory method to see the list as a list of arguments
            candidates = scala.collection.mutable.Set(candidates.toList.sortWith(_._2 < _._2).take(k) :_*)
        }

        def searchFrom(node: KDNode[T, D]): KDNode[T, D] = {
            if (point(node.axis) < node.point(node.axis)) {
                if (!node.hasLeftChild()) node
                else searchFrom(node.left)
            }
            else {
                if (!node.hasRightChild()) node
                else searchFrom(node.right)
            }
        }

        def backtrack(node: KDNode[T, D]): Unit = {
            backtrackCounter = backtrackCounter + 1
            var dist = euclideanDistance(node.point)
            if (candidates.size < k || dist < maxDistance) {
                candidates.add(node, dist)
                proned.add(node)
            }
            if (node.hasParent()) {
                searchNN(node.parent)
                backtrack(node.parent)
            }
            else {
                searchNN(node)
            }
        }

        def planeDistance(node: KDNode[T,D]): Double =
            Math.sqrt(Math.pow(node.point(node.axis).toDouble() - point(node.axis).toDouble() ,2))

        def searchNN(node: KDNode[T, D]): Unit = {
            searchCounter = searchCounter + 1
            val dist = euclideanDistance(node.point)

            if(!proned.contains(node)) {
                if (candidates.size < k || dist < maxDistance) candidates.add(node, dist)
                if (node.hasRightChild()) {
                    if (planeDistance(node.right) <= maxDistance || dist < maxDistance)
                    searchNN(node.right)
                }
                if (node.hasLeftChild()) {
                    if (planeDistance(node.left) <= maxDistance || dist < maxDistance)
                        searchNN(node.left)
                }
            }
            proned.add(node)
            if (candidates.size > k) proneCandidates()
        }

        if (k < 1) throw new Exception("k less than 1!")
        var node = searchFrom(root)
        candidates.add((node, euclideanDistance(node.point)))
        backtrack(node)

//        println("Candidates: " + candidates.size)
//        println("search: " + searchCounter)
//        println("backtrack: " + backtrackCounter)
//        println("proned: " + proned.size)

        candidates.toList.sortWith(_._2 < _._2).take(k)
    }

    def update(point: Array[T], value: D): Unit = {
        checkDimentions(point)
        val node = search(point)
        node.data = value
    }

    /**
      * Get all nodes starting from node
      *
      * @param node a node (default is tree root)
      * @return
      */
    def getNodes(node: KDNode[T, D] = root): List[KDNode[T, D]] = {
        if (node.hasLeftChild() && node.hasRightChild()) node :: getNodes(node.left) ::: getNodes(node.right)
        else if (node.hasLeftChild()) node :: getNodes(node.left)
        else if (node.hasRightChild()) node :: getNodes(node.right)
        else List(node)
    }

    /**
      * Calculates the next axis
      *
      * @param axis current axis
      * @return
      */
    private def nextAxis(axis: Int): Int = (axis+1) % dimensions

    /**
      * Check dimensionality of a points list
      *
      * @param points list of points
      */
    private def checkDimentions(points:  List[Array[T]]): Unit = for (point <- points) this.checkDimentions(point)

    /**
      * Chechs dimensionality of a point
      *
      * @param point a point
      */
    private def checkDimentions(point: Array[T]): Unit =
        if (point.length != this.dimensions) throw new Exception("All Points must have the same dimensionality!")
}