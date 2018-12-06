package scalap.tree

class KDTree(var points: Array[List[Int]])
{
    private def dimensions: Int = points(0).length
    this.checkDimentions(points)
    var root: Node = build(points, 0)

    class Node(var point: List[Int],
               var left: Node,
               var right: Node,
               var axis: Int)

    def build(points:  Array[List[Int]], axis: Int): Node = {
        if (points.isEmpty) {
            return null
        }

        val sorted_points = points.sortWith(_(axis) < _(axis))
        val median = sorted_points.length / 2
        val left  = build(sorted_points.slice(0,median), nextAxis(axis))
        val data = sorted_points(median)
        val right = build(sorted_points.slice(median+1, sorted_points.length), nextAxis(axis))

        return new Node(data, left, right, axis)
    }

    def insert(point: List[Int]): Node = {
        checkDimentions(point)

        def insertTo(node: Node, point:List[Int]): Node = {
            node.point = point
            return node
        }

        def insertToLeft(node: Node, point:List[Int]): Node = {
            node.left = new Node(point, null, null, nextAxis(node.axis))
            return node.left
        }

        def insertToRight(node: Node, point:List[Int]): Node = {
            node.right = new Node(point, null, null, nextAxis(node.axis))
            return node.right
        }

        def insertFrom(node: Node, point:List[Int]): Node = {
            if (node.point==null) return insertTo(node, point)
            if (point(node.axis) < node.point(node.axis)) {
                if (node.left == null) insertToLeft(node, point)
                else insertFrom(node.left, point)
            }
            else {
                if (node.right == null) insertToRight(node, point)
                else insertFrom(node.right, point)
            }
        }

        return insertFrom(root, point)
    }

    private def nextAxis(axis: Int): Int = (axis+1) % dimensions

    private def checkDimentions(points:  Array[List[Int]]): Unit = {
        for (point <- points) {
            this.checkDimentions(point)
        }
    }

    private def checkDimentions(point: List[Int]): Unit = {
        if (point.length != this.dimensions) {
            throw new Exception("All Points must have the same dimensionality!")
        }
    }
}