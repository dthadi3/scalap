package scalap.tree.datatypes

class KDNode[T, D](var point: Array[T] = null,
                   var data: D = null,
                   var axis: Int = 0,
                   var parent: KDNode[T, D] = null,
                   var left: KDNode[T, D] = null,
                   var right: KDNode[T, D] = null
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

    def isRoot(): Boolean = {
        this.parent == null
    }

    def hasRightChild(): Boolean = {
        this.right != null && this.right.point != null
    }

    def hasLeftChild(): Boolean = {
        this.left != null && this.left.point != null
    }

    def hasParent(): Boolean = {
        this.parent != null
    }
}
