package scalap.tree.datatypes

/**
  * KD tree Node.
  * @param point vector
  * @param data data of the vector
  * @param axis comparison axis
  * @param parent parent KDNode
  * @param left left child
  * @param right right child
  * @tparam T Type of point values
  * @tparam D Type of data
  */
class KDNode[T, D](var point: Array[T] = null,
                   var data: D = null,
                   var axis: Int = 0,
                   var parent: KDNode[T, D] = null,
                   var left: KDNode[T, D] = null,
                   var right: KDNode[T, D] = null)
{
    /**
      * String representation of a node
      * @return Node representation in string format
      */
    override def toString(): String = {
        if (point != null) "Node((" + point.mkString(", ") + ") , axis:" + axis + ", parent:" + parent + ")"
        else "EMPTY-NODE"
    }

    /**
      * Kills KDNode
      */
    def die(): Unit = {
        if (this.parent.left == this) this.parent.left = null
        else if (this.parent.right == this) this.parent.right = null
        this.point = null
    }

    /**
      * Checks if KDNode is a leaf node
      * @return boolean
      */
    def isLeaf(): Boolean = {
        this.right == null && this.left == null
    }

    /**
      * Checks if KDNode is the root of KDTree
      * @return boolean
      */
    def isRoot(): Boolean = {
        this.parent == null
    }

    /**
      * Checks if KDNode has right child
      * @return boolean
      */
    def hasRightChild(): Boolean = {
        this.right != null && this.right.point != null
    }

    /**
      * Checks if KDNode has left child
      * @return boolean
      */
    def hasLeftChild(): Boolean = {
        this.left != null && this.left.point != null
    }

    /**
      * Checks if KDNode has parent
      * @return boolean
      */
    def hasParent(): Boolean = {
        this.parent != null
    }
}
