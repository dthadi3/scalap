package scalap.tests

import scalap.tree.KDTree
import scalap.tree.datatypes.KDNode

class KDTreeTest extends BaseTest
{
    var items = List(
        (List(1,2).toArray, 1),
        (List(3,4).toArray, 1),
        (List(5,6).toArray, 1),
        (List(4,7).toArray, 1),
        (List(7,5).toArray, 1),
        (List(6,4).toArray, 1),
        (List(1,3).toArray, 1),
        (List(7,8).toArray, 1),
        (List(4,2).toArray, 1),
        (List(0,0).toArray, 1),
        (List(6,6).toArray, 1),
        (List(7,0).toArray, 1),
        (List(7,1).toArray, 1),
        (List(7,9).toArray, 1),
        (List(1,7).toArray, 1)
    )
    val kd = new KDTree[Int, Int](items)

    def searchTest(): Unit = {
        items.foreach(item => assertTrue(kd.search(item._1).isInstanceOf[KDNode[Int, Int]]))
        assert(kd.search(List(-1,-9).toArray), null)
        assert(kd.search(List(-7,9).toArray), null)
        assert(kd.search(List(7,-9).toArray), null)
        assert(kd.search(List(-7,-9).toArray), null)
        println("[+] KDTree search OK!")
    }

    def buildTest(): Unit = {
        var nodes = kd.getNodes()
        assertTrue(nodes.head.point.sameElements(List(5,6)))
        assertTrue(nodes(1).point.sameElements(List(1,3)))
        assertTrue(nodes(2).point.sameElements(List(1,2)))
        assertTrue(nodes(3).point.sameElements(List(0,0)))
        assertTrue(nodes(4).point.sameElements(List(4,2)))
        assertTrue(nodes(5).point.sameElements(List(3,4)))
        assertTrue(nodes(6).point.sameElements(List(1,7)))
        assertTrue(nodes(7).point.sameElements(List(4,7)))
        assertTrue(nodes(8).point.sameElements(List(7,5)))
        assertTrue(nodes(9).point.sameElements(List(7,0)))
        assertTrue(nodes(10).point.sameElements(List(6,4)))
        assertTrue(nodes(11).point.sameElements(List(7,1)))
        assertTrue(nodes(12).point.sameElements(List(7,8)))
        assertTrue(nodes(13).point.sameElements(List(6,6)))
        assertTrue(nodes(14).point.sameElements(List(7,9)))
        println("[+] KDTree build OK!")
    }

    def knnTest(): Unit = {
        val nn = kd.nearest(List(5,6).toArray)
        assertTrue(List(5,6).toArray sameElements  nn._1.point)
        assert(0, nn._2)

        var knn = kd.kNN(List(6,5).toArray, 4)
        var res = knn.map{node => node._1.point.toList}
        assert(4, knn.size)
        assertTrue(res contains List(6,6))
        assertTrue(res contains List(6,4))
        assertTrue(res contains List(7,5))
        assertTrue(res contains List(5,6))

        knn = kd.kNN(List(2,2).toArray, 3)
        res = knn.map{node => node._1.point.toList}
        assert(3, knn.size)
        assertTrue(res contains List(1,2))
        assertTrue(res contains List(1,3))
        assertTrue(res contains List(4,2))

        knn = kd.kNN(List(3,3).toArray, 2)
        res = knn.map{node => node._1.point.toList}
        assert(2, knn.size)
        assertTrue(res contains List(3,4))
        assertTrue(res contains List(4,2))

        knn = kd.kNN(List(6,8).toArray, 2)
        res = knn.map{node => node._1.point.toList}
        assert(2, knn.size)
        assertTrue(res contains List(7,8))
        assertTrue(res contains List(7,9))

        knn = kd.kNN(List(8,7).toArray, 4)
        res = knn.map{node => node._1.point.toList}
        assert(4, knn.size)
        assertTrue(res contains List(7,8))
        assertTrue(res contains List(7,9))
        assertTrue(res contains List(7,5))
        assertTrue(res contains List(6,6))

        knn = kd.kNN(List(5,5).toArray, 4)
        res = knn.map{node => node._1.point.toList}
        assert(4, knn.size)
        assertTrue(res contains List(5,6))
        assertTrue(res contains List(6,6))
        assertTrue(res contains List(6,4))
        assertTrue(res contains List(7,5))

        knn = kd.kNN(List(4,5).toArray, 4)
        res = knn.map{node => node._1.point.toList}
        assert(4, knn.size)
        assertTrue(res contains List(3,4))
        assertTrue(res contains List(5,6))
        assertTrue(res contains List(4,7))
        assertTrue(res contains List(6,6))

        knn = kd.kNN(List(5,6).toArray, 5)
        res = knn.map{node => node._1.point.toList}
        assert(5, knn.size)
        assertTrue(res contains List(5,6))
        assertTrue(res contains List(6,6))
        assertTrue(res contains List(4,7))
        assertTrue(res contains List(7,5))
        assertTrue(res contains List(6,4))

        knn = kd.kNN(List(2,9).toArray, 2)
        res = knn.map{node => node._1.point.toList}
        assert(2, knn.size)
        assertTrue(res contains List(1,7))
        assertTrue(res contains List(4,7))

        knn = kd.kNN(List(100,100).toArray, 3)
        res = knn.map{node => node._1.point.toList}
        assert(3, knn.size)
        assertTrue(res contains List(7,8))
        assertTrue(res contains List(7,9))
        assertTrue(res contains List(6,6))

        println("[+] KDTree knn OK!")
    }

    def insertTest(): Unit = {
        assertTrue(kd.search(List(6,9).toArray) == null)
        kd.insert((List(6,9).toArray, 1))
        assertTrue(kd.search(List(6,9).toArray).isInstanceOf[KDNode[Int, Int]])
        println("[+] KDTree insert OK!")
    }

    /**
      * Delete test, always run after insertTest
      */
    def deleteTest(): Unit = {
        assertTrue(kd.search(List(6,9).toArray).isInstanceOf[KDNode[Int, Int]])
        kd.delete(List(6,9).toArray)
        assertTrue(kd.search(List(6,9).toArray) == null)

        var nodes = kd.getNodes()
        kd.delete(List(5,6).toArray)
        assertTrue(nodes.head.point.sameElements(List(6,4)))
        assertTrue(nodes(1).point.sameElements(List(1,3)))
        assertTrue(nodes(2).point.sameElements(List(1,2)))
        assertTrue(nodes(3).point.sameElements(List(0,0)))
        assertTrue(nodes(4).point.sameElements(List(4,2)))
        assertTrue(nodes(5).point.sameElements(List(3,4)))
        assertTrue(nodes(6).point.sameElements(List(1,7)))
        assertTrue(nodes(7).point.sameElements(List(4,7)))
        assertTrue(nodes(8).point.sameElements(List(7,5)))
        assertTrue(nodes(9).point.sameElements(List(7,0)))
        assertTrue(nodes(11).point.sameElements(List(7,1)))
        assertTrue(nodes(12).point.sameElements(List(7,8)))
        assertTrue(nodes(13).point.sameElements(List(6,6)))
        assertTrue(nodes(14).point.sameElements(List(7,9)))

        println("[+] KDTree delete OK!")
    }

    def updateTest(): Unit = {
        kd.update(List(7, 9).toArray, 10)
        val node = kd.search(List(7, 9).toArray)
        assertTrue(node.data == 10)
        println("[+] KDTree update OK!")
    }

    def rangeSearchTest(): Unit = {
        val nodes = kd.rangeSearch(List(4,4).toArray, List(8,8).toArray)
        val res = nodes.map{node => node.point.toList}
        assertTrue(res contains List(4,7))
        assertTrue(res contains List(6,4))
        assertTrue(res contains List(6,6))
        assertTrue(res contains List(7,8))
        assertTrue(res contains List(7,5))
        println("[+] KDTree range search OK!")
    }

    override def test(): Unit = {
        buildTest()
        searchTest()
        knnTest()
        insertTest()
        deleteTest()
        updateTest()
        rangeSearchTest()
    }
}
