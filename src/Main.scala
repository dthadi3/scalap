import scalap.hashing.BloomFilter
import scalap.tree.KDTree

object Main extends App {
    def testKDTree(): Unit = {
        var kd = new KDTree(
            Array(
                List(1,2),
                List(3,4),
                List(5,6),
                List(7,8),
                List(1,7)
            )
        )

        var node = kd.insert(List(6,9))
        println(node.point)
        println(node.axis)
        println(node.left)
        println(node.right)
    }


    def testBloomFilters(): Unit = {
        var bloomFilter = new BloomFilter( 100, 1000)

        bloomFilter.build(List("rafael", "asdf"))
        bloomFilter.insert("plapla")
        bloomFilter.insert("plaplapla")
        bloomFilter.insert("plaplaplapla")
        bloomFilter.insert("plaplaplaplapla")
        bloomFilter.insert("plaplaplaplaplapla")
        println(bloomFilter.search("plaplaplaplapla"))
    }

//    testKDTree()
    testBloomFilters()

}