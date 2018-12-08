import scalap.hashing.{BloomFilter, LSH, MinHash}
import scalap.tree.KDTree

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


object Main extends App {
    def testKDTree(): Unit = {
        val kd = new KDTree(
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

    def testMinHash(): Unit = {
        val documents = Map(
            (0, "lorem ipsum dolor sit amet"),
            (1, "excepteur sint occaecat cupidatat non proident"),
            (2, "irure dolor in reprehenderit in voluptate velit esse"),
            (3, "to do or not to do"),
            (4, "to do or not to do"),
            (5, "to be or not to be"))

        val minHash = new MinHash(documents, 5)
        val minHashes = minHash.getMinHashes()
        for (minHash <- minHashes) {
            println("Document: " + minHash._1)
            minHash._2.foreach(println)
        }
    }

    def testLSH():Unit = {

    }
//    testKDTree()
//    testBloomFilters()
    testMinHash()
//    testLSH()

}