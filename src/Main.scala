import scalap.hashing.{BloomFilter, LSH, MinHash}
import scalap.tree.KDTree

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


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

    def testMinHash(): Unit = {
        val documents = List(
            "lorem ipsum dolor sit amet", // document 0
            "excepteur sint occaecat cupidatat non proident", // document 1
            "irure dolor in reprehenderit in voluptate velit esse", // document 2
            "to do or not to do", // document 3
            "to do or not to do", // document 4
            "to be or not to be") // document 5
            .toIndexedSeq.zipWithIndex

        val minHash = new MinHash(documents, 5)
        val minHashes = minHash.documentWords.mapValues(wordSet => minHash.getMinHash(wordSet))
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