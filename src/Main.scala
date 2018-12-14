import scalap.hashing.{BloomFilter, LSH, MinHash}
import scalap.tree.KDTree

import scala.collection.LinearSeq
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scalap.tests.KDTreeTest


object Main extends App {

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
            (5, "to be or not to be"),
            (6, "dolorem ipsum dolor sit amet"))

        val minHash = new MinHash(documents, 5)
        val minHashes = minHash.getMinHashes()
        for (minHash <- minHashes) {
            println("Document: " + minHash._1)
            minHash._2.foreach(println)
        }

        println()
        println(minHash.similarity(0,6))
    }

    def testLSH(): Unit = {
        val documents = Map(
            (0, "lorem ipsum dolor sit amet"),
            (1, "excepteur sint occaecat cupidatat non proident"),
            (2, "irure dolor in reprehenderit in voluptate velit esse"),
            (3, "to do or not to do"),
            (4, "to do or not to do"),
            (5, "to be or not to be"))

        val lsh = new LSH(4, 2, documents, 0.2)
        println()
        println("Similar to 'to be or not to be':")
        lsh.findSimilar("mmm to be or not to be").foreach(println)
    }

//    var kdTreeTest = new KDTreeTest()
//    kdTreeTest.run()
//    testBloomFilters()
//    testMinHash()
    testLSH()

}