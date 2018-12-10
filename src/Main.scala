import scalap.hashing.{BloomFilter, LSH, MinHash}
import scalap.tree.KDTree

import scala.collection.LinearSeq
import scala.collection.mutable.ArrayBuffer
import scala.io.Source


object Main extends App {
    def testKDTree(): Unit = {
        val kd = new KDTree(Array(
            LinearSeq(1,2).toArray,
            LinearSeq(3,4).toArray,
            LinearSeq(5,6).toArray,
            LinearSeq(4,7).toArray,
            LinearSeq(7,5).toArray,
            LinearSeq(6,4).toArray,
            LinearSeq(1,3).toArray,
            LinearSeq(7,8).toArray,
            LinearSeq(1,7).toArray
        ))

        println("Nodes: ")
        kd.getNodes().foreach(println)
        println()

        println("Nodes after (6,9) insertion: ")
        kd.insert(List(6,9).toArray)
        kd.getNodes().foreach(println)
        println()

        println("Search: " + kd.search(List(1,7).toArray))
        println("Search: " + kd.search(List(3,4).toArray))
        println()

//        kd.remove(List(6,9))
//        println("Points after (6,9) removal:")
//        kd.getNodes().foreach(println)
//        println()
//
        kd.delete(List(4,7).toArray)
        println("Points after (4,7) removal:")
        kd.getNodes().foreach(println)
        println()
//

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

        val lsh = new LSH(2, 2, documents, 0.2)
        println()
        println("Similar to 'to be or not to be':")
        lsh.findSimilar("mmm to be or not to be").foreach(println)
    }
    testKDTree()
//    testBloomFilters()
//    testMinHash()
//    testLSH()

}