import scalap.hashing.{BloomFilter, LSH, MinHash}
import scalap.tree.KDTree

import scala.collection.LinearSeq
import scala.collection.mutable.ArrayBuffer
import scala.io.Source


object Main extends App {
    def testKDTree(): Unit = {
        val kd = new KDTree[Int, Int](List(
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
        ))

//        println("Nodes: ")
//        kd.getNodes().foreach(println)
//        println()
//
//        println("Nodes after (6,9) insertion: ")
//        kd.insert(List(6,9).toArray)
//        kd.getNodes().foreach(println)
//        println()
//
//        println("Search: " + kd.search(List(1,7).toArray))
//        println("Search: " + kd.search(List(3,4).toArray))
//        println()
//
////
//        kd.delete(List(4,7).toArray)
//        println("Points after (4,7) removal:")
//        kd.getNodes().foreach(println)
//        println()

        kd.insert((List(6,9).toArray, 1))
        kd.getNodes().foreach(println)
        println()
//        println(kd.nearestNeighbor(List(6, 5).toArray))
        println()
//        kd.kNearestNeighbors(List(6,5).toArray, 4).foreach(println)
//        kd.kNearestNeighbors(List(2,2).toArray, 4).foreach(println)
//        kd.kNearestNeighbors(List(6,8).toArray, 4).foreach(println)
//        kd.kNearestNeighbors(List(-1,-1).toArray, 4).foreach(println)
        kd.insert((List(6,7).toArray, 1))
        kd.kNN(List(5,6).toArray, 2).foreach(println)
//        kd.rangeSearch(List(5,5).toArray, List(6,6).toArray).foreach(println)
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