import scalap.hashing.{BloomFilter, LSH, MinHash}
import scalap.tree.KDTree

import scala.collection.LinearSeq
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scalap.tests.{BloomFilterTest, KDTreeTest, LSHTest, MinHashTest}


object Main extends App {
    val tests = List(
        new KDTreeTest(),
        new MinHashTest(),
        new LSHTest(),
        new BloomFilterTest()
    )

    tests.foreach(test => test.run())
}