package scalap.examples

import scalap.benchmark.Benchmark
import scalap.hashing.BloomFilter

import scala.collection.mutable.ListBuffer
import scala.util.Random

object BloomFilterExample extends App
{
    def randomString(length: Int) = {
        val r = new Random()
        val sb = new StringBuilder
        for (i <- 1 to length) {
            sb.append(r.nextPrintableChar)
        }
        sb.toString
    }

    def run(nElements: Int = 1000, nBits: Int = 10000, lookups: Int = 1000): Unit = {
        println("[*] BloomFilter Example")
        println("[i] Creating bloomfilter with " + nElements + " expected elements and " + nBits + "bits")
        var bloomFilter = new BloomFilter( nElements, nBits)

        var items = ListBuffer[String]()

        println("[i] Building Bloom Filter")
        println("[i] False positive probabilitity is " + bloomFilter.probabilityOfFalsePositives)
        println("[i] Performing " + nElements + " Insertions")
        Benchmark.time {
            bloomFilter.insert(randomString(10))
        }

        println("[i] Performing " + lookups + " lookups")
        Benchmark.time {
            bloomFilter.search(randomString(10))
        }

        bloomFilter.insert("savarakatranemia")
        println("[i] Word savarakatranemia exist in BloomFilter = " + bloomFilter.search("savarakatranemia"))
        println()
    }
}
