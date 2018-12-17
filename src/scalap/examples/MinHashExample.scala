package scalap.examples

import scalap.benchmark.Benchmark
import scalap.hashing.MinHash

import scala.io.Source
import scala.util.Random

object MinHashExample extends App
{
    def run(length: Int = 100, similarityChecks: Int = 1000): Unit = {

        val random = new Random()

        println("[*] MinHash Example")
        val lines = Source.fromFile("fixtures/random-text.txt")
            .getLines
            .toList
            .map(_.replaceAll("[^a-zA-Z ]", "") // leters only
                .toLowerCase)

        val documents = ((Stream from 0) zip lines).toMap

        println("[i] Creating MinHasher for " + documents.size + " size " + length )
        val minHash = new MinHash(documents, length)

        println("[i] Creating " + documents.size + " Minhashes")
        Benchmark.time({
            var minHashes = minHash.getMinHashes()
        })

        println("[i] Performing  " + similarityChecks + " similarity checks")
        Benchmark.time({
            for (i <- 0 until similarityChecks) {
                minHash.similarity(random.nextInt(7),random.nextInt(7))
            }
        })

        println("[i] Documents similarity 100 and 101 = " + minHash.similarity(100, 101))
        println("[i] Documents similarity 0 and 1 = " + minHash.similarity(0, 1))
        println()
    }
}
