package scalap.examples

import scalap.benchmark.Benchmark
import scalap.hashing.LSH

import scala.io.Source
import scala.util.Random

object LSHExample extends App
{
    def run(minHashLength: Int = 100, numberOfBands: Int = 50, threshold: Double = 0.5, nSimChecks: Int = 1000): Unit = {
        println("[*] LSH Example")
        val random = new Random()


        val lines = Source.fromFile("fixtures/random-text.txt")
            .getLines
            .toList
            .map(_.replaceAll("[^a-zA-Z ]", "") // leters only
                .toLowerCase)

        val documents = ((Stream from 0) zip lines).toMap

        println("[i] Preparing LSH")
        val lsh = Benchmark.time {
            new LSH(minHashLength, numberOfBands, documents, threshold)
        }

        println("[i] Performing " + nSimChecks + " similarity checks")
        Benchmark.time {
            for (i <- 0 until nSimChecks) {
                lsh.findSimilar(lines(random.nextInt(documents.size)))
            }
        }

        println("[i] Similar to  mmm to be or not to be: " + lsh.findSimilar("mmm to be or not to be"))
        println()
    }
}
