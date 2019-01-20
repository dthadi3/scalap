package scalap.examples

import scalap.benchmark.Benchmark
import scalap.tree.KDTree

import scala.collection.mutable.ListBuffer
import scala.util.Random

object KDTreeExample extends App
{
    def run(nNodes: Int = 100000,
            nKNN: Int = 10000,
            nSearches: Int = 10000,
            nInsertions: Int = 10000): Unit = {

        println("[*] KDTree Example")

        val random = new Random()

        var items = ListBuffer[(Array[Int], Int)]()
        for (i <- 0 until nNodes) {
            items += ((List(random.nextInt(), random.nextInt()).toArray, 1))
        }

        println("[i] Building KDTree with " + nNodes + " nodes")
        var kdTree = Benchmark.time {
            new KDTree(items.toList)
        }
        //kdTree.getNodes().foreach(println)

        println("[i] Performing " + nKNN + " knn searches")
        Benchmark.time{
            for(i <- 0 until nKNN)
                kdTree.kNN(List(random.nextInt(), random.nextInt()).toArray, random.nextInt(10) + 1)
        }

        println("[i] Performing " + nSearches + " searches")
        Benchmark.time{
            for(i <- 0 until nSearches)
                kdTree.search(List(random.nextInt(), random.nextInt()).toArray)
        }

        println("[i] Performing " + nInsertions + " insertions")
        Benchmark.time{
            for(i <- 0 until nInsertions)
                kdTree.insert((List(random.nextInt(), random.nextInt()).toArray, 1))
        }
        println()
    }
}
