import scalap.examples.{BloomFilterExample, KDTreeExample, LSHExample, MinHashExample}
import scalap.tests.{BloomFilterTest, KDTreeTest, LSHTest, MinHashTest}


object Main extends App {
    def test(): Unit = {
        println("[*] Running all tests")
        val tests = List(
            new KDTreeTest(),
            new MinHashTest(),
            new LSHTest(),
            new BloomFilterTest()
        )

        tests.foreach(test => test.run())
        println()
    }

    override def main(args: Array[String]) = {
        if(args.length == 0) {
            println("Usage: ./exe <option>")
            println("Options:")
            println("   --test         run all tests")
            println("   --examples     run all examples")
            println("   --kdtree       run KDTree example")
            println("   --bloomfilter  run Bloom Filter example")
            println("   --minhash      run MinHash example")
            println("   --lsh          run LSH example")
            System.exit(1)
        }

        if(args.contains("--test"))
            test()
        if(args.contains("--kdtree") || args.contains("--examples"))
            KDTreeExample.run()
        if(args.contains("--bloomfilter") || args.contains("--examples"))
            BloomFilterExample.run()
        if(args.contains("--minhash") || args.contains("--examples"))
            MinHashExample.run()
        if(args.contains("--lsh") || args.contains("--examples"))
            LSHExample.run()
    }
}