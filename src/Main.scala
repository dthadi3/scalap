import scalap.examples.{BloomFilterExample, KDTreeExample, LSHExample, MinHashExample}
import scalap.tests.{BloomFilterTest, KDTreeTest, LSHTest, MinHashTest}


object Main extends App {
    def test(): Unit = {
        val tests = List(
            new KDTreeTest(),
            new MinHashTest(),
            new LSHTest(),
            new BloomFilterTest()
        )

        tests.foreach(test => test.run())
    }

    test()

    println()
    KDTreeExample.run()

    println()
    BloomFilterExample.run()

    println()
    MinHashExample.run()

    println()
    LSHExample.run()
}