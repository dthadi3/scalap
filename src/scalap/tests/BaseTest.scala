package scalap.tests

abstract class BaseTest
{
    def test(): Unit

    def run(): Unit = {
        try {
            test()
        }
        catch {
            case e: TestFailedException =>
                println("[-] " + this.getClass.getName + ": "+ e.getMessage)
                e.getStackTrace
                    .filter(_.toString.contains(this.getClass.getName))
                    .foreach(trace =>
                        println("    " + trace))

        }
    }

    def assert[T](expected: T, actual: T): Unit = {
        if (expected != actual) {
            throw TestFailedException("Assertion Failed, expected " + expected + " actual " + actual)
        }
    }

    def assertTrue(condition: Boolean): Unit = {
        if (!condition) {
            throw TestFailedException("Assertion Failed...")
        }
    }
}
