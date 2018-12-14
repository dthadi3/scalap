package scalap.tests

final case class TestFailedException(private val message: String = "", private val cause: Throwable = None.orNull)
    extends Exception(message, cause)