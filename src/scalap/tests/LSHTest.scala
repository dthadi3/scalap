package scalap.tests

import scalap.hashing.LSH

class LSHTest extends BaseTest
{
    val documents = Map(
        (0, "lorem ipsum dolor sit amet"),
        (1, "excepteur sint occaecat cupidatat non proident"),
        (2, "irure dolor in reprehenderit in voluptate velit esse"),
        (3, "to do or not to do"),
        (4, "to do or not to do"),
        (5, "to be or not to be"))

    override def test(): Unit = {
        val lsh = new LSH(4, 2, documents, 0.2)
        assertTrue(lsh.findSimilar("mmm to be or not to be").contains(5))
        println("[+] LSH OK!")
    }
}
