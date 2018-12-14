package scalap.tests

import scalap.hashing.MinHash

class MinHashTest extends BaseTest
{
    val documents = Map(
        (0, "lorem ipsum dolor sit amet"),
        (1, "excepteur sint occaecat cupidatat non proident"),
        (2, "irure dolor in reprehenderit in voluptate velit esse"),
        (3, "to do or not to do"),
        (4, "to do or not to do"),
        (5, "to be or not to be"),
        (6, "dolorem ipsum dolor sit amet"))

    val minHash = new MinHash(documents, 20)

    override def test(): Unit = {
        val minHashes = minHash.getMinHashes()
        assert(minHash.similarity(3,4), 1)
        assert(minHash.similarity(6,6), 1)

        println("[+] MinHash OK!")
    }
}
