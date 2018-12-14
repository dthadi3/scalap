package scalap.tests

import scalap.hashing.BloomFilter

class BloomFilterTest extends BaseTest
{
    var bloomFilter = new BloomFilter( 100, 1000)

    bloomFilter.build(List("rg", "asdf"))


    override def test(): Unit = {
        assertTrue(bloomFilter.search("rg"))
        bloomFilter.insert("plapla")
        assertTrue(bloomFilter.search("plapla"))
        bloomFilter.insert("plaplapla")
        assertTrue(bloomFilter.search("plaplapla"))
        bloomFilter.insert("plaplaplapla")
        assertTrue(bloomFilter.search("plaplaplapla"))
        bloomFilter.insert("plaplaplaplapla")
        assertTrue(bloomFilter.search("plaplaplaplapla"))
        bloomFilter.insert("plaplaplaplaplapla")
        assertTrue(bloomFilter.search("plaplaplaplaplapla"))
        println("[+] BloomFilter OK!")
    }
}
