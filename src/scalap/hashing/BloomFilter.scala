package scalap.hashing
import scala.collection.mutable
import scala.util.hashing.MurmurHash3
import scala.math.pow
import scala.math.log
import scala.math.round

/**
  * Creates a bloom filter
  * @param elementsCount expected number of elements
  * @param bitsCount length of bit array
  * @param hashesCount number of hash Functions
  */
class BloomFilter(var elementsCount: Int, var bitsCount: Int, var hashesCount: Int)
{
    /**
      * Creates a bloom filter with the optimal number of hashFunctions
      * @param elementsCount expected number of elements
      * @param bitsCount length of bit array
      */
    def this(elementsCount:Int, bitsCount: Int) {
        this(elementsCount,
            bitsCount,
            // This is the optimal value of hash functions
            round((bitsCount/elementsCount)*log(2)).toInt)
    }

    private val bitSet = new mutable.BitSet(bitsCount)
    checkInputs()
    println("Number of HashFunctions: " + hashesCount)
    println("False Positive Probability: " + porbabilityOfFalsePositives)

    /**
      * Builds a bloomfilter by inserting a list of strings one by one
      * @param strings list of strings
      */
    def build(strings : List[String]): Unit = {
        strings.foreach(str => insert(str))
    }

    /**
      * Adds one string to the bloom filter
      * @param str a string
      */
    def insert(str: String): Unit = {
        calculateHashes(str)
            .foreach(i => bitSet.update(i, included = true))
    }

    /**
      * Search if this string occur in the bloom filter.
      * It's possible to have false positives but not false negatives!
      * @param str a string
      * @return true if exist
      */
    def search(str: String): Boolean = {
        calculateHashes(str)
            .map(hash => bitSet(hash))
            .reduce((a,b) => a & b)
    }

    /**
      * Calculates the probability of a false negative to occur in search.
      * @return probability
      */
    def porbabilityOfFalsePositives: Double = {
        // Aproximation: Math.pow(1f - Math.exp((-hashesCount*this.elementsCount)/this.bitsCount), hashesCount)
        pow(1f - pow(1-1f/this.bitsCount, this.hashesCount*this.elementsCount), hashesCount)
    }

    /**
      * Calculates hashes for the given string
      * @param str a string
      * @return list of indexes of bloom filter to become 1.
      */
    private def calculateHashes(str: String): List[Int] = {
        var result = List[Int]()
        for (i <- 0 until hashesCount) {
            // i is the seed
            // with different seed we have different number of results
            val hash = MurmurHash3.bytesHash(str.getBytes(), i)
            val index = Math.abs(hash) % bitsCount
            result = index :: result
        }

        return result
    }

    private def checkInputs(): Unit = {
        if (elementsCount < 1)
            throw new Exception("You must specify a positive number of expected Elements!")
        if (bitsCount < 1)
            throw new Exception("You must specify a positive number of expected bit Array length!")
        if (hashesCount < 1)
            throw new Exception("You must specify a positive number of hash functions! You can leave it blank an I will take care of it.")
    }
}
