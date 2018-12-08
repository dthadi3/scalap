package scalap.hashing

import scala.util.Random

/**
  * MinHash implementation
  * Info: https://en.wikipedia.org/wiki/MinHash
  * @param documents map of documents (id, text)
  * @param length length of MinHash signature
  */
class MinHash(documents: Map[Int, String], var length: Int)
{
    val documentWords: Map[Int, Set[String]] = extractDocumentWords()
    val vocabulary: Map[String, Int] = documentWords.values.flatten.toSet.toIndexedSeq.zipWithIndex.toMap
    val hashFunctions: Seq[Int => Int] = generateHashFunctions(length)

    /**
      * Extracts Words from each document.
      * @return map (documentId, Set of words)
      */
    private def extractDocumentWords(): Map[Int, Set[String]] = {
        documents
            .map(document => {
                val documentId = document._1
                val words = document._2.split(" ").map(_.mkString).toSet
                (documentId, words)
            })
    }

    /**
      * Generates n hash Functions.
      * @param n number of functions to generate.
      * @return Sequence of hash functions
      */
    private def generateHashFunctions(n: Int): Seq[Int => Int] = {
        val slopes = Random.shuffle(0 to vocabulary.size)
        val intercepts = Random.shuffle(0 to vocabulary.size)
        val hashFunctionCoefficients = slopes.zip(intercepts).take(length)
        //hashFunctions.foreach(println)
        val hashFunctions = hashFunctionCoefficients.map(hashFunction =>
            (wordIndex: Int) =>
                calculateHash(hashFunction._1, hashFunction._2, wordIndex))

        return hashFunctions
    }

    /**
      * Generic hash function
      * @param slope function slope
      * @param intercept intercept
      * @param wordIndex word index to hash
      * @param mod upper limit (default is vocabulary size)
      * @return result
      */
    private def calculateHash(slope: Int, intercept: Int, wordIndex: Int, mod: Int = vocabulary.size): Int =
        (slope*wordIndex + intercept) % mod

    /**
      * Creates MinHash for a set of words.
      * Pseudocode:
      * for each row r do begin
      *     for each hash function hi do
      *         compute hi(r);
      *     for each column c
      *         if c has 1 in row r
      *             for each hash function hi do
      *                 if hi(r) is smaller than M(i,C) then
      *                     M(i,C) := hi(r);
      *
      * implemented in functional style
      * @param words set of words
      * @return MinHash signature vector
      */
    def getMinHash(words: Set[String]): Array[Double] = {
        val minHash = Array.fill[Double](length)(Double.PositiveInfinity)
        words
            .filter(word => vocabulary.contains(word))
            .foreach { word =>
                val wordIndex = vocabulary(word)
                var hashIndex = 0

                hashFunctions.foreach { hash =>
                    val permutedIndex = hash(wordIndex)

                    if (minHash(hashIndex) > permutedIndex)
                        minHash(hashIndex) = permutedIndex

                    hashIndex += 1;
                }
            }
        return minHash
    }

    /**
      * Calculates MinHash for all documents.
      * @return a map of (documentId, MinHash)
      */
    def getMinHashes(): Map[Int, Array[Double]] = {
        return this.documentWords.mapValues(wordSet => this.getMinHash(wordSet))
    }
}
