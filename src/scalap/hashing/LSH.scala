package scalap.hashing

import scalap.hashing.datatypes.Band

/**
  * Locality Sensitivity Hashing Implementation
  * Info: https://en.wikipedia.org/wiki/Locality-sensitive_hashing
  *
  * @param minHashLength MinHash length
  * @param numberOfBands number of Bands
  * @param documents map of documents (id, text)
  * @param threshold similarity threshold
  */
@throws[IllegalArgumentException]
class LSH(minHashLength: Int = 100,
          numberOfBands: Int,
          documents: Map[Int, String],
          threshold: Double = 0.5) {

    if (minHashLength < numberOfBands) {
        throw new IllegalArgumentException("Number of bands exceeds minHash length")
    }

    var minHash = new MinHash(documents, minHashLength)
    var bands: IndexedSeq[Band] = _

    createHash()

    def createHash(): Unit = {
        val minHashCollection = minHash.getMinHashes()
//        println("minHash collection");
//        minHashCollection.foreach(x => println(x._2.toList))
        val bands =
            (0 until numberOfBands).map { bandIndex =>
                val elementsPerBand = (minHashLength.toFloat / numberOfBands).ceil.toInt
                val start = bandIndex * elementsPerBand
                val end = if (bandIndex == numberOfBands - 1) minHashLength else start + elementsPerBand
                val subArray = minHashCollection.map(document => (document._1, document._2.slice(start, end)))
                val band = new Band()
                subArray.foreach(array => band.hash(array))
                band
            }
        this.bands = bands
    }

    def findSimilar(document: String): Set[Int] = {
        val words = document.split(" ").map(_.mkString).toSet
        val candidates = findCandidates(words)
        candidates.filter(candidate => similarity(words, minHash.documentWords(candidate)) > threshold)
    }

    private def findCandidates(words: Set[String]): Set[Int] = {
        val minHashSig = minHash.getMinHash(words)

        val subArrays = partitionArray(minHashSig).zipWithIndex
        //subArrays.foreach(x => println("subarray is " + x._1.toList))

        val candidates = subArrays.flatMap { subArray =>
            val index = subArray._2
            val hashedBucket = bands(index).getCollisionObjects(subArray._1)
            // println(hashedBucket)
            hashedBucket
        }.flatten.toSet
        candidates
    }

    private def partitionArray(minHash: Array[Double]): IndexedSeq[Array[Double]] = {
        // first assume they are proper multiples
        val elementsPerBand = minHash.length / numberOfBands
        (0 until numberOfBands).map { bandIndex =>
            val start = bandIndex * elementsPerBand
            val end = start + elementsPerBand
            minHash.slice(start, end)
        }
    }

    private def similarity(set1: Set[String], set2: Set[String]): Double = {
        val intersection: Double = set1.intersect(set2).size.toFloat
        val union = set1.union(set2).size
        return intersection / union
    }
}
