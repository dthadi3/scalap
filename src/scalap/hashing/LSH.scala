package scalap.hashing

/**
  * Locality Sensitivity Hashing Implementation
  * Info:
  *  - https://en.wikipedia.org/wiki/Locality-sensitive_hashing
  *  - https://towardsdatascience.com/understanding-locality-sensitive-hashing-49f6d1f6134
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
    var minHashes: Map[Int, Array[Double]] = minHash.getMinHashes()
    var bands: IndexedSeq[LSHBand] = createBands()

    /**
      * Creates all (numberOfBands) bands.
      * @return bands
      */
    private def createBands(): IndexedSeq[LSHBand] = {
        (0 until numberOfBands).map({ bandIndex =>
            val rowsPerBand = (minHashLength.toFloat / numberOfBands).ceil.toInt
            val start = bandIndex * rowsPerBand
            val end = if (bandIndex == numberOfBands - 1) minHashLength else start + rowsPerBand
            val hashSigPart = minHashes.map(document => (document._1, document._2.slice(start, end)))
            val band = new LSHBand()
            hashSigPart.foreach(array => band.add(array))
            band
        })
    }

    /**
      * Finds similar documents by performing actual similarity check in the candidates set
      * and filtering out those who's similarity is lower than the given threshold.
      * @param document string document
      * @return
      */
    def findSimilar(document: String, threshold: Double = this.threshold): Set[Int] = {
        val words = document.split(" ").map(_.mkString).toSet
        val candidates = findCandidates(words)
        candidates.filter(candidate => similarity(words, minHash.documentWords(candidate)) > threshold)
    }

    /**
      * Finds candidates by calling findCandidates on each band.
      * @param words a set of words (bag of words)
      * @return set of candidates
      */
    private def findCandidates(words: Set[String]): Set[Int] = {
        val minHashSig = minHash.getMinHash(words)
        val minHashParts = partitionMinHashValues(minHashSig).zipWithIndex
        val candidates = minHashParts.flatMap({ subArray =>
            val index = subArray._2
            val hashedBucket = bands(index).findCandidates(subArray._1)
            hashedBucket
        }).flatten.toSet
        candidates
    }


    /**
      * Partitions a min hash signature in numberOfBands pieces.
      * @param minHash a min hash signature
      * @return sliced min hash signature
      */
    private def partitionMinHashValues(minHash: Array[Double]): IndexedSeq[Array[Double]] = {
        val elementsPerBand = minHash.length / numberOfBands
        (0 until numberOfBands).map { bandIndex =>
            val start = bandIndex * elementsPerBand
            val end = start + elementsPerBand
            minHash.slice(start, end)
        }
    }

    /**
      * Performs actual similarity (Jaccard coefficient) between two sets of words.
      * @param set1 a set of words (bag of words)
      * @param set2 a set of words (bag of words)
      * @return Jaccard similarity of two documents
      */
    private def similarity(set1: Set[String], set2: Set[String]): Double = {
        val intersection: Double = set1.intersect(set2).size.toFloat
        val union = set1.union(set2).size
        intersection / union
    }
}
