package scalap.hashing

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Represent a LSH Band. with some buckets
  */
class LSHBand()
{
    /**
      * It's a map of
      * minHashPart -> Array of document ids
      */
    val buckets: mutable.Map[String, ArrayBuffer[Int]] = mutable.Map[String, ArrayBuffer[Int]]()

    /**
      * Adds minhash part to a bucket.
      * Number of buckets is automatic, we don't generate a bucket if we don't need to.
      * @param minHashPart Map ( documentId, a part of the MinHash signature of the document)
      */
    def add(minHashPart: (Int, Array[Double])): Unit = {
        buckets.get(minHashPart._2.map(_.toInt).mkString("")) match {
            case Some(value: ArrayBuffer[Int]) => value += minHashPart._1
            case None => buckets(minHashPart._2.map(_.toInt).mkString("")) = ArrayBuffer(minHashPart._1)
        }
    }

    /**
      * Finds candidates in order to actually perform similarity check
      * @param minHashPart a part of the MinHash signature of the document
      * @return
      */
    def findCandidates(minHashPart: Array[Double]): Option[List[Int]] = {
        buckets.get(minHashPart.map(_.toInt).mkString("")) match {
            case Some(value: ArrayBuffer[Int]) => Some(value.toList);
            case None => buckets(minHashPart.map(_.toInt).mkString("")) = ArrayBuffer(-1); None
        }
    }

    /**
      * @return the number of active buckets of the Band.
      */
    def numberOfActiveBuckets(): Int = buckets.size
}