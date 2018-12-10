package scalap.hashing.datatypes

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Band()
{
    val buckets: mutable.Map[List[Double], ArrayBuffer[Int]] = mutable.Map[List[Double], ArrayBuffer[Int]]()

    def hash(subArray: (Int, Array[Double])): Unit = {
        buckets.get(subArray._2.toList) match {
            case Some(value: ArrayBuffer[Int]) => value += subArray._1;
            case None => buckets(subArray._2.toList) = ArrayBuffer(subArray._1)
        }
    }

    def getCollisionObjects(subArray: Array[Double]): Option[List[Int]] = {
        buckets.get(subArray.toList) match {
            case Some(value: ArrayBuffer[Int]) => Some(value.toList);
            case None => buckets(subArray.toList) = ArrayBuffer(-1); None
        }
    }
}