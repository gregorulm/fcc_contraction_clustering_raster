/**
Contraction Clustering (RASTER):
Reference Implementation in Scala with an Example
(c) 2016, 2017 Fraunhofer-Chalmers Centre for Industrial Mathematics

Algorithm development and implementation:
Gregor Ulm (gregor.ulm@fcc.chalmers.se)

This demo has been developed using Scala 2.11.6 and JVM 1.8.0
on Ubuntu Linux 16.04.

For a description of the algorithm including relevant theory, please
consult our paper on Contraction Clustering (RASTER).
**/

import java.io._
import scala.io.Source
import scala.util.Try

type Point      = (Double, Double);
type Point_Int  = (Int, Int);
type Map_Points = Map[(Int, Int), Int];
type Set_Points = Set[Point_Int];
type Output     = List[(Int, Double, Double)];


def all_points(points: List[Point],
               acc   : Map_Points ,
               scalar: Int       ): Map_Points =
{
  if (points.size == 0) {
    acc
  }
  else {
    val (x, y)::xs = points
    val x2         = (math floor (x * scalar)).toInt
    val y2         = (math floor (y * scalar)).toInt

    val z = acc.get((x2, y2)) match {
      case Some(count) => count + 1
      case None        => 1
    }

    all_points(xs, acc + ((x2, y2) -> z), scalar)
  }
}



def map_to_tiles(points   : List[Point],
                 precision: Int        ,
                 threshold: Int        ): (Iterable[(Int, Int)], Int) =
{
  val scalar = scala.math.pow(10, precision).toInt
  val ps     = all_points(points, Map(), scalar)
  val tiles  = ps.filter((t) => t._2 >= threshold).keys
  (tiles, scalar)
}



def get_neighbors(point: (Int, Int)      ,
                  tiles: Set[(Int, Int)] ): Set[(Int, Int)] =
{
  val (x, y)    = point
  val neighbors = Set((x + 1, y    ),
                      (x - 1, y    ),
                      (x    , y + 1),
                      (x    , y - 1),
                      (x + 1, y - 1),
                      (x + 1, y + 1),
                      (x - 1, y - 1),
                      (x - 1, y + 1))
  neighbors.filter ((t) => tiles.contains(t))
}



def cluster_one(to_check: Set_Points,
                tiles   : Set_Points,
                acc     : Set_Points): Set_Points =
{
  if (to_check.isEmpty) {
    acc
  }
  else {
    val x         = to_check.head
    val acc2      = acc + x
    val neighbors = get_neighbors(x, tiles)
    val filtered  = neighbors.filter((t) => !acc2.contains(t))

    cluster_one(filtered.union(to_check.tail), tiles, acc2)
  }
}



def cluster_all(tiles   : Set_Points      ,
                min_size: Int             ,
                acc     : List[Set_Points]): List[Set_Points] =
{
  if (tiles.isEmpty) {
    acc
  }
  else {
    val start   = Set(tiles.head)
    val cluster = cluster_one(start, tiles, Set())
    val tiles2  = tiles.diff(cluster)

    val acc2    = if (cluster.size >= min_size) {
                    cluster::acc
                  } else {
                    acc
                  }

    cluster_all(tiles2, min_size, acc2)
  }
}



def scale(point : (Int, Int),
          scalar: Int       ,
          i     : Int       ) : (Int, Double, Double) =
{
  val (x, y) = point
  (i, (x * 1.0) / scalar, (y * 1.0) / scalar)
}



def number_clusters(clusters: List[Set_Points],
                    acc     : Output          ,
                    scalar  : Int             ,
                    i       : Int             ): Output =
{
  if (clusters.isEmpty) {
    acc
  }
  else {
    val c::cs    = clusters
    val numbered = (c.toList).map((t) => scale(t, scalar, i))
    val acc2     = numbered ++ acc
    number_clusters(cs, acc2, scalar, (i + 1))
  }
}



def make_tuple(s: String): (Double, Double) =
{ // Note that valid input is assumed
  val point = s.trim.split(",")
  val x     = point(0).toDouble
  val y     = point(1).toDouble
  (x, y)
}



def prepare_output(numbered:List[(Int, Double, Double)],
                   acc     :List[String]               ):List[String] =
{
  if (numbered.isEmpty) {
    acc
  }
  else {
    val (num, x, y)::xs = numbered
    val line            = num + ", " + x + ", " + y + "\n"
    prepare_output(xs, line::acc)
  }
}

def main =
{
  val file   = Source.fromFile("input/sample.csv")
  val lines  = file.getLines.toList
  val points = lines.map((t) => make_tuple(t))
  file.close

  // step 1: projection
  val threshold = 5
  val precision = 1
  // tiles: significant tiles
  val (tiles, scalar) = map_to_tiles(points, precision, threshold)

  // step 2: agglomeration
  val min_size = 5
  val clusters = cluster_all(tiles.toSet, min_size, List())
  // clusters.foreach(println)
  val numbered = number_clusters(clusters, List(), scalar, 1)

  println("Number of clusters: " + clusters.size)
  // numbered.foreach(println)

  val output   = prepare_output(numbered, List())
  val header   = "Cluster Number, X-Position, Y-Position\n"
  val file_out = "output/clustered.csv"
  val writer   = new BufferedWriter(new FileWriter(file_out))
  (header::output).foreach(writer.write)
  writer.close()
}
