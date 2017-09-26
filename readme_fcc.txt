Contraction Clustering (RASTER): Demo and Reference Implementations

(c) 2016, 2017 Fraunhofer-Chalmers Centre for Industrial Mathematics,
    Department of Systems and Data Analysis

Algorithm development and implementation:
Gregor Ulm (gregor.ulm@fcc.chalmers.se)



Description
-----------
Contraction Clustering (RASTER) is a liner-time constant-memory big data
clustering algorithm, which was developed at FCC with the purpose of
performing fast clustering of huge sets of GPS data.

The implementations presented here differ from our internal
implementation. The latter is used mainly for constructing time-evolving
network graphs, where nodes are the approximate centers of clusters.
Furthermore, that implementation uses constant memory because the used
data, fixed-precision GPS coordinates, is bounded. When using an
unbounded domain, such as coordinates expressed in natural numbers,
RASTER no longer requires only constant memory. As a further note, we
have excluded functions that handle idiosyncracies of projecting points
onto a sphere into account.

Consequently, the implementations in this folder illustrate the main
idea of RASTER. For industrial applications you may need to make
modifications that take the idiosyncracies of your data into account.




Licence
-------
The code in this repository is released unter the MIT License.



Content
-------
Folders:

. 1_Demo:
  Python 3 demo of RASTER, includng a visualization using matplotlib

. 2_Demo_Sample_Output
  Sample output as PDF of executing the demo in the folder above;
  results are presented as clusters of tiles as well as clusters of
  points

. 3_Generator
  Tool for generating sample input data consisting of two-dimensional
  points

The following folders contain reference implementations that process
a sample CSV file and produce a CSV file containing a description of
resulting clusters as projections to so-called significant tiles.

. 4_Python
  Implemented and tested with Python 3.5.2

. 5_Erlang
  Implemented and tested with Erlang/OTP 18

. 6_Haskell
  Implemented and tested with GHC 7.10.3

. 7_Scala
  Implemented and tested with Scala 2.11.6 and JVM 1.8.0