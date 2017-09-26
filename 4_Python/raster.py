"""
Contraction Clustering (RASTER):
Reference Implementation in Python with an Example
(c) 2016, 2017 Fraunhofer-Chalmers Centre for Industrial Mathematics

Algorithm development and implementation:
Gregor Ulm (gregor.ulm@fcc.chalmers.se)

Requirements:
. Python 3
. external libraries: numpy, pandas


This demo has been developed and tested on Ubuntu Linux 16.04.

For a description of the algorithm including relevant theory, please
consult our paper on Contraction Clustering (RASTER).

"""

import os

# user-defined modules
import clustering as c



if __name__ == "__main__":

    os.system('clear')

    # load input data
    with open("input/sample.csv", "r") as f:
        content = f.readlines()

    all_points = []

    for line in content:

        line   = line.strip()
        (x, y) = line.split(",")
        x      = float(x)
        y      = float(y)

        all_points.append((x, y))


    """
    1) RASTER clusters

    RASTER projects points to tiles and disregards the former after the
    projection has been performed. Thus, it requires merely constant
    space, assuming bounded integers or a bounded coordinate system like
    the GPS coordinate system for out planet.

    Input is projected to points that represent tiles.

    """

    ## Step 1: Projection
    threshold       = 5
    (tiles, scalar) = c.mapToTiles_Tiles(all_points, 1, threshold)

    tiles           = list(tiles)

    """
    tiles = sorted(tiles)
    for x in tiles:
        print(x)
    exit()
    """

    ## Step 2: Agglomeration
    min_size = 5
    clusters = c.raster_clustering_tiles(tiles, min_size)
    print("Number of clusters: ", len(clusters))


    output = []
    count  = 1
    for cluster in clusters:

        for (x, y) in cluster:
            x = x / scalar
            y = y / scalar
            output.append((count, x, y))

        count += 1


    f = open("output/clustered.csv", "w")
    f.write("Cluster Number, X-Position, Y-Position\n")
    for (num, x, y) in output:
        f.write(str(num) + ", " + str(x) + ", " + str(y) + "\n")
    f.close()


