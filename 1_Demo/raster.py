"""
Contraction Clustering (RASTER):
Reference Implementation in Python with an Example
(c) 2016, 2017 Fraunhofer-Chalmers Centre for Industrial Mathematics

Algorithm development and implementation:
Gregor Ulm (gregor.ulm@fcc.chalmers.se)

Requirements:
. Python 3
. external libraries: numpy, pandas, matplotlib


This demo has been developed and tested on Ubuntu Linux 16.04.

For a description of the algorithm including relevant theory, please
consult our paper on Contraction Clustering (RASTER).

"""

import os
import random

# user-defined modules
import clustering           as c
import visualizing          as v



if __name__ == "__main__":

    os.system('clear')

    print("Select Contraction Clustering (RASTER) demo:")
    print("1: Clustering using signficant tiles")
    print("2: Ditto, but visualizing clusters with constituent points.")

    choice = None

    while True:
        print("Enter '1' or '2':")
        selection = input()
        selection = selection.strip()

        if selection == "1" or selection == "2":
            choice = selection
            break

        continue


    # Generating input data

    NUM_CLUSTERS = 7
    centers      = []
    all_points   = []

    # cluster centers
    while len(centers) < NUM_CLUSTERS:

        x = random.uniform(0.0, 15.0)
        y = random.uniform(0.0, 15.0)

        valid = True
        for (a, b) in centers:

            # ensure cluster centers are not too closely placed
            min_dist = 1.5

            if abs(x - a) < min_dist or abs(y - b) < min_dist:
                valid = False
                break

        if valid:
            centers.append((x, y))


    # spread points around center
    for (x, y) in centers:

        size = random.randint(500, 800)

        for i in range(0, size):
            # spread is random, too
            z   = random.uniform(0.0, 4.5)
            eps = random.uniform(0.0, z)
            p1  = random.uniform(x - eps, x + eps)
            p2  = random.uniform(y - eps, y + eps)

            all_points.append((p1, p2))



    """
    1) RASTER clusters

    RASTER projects points to tiles and disregards the former after the
    projection has been performed. Thus, it requires merely constant
    space, assuming bounded integers or a bounded coordinate system like
    the GPS coordinate system for out planet.
    
    Input is projected to points that represent tiles.

    """

    # Note: there is significant but delibreate repetition in the two
    #       if-clauses below; however, the goal is to make both
    #       variations of RASTER easier to follow, so it seemed like a
    #       reasonable trade-off

    if choice == "1":

        ## Step 1: Projection
        threshold       = 5
        (tiles, scalar) = c.mapToTiles_Tiles(all_points, 1, threshold)

        # Plot entire input
        v.plot_list(all_points, "All points")

        # Plot tiles containing >= threshold points
        tiles        = list(tiles)
        tiles_scaled = []

        for (x, y) in tiles:
            x = x / scalar
            y = y / scalar
            tiles_scaled.append((x ,y))

        # Plot entire input and superimpose tile positions (red)
        v.plot_list(tiles_scaled , "Tiles >= threshold")

        v.plot_cluster(all_points, tiles_scaled,
            "Tile positions (lower left corner) superimposed")

        # Note: points are not yet clustered!

        ## Step 2: Agglomeration
        min_size = 5
        clusters = c.raster_clustering_tiles(tiles, min_size)
        print("Number of clusters: ", len(clusters))

        # Plot input and resulting clusters
        v.plot_cluster(
            all_points, tiles_scaled,
            "Clusters of tile positions superimposed")



    """
    2) RASTER clusters specified by the points that constitute clusters

    Note that this example only serves as a visualization. The standard
    algorithm described in our RASTER paper only retains tiles and drops
    points, due the goal of processing huge datasets.

    """
    if choice == "2":

        ## Step 1: Projection
        threshold        = 5
        (mapped, scalar) = c.mapToTiles_Points(all_points, 1, threshold)

        # Plot entire input
        v.plot_list(all_points, "All points")

        # Plot tiles containing >= threshold points
        tiles        = list(mapped.keys())
        tiles_scaled = []

        for (x, y) in tiles:
            x = x / scalar
            y = y / scalar
            tiles_scaled.append((x ,y))

        v.plot_list(tiles_scaled, "Tiles >= threshold")

        # Plot points mapped to tiles
        points = []
        for x in mapped:
            points += mapped[x]

        v.plot_list(points, "Retained points for tiles")

        # Plot entire input and superimpose retained points (red)
        v.plot_cluster(all_points, points,
            "Retained points superimposed")

        # Note: points are not yet clustered!

        ## Step 2: Agglomeration
        min_size = 5
        clusters = c.raster_clustering_points(tiles, min_size)
        print("Number of clusters: ", len(clusters))

        tiles = []
        for x in clusters:
            tiles += x

        final_points = []
        for t in tiles:
            final_points += mapped[t]

        # Plot input and resulting clusters
        v.plot_cluster(
            all_points, final_points, "Clusters of points superimposed")
