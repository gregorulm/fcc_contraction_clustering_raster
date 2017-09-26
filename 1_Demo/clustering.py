"""
Contraction Clustering (RASTER):
Reference Implementation in Python with an Example
(c) 2016, 2017 Fraunhofer-Chalmers Centre for Industrial Mathematics

Algorithm development and implementation:
Gregor Ulm (gregor.ulm@fcc.chalmers.se)

"""

import math
import random


# retains number of observations
def mapToTiles_Tiles(points   : list,
                     precision: int ,
                     threshold: int ) -> (dict, int):
    """
    The key idea behind this function is to reduce the precision of
    spatial coordinates. These coordinates are assigned to the
    bottom-left corner of an imaginary tile, which is defined by the
    reduced precision. For instance, the tile corner (50.0212, 1.1123)
    can be used to reduce all points (50.0212__, 1.1123__) to one
    single point.

    """

    scalar    = 10 ** precision
    allPoints = dict()

    for (lat, lon) in points:

        lat = int(lat * scalar)
        lon = int(lon * scalar)

        if (lat, lon) in allPoints.keys():
            allPoints[(lat, lon)] += 1

        else:
            allPoints[(lat, lon)] = 1

    # filter results to only retain tiles that contain at lest the
    # provided threshold value of observations
    result = set()
    for k in allPoints.keys():
        if allPoints[k] >= threshold:
            result.add(k)

    return (result, scalar)



# retains points
def mapToTiles_Points(points   : list,
                      precision: int ,
                      threshold: int ) -> (dict, int):

    scalar    = 10 ** precision
    allPoints = dict()

    for (x, y) in points:

        # avoid floating point issues
        lat = int(x * scalar)
        lon = int(y * scalar)

        if (lat, lon) in allPoints.keys():
            vals = allPoints[(lat, lon)]
            vals.add((x, y))
            allPoints[(lat, lon)] = vals

        else:
            vals = set()
            vals.add((x, y))
            allPoints[(lat, lon)] = vals

    # filter out tiles that are below the provided threshold value
    # k: tile; v: points
    projection = dict()

    for k in allPoints.keys():
        if len(allPoints[k]) >= threshold:
            projection[k] = allPoints[k]

    return (projection, scalar)



def getNeighbors(coordinate: tuple, squares: set):
    # neighbor lookup in O(1)
    assert coordinate in squares

    result = []

    (x, y) = coordinate
    assert isinstance(x, int)
    assert isinstance(y, int)

    # 8-way clustering
    neighbors  = [(x + 1, y    ),
                  (x - 1, y    ),
                  (x    , y + 1),
                  (x    , y - 1),
                  (x + 1, y - 1),
                  (x + 1, y + 1),
                  (x - 1, y - 1),
                  (x - 1, y + 1)]

    for n in neighbors:
        if n in squares:
            result.append(n)

    return result



def raster_clustering_points(squares: list, min_size: int):
    """
    Note: needs integers for GPS coordinates
    """

    sq = set(squares)

    clusters = []
    centers  = []

    while not (len(sq) == 0):

        # pick an arbitrary point as starting point for new cluster
        x = random.sample(sq, 1)[0]

        visited = set()
        visited.add(x)

        # collect all neighbors
        to_check = getNeighbors(x, sq)

        while not (to_check == []):

            # pop a coordinate off 'to_check'; get their neighbors
            val        = to_check.pop()
            visited.add(val)
            candidates = getNeighbors(val, sq)

            for c in candidates:
                if c not in visited:
                    to_check.append(c)


        # remove all points from set
        sq          = sq - visited
        new_cluster = list(visited)

        if len(new_cluster) >= min_size:
            clusters.append(new_cluster)

    return clusters



def raster_clustering_tiles(squares: list, min_size: int):

    squares  = set(squares)
    clusters = []
    centers  = []

    while not (len(squares) == 0):

        # pick an arbitrary point as starting point for new cluster
        x = random.sample(squares, 1)[0]

        visited  = set()
        visited.add(x)

        # collect all neighbors
        to_check = getNeighbors(x, squares)

        while not (to_check == []):

            # pop a coordinate off 'to_check'; get their neighbors
            val        = to_check.pop()
            visited.add(val)
            candidates = getNeighbors(val, squares)

            for c in candidates:
                if c not in visited:
                    to_check.append(c)


        # remove all points from set
        squares = squares - visited

        if len(list(visited)) >= min_size:
            # add to list of clusters
            clusters.append(visited)

    return clusters
