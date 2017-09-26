"""
Contraction Clustering (RASTER): Data Generator
(c) 2016, 2017 Fraunhofer-Chalmers Centre for Industrial Mathematics

Author:
Gregor Ulm (gregor.ulm@fcc.chalmers.se)

Requirements:
. Python 3

This script has been developed and tested on Ubuntu Linux 16.04.

"""
import os
import random


def generate_data():

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


    f = open("output/data.csv", "w")

    for (x, y) in all_points:
        f.write(str(x) + "," + str(y) + "\n")

    f.close()




if __name__ == "__main__":

    generate_data()
