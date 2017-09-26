"""
Contraction Clustering (RASTER):
Reference Implementation in Python with an Example
(c) 2016, 2017 Fraunhofer-Chalmers Centre for Industrial Mathematics

Algorithm development and implementation:
Gregor Ulm (gregor.ulm@fcc.chalmers.se)
"""

import math
import numpy   as np
import pandas  as pd

from matplotlib import pyplot  as plt
from matplotlib import patches


def plot_cluster(all_points    : list,
                 cluster_points: list,
                 title         : str ):

    df1 = pd.DataFrame(all_points)
    df2 = pd.DataFrame(cluster_points)

    fig = plt.figure()
    ax1 = fig.add_subplot(111)

    df1.columns=['Latitude', 'Longitude']
    df2.columns=['Latitude', 'Longitude']

    ax1.scatter(df1['Longitude'], df1['Latitude'],
            color='black', alpha=0.5, s=10)

    ax1.scatter(df2['Longitude'], df2['Latitude'],
            color='red', alpha=0.5, s=10)

    plt.title(title)
    plt.show()



def plot_cluster_tiles(all_points    : list,
                       cluster_points: list,
                       title         : str ):

    df1 = pd.DataFrame(all_points)
    df2 = pd.DataFrame(cluster_points)

    fig = plt.figure()
    ax1 = fig.add_subplot(111)

    df1.columns=['Latitude', 'Longitude']
    df2.columns=['Latitude', 'Longitude']

    ax1.scatter(df1['Longitude'], df1['Latitude'],
            color='black', alpha=0.5, s=10)

    ax1.scatter(df2['Longitude'], df2['Latitude'],
            color='red', alpha=0.5, s=10)

    plt.title(title)
    plt.show()



def plot_list(hubs: list, title: str):

    df = pd.DataFrame(hubs)
    return plot_dataframe(df, title)



def plot_dataframe(df, title: str):

    df.columns=['Latitude', 'Longitude']

    plt.figure()

    plt.scatter(df['Longitude'], df['Latitude'],
            color='green', alpha=0.5, s=1)

    plt.title(title)
    plt.ylabel('Latitude')
    plt.xlabel('Longitude')
    plt.grid()

    plt.tight_layout()

    plt.show()
