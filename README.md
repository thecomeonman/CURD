This document demoes the algorithm.

We will use the UCI voting dataset. ( Add reference. )

There is some pre-processing that needs to happen on the data. This
should be handled within the algorithm's code but for now it's
separately listed outside.

The clustering algo can be run on this dataset using the function
`RecursivePartitioningTreeClustering`.

Model details
=============

Using the `AssignCluster` function, we can predict the clustering
assignment for each observation in the dataset.

There are various other functions to help us analyse the results
further. These functions are needed only on such demonstrations since on
actual problems we won't have access to the actual labels of the
dataset.

![](CURD_files/figure-markdown_strict/ResultsExploration00-1.png)

![](CURD_files/figure-markdown_strict/ResultsExploration01-1.png)

![](CURD_files/figure-markdown_strict/ResultsExploration02-1.png)

![](CURD_files/figure-markdown_strict/ResultsExploration03-1.png)

Performance on other algorithms
===============================

We will compare the algorithm with hierarchical cluster by Ward method
and K-means.

Elaborate on the details.

![](CURD_files/figure-markdown_strict/ComparingOtherAlgos01-1.png)

![](CURD_files/figure-markdown_strict/ComparingOtherAlgos02-1.png)

![](CURD_files/figure-markdown_strict/ComparingOtherAlgos03-1.png)

![](CURD_files/figure-markdown_strict/ComparingOtherAlgos06-1.png)
