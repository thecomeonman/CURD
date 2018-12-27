Introduction
============

To the extent of our knowledge, there are four issues that are somewhat
under-explored in the existing body of research on clustering
algorithms, and therefore represent an opportunity for improvement.

-   Dealing with categorical features: In practice, categorical features
    have often been transformed to numeric ones through the use of
    indicator variables for each category. This is not optimal in case
    of ordinal features, or when there are many categories.
-   Interpretability: In real life problems such as market segmentation,
    the ability to interpret clusters and verify them against business
    knowledge is a key factor that drives adoption. Most clustering
    results require considerable post-processing effort to aid
    interpretation.
-   Stability: Algorithms whose results are likely to vary drastically
    based on small changes in initial conditions (e.g. k-means) are not
    desirable in many circumstances \[6,7\].
-   Scalability: Clustering algorithms of quadratic complexity or higher
    (such as DBSCAN or hierarchical clustering) cannot deal efficiently
    with large datasets.

Decision tree-based clustering approaches offer a way to address the
above issues in a reasonable manner: (a) the idea of splitting a node
based on values of a feature is perhaps most easily applied to
categorical data; (b) the tree structure allows for easy
interpretability (each cluster can be viewed as an if-then rule leading
from the root of the tree, i.e., the entire dataset to the leaf node,
i.e., the cluster) and the ability to discover hierarchies among
clusters; (c) the approach leads to stable results that do not vary
across multiple iterations and; (d) since the approach works on
increasingly smaller versions of the dataset, the complexity is
sub-quadratic.

Existing tree-based methods in the literature \[8\] are built on the
following assumption: clusters represent non-random agglomerations of
points in the feature space and can therefore be discovered by a
decision tree that attempts to distinguish between points in the actual
dataset and randomly generated points. While this approach shows much
promise, its effectiveness is also predicated on the precise assumptions
underlying the generation of random data.

We propose a different approach to tree construction: we hypothesize
that a non-random agglomeration would manifest itself through
interdependence between the features. In other words, if one categorical
feature can be well expressed in terms of the others, then it represents
a separation of points in the feature space. We apply this principle
inside a recursive partitioning algorithm to generate the tree, and
therefore the clusters.

Methodology
===========

The principal intuition behind the design of our proposed algorithm is
as follows: If we wish to split the node in a tree based on a feature,
it has to be one that divides the dataset into distinct groups. A useful
heuristic, therefore, is to choose the feature that can be best
predicted using the others. Further splits can be based on the same
principle, subject to caveats such as the number of data points left for
splitting.

Running the Model
=================

We will use the UCI voting dataset. ( Add reference. )

There is some pre-processing that needs to happen on the data. This
should be handled within the algorithm's code but for now it's
separately listed outside.

The clustering algo can be run on this dataset using the function
`RecursivePartitioningTreeClustering`.

Exploring the Results
=====================

Using the `AssignCluster` function, we can predict the clustering
assignment for each observation in the dataset.

There are various other functions to help us analyse the results
further. These functions are needed only on such demonstrations since on
actual problems we won't have access to the actual labels of the
dataset.

![](/tmp/RtmpCaVQKp/preview-348a770c892d.dir/CURD_files/figure-markdown_strict/ResultsExploration00-1.png)

![](/tmp/RtmpCaVQKp/preview-348a770c892d.dir/CURD_files/figure-markdown_strict/ResultsExploration01-1.png)

![](/tmp/RtmpCaVQKp/preview-348a770c892d.dir/CURD_files/figure-markdown_strict/ResultsExploration02-1.png)

![](/tmp/RtmpCaVQKp/preview-348a770c892d.dir/CURD_files/figure-markdown_strict/ResultsExploration03-1.png)

Performance on other algorithms
===============================

We will compare the algorithm with hierarchical cluster by Ward method
and K-means.

Elaborate on the details.

![](/tmp/RtmpCaVQKp/preview-348a770c892d.dir/CURD_files/figure-markdown_strict/ComparingOtherAlgos01-1.png)

![](/tmp/RtmpCaVQKp/preview-348a770c892d.dir/CURD_files/figure-markdown_strict/ComparingOtherAlgos02-1.png)

![](/tmp/RtmpCaVQKp/preview-348a770c892d.dir/CURD_files/figure-markdown_strict/ComparingOtherAlgos03-1.png)

![](/tmp/RtmpCaVQKp/preview-348a770c892d.dir/CURD_files/figure-markdown_strict/ComparingOtherAlgos06-1.png)
