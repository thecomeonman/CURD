% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TraverseTreeToMeasureClassificationQuality.R
\name{TraverseTreeToMeasureClassificationQuality}
\alias{TraverseTreeToMeasureClassificationQuality}
\title{FUNCTION: TEST PERFORMANCE OF CLUSTER TREE ON LABELED DATA
This function will traverse the tree to various depths and create a dataset
with a row for each node in the tree. This dataset will contain details of
how many points that node contains, whether or not it is a leaf node
and what the proportion of various classes is on that node.
Further, on the assumption that a node is assigned to a particular class, it
tells us how accurate each node is (based on the impurity level)
Depth-first traversal of the tree happens}
\usage{
TraverseTreeToMeasureClassificationQuality(dtTreeSummary, dtDataset, lTree,
  cClassLabelFeat)
}
\arguments{
\item{dtTreeSummary}{todo}

\item{dtDataset}{todo}

\item{lTree}{todo}

\item{cClassLabelFeat}{todo}
}
\value{
todo
}
\description{
FUNCTION: TEST PERFORMANCE OF CLUSTER TREE ON LABELED DATA
This function will traverse the tree to various depths and create a dataset
with a row for each node in the tree. This dataset will contain details of
how many points that node contains, whether or not it is a leaf node
and what the proportion of various classes is on that node.
Further, on the assumption that a node is assigned to a particular class, it
tells us how accurate each node is (based on the impurity level)
Depth-first traversal of the tree happens
}
