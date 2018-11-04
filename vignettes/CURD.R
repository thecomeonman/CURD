## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 12,
  fig.height = 12
)

## ----RunParameters, echo = FALSE, message = F, warning = F---------------

# CLEAR THE WORKSPACE
rm(list = ls())
set.seed(1)
# gc()

library(data.table)

library(CURD)
library(randomForest)
library(rpart)
library(cluster)
library(clue)

library(ggplot2)
library(ggrepel)
library(gridExtra)
library(scales)

library(DT)

theme_set(theme_bw(12))

# After running the model, should there also be some summaries created?
bDoAnalysis = T

# Should the results be compared with some other algos
bCompareOtherAlgos = T
bDoKMeans = T
bDoHierarachical = T
# Comparing MBKM needs some other code which we can't publish
# at the moment.
bDoMBKM = F


## ----LoadingData, echo = FALSE, message = F, warning = F-----------------

# You can replace dtUCIVotingRecords with any other dataset you want to 
# run this on. Review the LoadingData chunk and the ModelParameters chunk.

# Removing missing data rows for convenience
dtDataset = dtUCIVotingRecords[
   !apply(
      dtUCIVotingRecords, 
      1,
      function(x) { 
         any(x == '?')
      }
   )
]

# rm(dtUCIVotingRecords)

dtDataset = dtDataset[complete.cases(dtDataset)]

setnames(
   dtDataset,
   make.names(colnames(dtDataset))
)


## ----ModelParameters, echo = FALSE, message = F, warning = F-------------

cClassLabelFeat = 'Class'

# Choose either RF or Rpart right now. If only one independent feature remains,
# it defaults to Rpart
cClassifier = "RF"

# number of layers the tree is allowed to have
nMaxTreeDepth = 6

# Explain
bKeepFeatureInMixAfterSplit = FALSE

# Explain
bKeepFeatureInPredAfterSplit = TRUE

# Explain
bUseOptimalChunkPerformance = TRUE

# Features to use (for predicting, node splitting)
# todo, what's the difference between vNodeCandidates and vPredictorFeatures?
vNodeCandidates = setdiff(
   colnames(dtDataset), 
   cClassLabelFeat
)

vPredictorFeatures = setdiff(
   colnames(dtDataset), 
   cClassLabelFeat
)

# how many observations must be there for a split to be attempted
nMinRows = max(
   5, 
   round(0.1 * nrow(dtDataset))
)

# Some other columns that will be required
vKeyFeatures = 'ID'
dtDataset[, ID := .I]
dtDataset[, ClusteringRule := "Root"]
dtDataset[, ParentRule := "Root"]
dtDataset[, NodeLabel := "Root"]
dtDataset[, Depth := 1]

vRows = 1:nrow(dtDataset)

# Initial node
lRootNode = list(
   cNodeLabel = 'Root',
   cClusteringRule = "Root",
   nDepth = 1,
   vRows = vRows,
   cParentSplitFeat = "",
   vParentSplitVals = c(),
   cSplitFeat = ""
)

# Mark out ordinal features
# These can be handled better than categorical features
vcOrdinalFeatures = c()

# Listing out features and associated details
dtFeatureChunking = data.table(
   NodeCandidate = vNodeCandidates
)

dtFeatureChunking[, 
   FeatureType := 'Nominal' 
]

dtFeatureChunking[
   NodeCandidate %in% vcOrdinalFeatures, 
   FeatureType := 'Ordinal'
]

# We can allow for chunking of ordinal features, i.e. grouping neighbouring
# ordinal values into one bucket
dtFeatureChunking[, 
   Chunk := FALSE 
]

dtFeatureChunking[
   NodeCandidate %in% vcOrdinalFeatures, 
   Chunk := T 
]
dtFeatureChunking[
   NodeCandidate %in% vcOrdinalFeatures, 
   MinNumChunks := 2 
]
dtFeatureChunking[
   NodeCandidate %in% vcOrdinalFeatures, 
   MaxNumChunks := 3 
]


