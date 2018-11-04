
#' End function to evaluate (using RF) predictor candidates on which node is 
#' to be split
#'
#' Recursive partitioning
#' This is the wrapper function that recursively partitions the dataset to form
#' a tree-based clustering solution.
#' The boundary conditions are:
#'   - Less than a prespecified number of points at a node
#'   - No more node candidates
#'   - No distinct feature values in any of the node candidates
#'   - Tree depth has reached a maximum
#' @param dtDataset
#' @param vRows
#' @param vNodeCandidates
#' @param vPredictorFeatures
#' @param vKeyFeatures
#' @param dtFeatureChunking
#' @param lTree
#' @param nTreeDepth
#' @param nMinRows
#' @param nMaxTreeDepth
#' @param cClassifier
#' @param bKeepFeatureInMixAfterSplit
#' @param bKeepFeatureInPredAfterSplit
#' @param bUseOptimalChunkPerformance
#'
#' @return todo
#'
#' @import data.table
#'
#' @export
RecursivePartitioningTreeClustering = function(
   dtDataset,
   vRows,
   vNodeCandidates,
   vPredictorFeatures,
   vKeyFeatures,
   dtFeatureChunking,
   lTree,
   nTreeDepth,
   nMinRows = 1,
   nMaxTreeDepth = Inf,
   cClassifier = "RF",
   bKeepFeatureInMixAfterSplit = FALSE,
   bKeepFeatureInPredAfterSplit = TRUE,
   bUseOptimalChunkPerformance = TRUE
) {

   # Initialize boundary condition to false, set to true if any of the 
   # following conditions is satisfied
   bBoundaryCondition = FALSE

   # Boundary condition: If there are too few elements
   bBoundaryCondition = bBoundaryCondition || 
      (length(vRows) <= nMinRows)

   # Boundary condition: If there are no more features to split on
   bBoundaryCondition = bBoundaryCondition ||
      (length(vNodeCandidates) == 0)

   # Boundary condition: If the tree has reached maximum allowable depth
   bBoundaryCondition = bBoundaryCondition ||
      (nTreeDepth == nMaxTreeDepth)

   # Boundary condition: If none of the node candidates has more than one 
   # distinct value (nothing more to split)
   bBoundaryCondition = bBoundaryCondition ||
      (nrow(dtDataset[vRows, .N, by=c(vNodeCandidates)]) == 1)

   # If none of the boundary conditions is satisfied, then continue to 
   # recursively partition the dataset
   if ( !bBoundaryCondition ) {
      #cat("Current rules: ", unique(dtDataset$ClusteringRule), "")

      # Remove any node candidates with only one distinct value in the remaining rows
      vRemCandidates = c()
      for (cFeat in vNodeCandidates) {
         if (nrow(dtDataset[vRows, .N, by=c(cFeat)]) == 1) {
            vRemCandidates = c(vRemCandidates, cFeat)
         }
      }
      cat("Removing node candidates with only one distinct value: ", vRemCandidates, "")

      vNodeCandidates = setdiff(vNodeCandidates, vRemCandidates)

      # Find the right feature to split on
      lNodeEval = EvaluateFeaturesUsingClassifier(
         dtDataset = dtDataset
         , vRows = vRows
         , vNodeCandidates = vNodeCandidates
         , vPredictorFeatures = vPredictorFeatures
         , vKeyFeatures = vKeyFeatures
         , dtFeatureChunking = dtFeatureChunking
         , cClassifier = cClassifier
         , bUseOptimalChunkPerformance = bUseOptimalChunkPerformance
      )

      cBestFeat = lNodeEval$cBestFeat

      lClassifierPerf = lNodeEval[[cBestFeat]]
      vClassLabels = lClassifierPerf$vClassLabels
      lVarGroups = lClassifierPerf$lVarGroups
      lTree[["cSplitFeat"]] = cBestFeat
      lChildren = list()

      # Iterate through the splits and make recursive calls to this function itself
      nSplitNum = length(lVarGroups)
      nSplitInd = 0
      for (nSplitInd in 1:nSplitNum) {
         # Rows pertaining to this split
         vSplitCategories = lVarGroups[[nSplitInd]]
         vSplitRows = intersect(dtDataset[get(cBestFeat) %in% vSplitCategories, which = T], vRows)

         # The rule defining this split
         dtDataset[vSplitRows, ParentRule := ClusteringRule]
         dtDataset[vSplitRows, ClusteringRule := ifelse(ClusteringRule == "Root",
                                                         paste0('IF [', cBestFeat, ' IN {'
                                                               , paste0(vSplitCategories, collapse=' OR '), '}]'),
                                                         paste0(ClusteringRule, ' AND [', cBestFeat, ' IN {'
                                                               , paste0(vSplitCategories, collapse=' OR '), '}]'))]
         dtDataset[vSplitRows, NodeLabel := paste0(NodeLabel, ">", nSplitInd)]
         dtDataset[vSplitRows, Depth := Depth + 1]

         #print(unique(dtDataset$ClusteringRule))
         cat("Further partitioning rows where ", cBestFeat, " in {"
            , paste0(vSplitCategories, collapse=' OR '), "}...")

         # Create the child node
         lChildNode = list(cNodeLabel = unique(dtDataset[vSplitRows,]$NodeLabel)
                           , cClusteringRule = unique(dtDataset[vSplitRows,]$ClusteringRule)
                           , nDepth = unique(dtDataset[vSplitRows,]$Depth)
                           , vRows = vSplitRows
                           , cParentSplitFeat = cBestFeat
                           , vParentSplitVals = vSplitCategories
                           , cSplitFeat = "")

         # Should we keep the feature used for splitting in the mix still?
         # This is applicable when chunking happens, and further splits are still an option
         if (bKeepFeatureInMixAfterSplit == TRUE) {
            vNodeCandidatesInChild = vNodeCandidates
         } else {
            vNodeCandidatesInChild = setdiff(vNodeCandidates, cBestFeat)
         }

         # Should we keep the feature used for splitting in the predictor list still?
         if (bKeepFeatureInPredAfterSplit == TRUE) {
            vPredictorFeaturesInChild = vPredictorFeatures
         } else {
            vPredictorFeaturesInChild = setdiff(vPredictorFeatures, cBestFeat)
         }
         cat("Node candidates in child: ", vNodeCandidatesInChild, "")

         #rm(lNodeEval)
         #gc()

         # Recursive function call
         lChildNode = RecursivePartitioningTreeClustering(
            dtDataset = dtDataset
            , vRows = vSplitRows
            , vNodeCandidates = vNodeCandidatesInChild
            , vPredictorFeatures = vPredictorFeaturesInChild
            , vKeyFeatures = vKeyFeatures
            , dtFeatureChunking = dtFeatureChunking
            , lTree = lChildNode
            , nTreeDepth = nTreeDepth + 1
            , nMinRows = nMinRows
            , nMaxTreeDepth = nMaxTreeDepth
            , cClassifier = cClassifier
            , bKeepFeatureInMixAfterSplit = bKeepFeatureInMixAfterSplit
            , bKeepFeatureInPredAfterSplit = bKeepFeatureInPredAfterSplit
            , bUseOptimalChunkPerformance = bUseOptimalChunkPerformance
         )

         # Add the child node to the list of children
         lChildren[[paste0(cBestFeat, ">>", nSplitInd)]] = lChildNode
      } # End iteration through feature values for the node split

      lTree[['lChildren']] = lChildren

   } # End block where boundary condition is not met

   # Return the tree
   return (lTree)

}