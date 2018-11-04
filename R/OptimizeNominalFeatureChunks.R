#' Figure out how to divide nominal features into chunks
#'
#' The higher the number of categories, the more splits on a node --
#' this not only results in too many segments (which not generally not a 
#' desirable characteristic), but also possibly in cases where some segments
#' have too little data to proceed. The ideal approach, therefore, might be
#' to arrive at groups of categories wherein the data points whose category
#' values for the given feature fall in the same group are relatively homogenous.
#'
#' A simple heuristic to determine this grouping can be based on the confusion
#' matrix of a classifier built to predict the given categorical feature. 
#' Let $C_{p \times p}$ be the confusion matrix for a predictive model for a
#' feature with $p$ category values, where $C_{i,j}$ represents the proportion
#' of instances in the validation sample where a data point in category $i$
#' has been classified as being in category $j$. Let 
#' $\tilde{C} = 1 - (C + C^T)/2$ (where $(.)^T$ is the transpose operator) be
#' a symmetric matrix that represents the pairwise distance between category
#' values. The intuition here is that, if the classifier cannot distinguish
#' between categories $i$ and $j$, then they can be combined -- the pairwise
#' distance matrix $\tilde{C}$ represents how close/far the category values are
#' in this respect. A hierarchical clustering algorithm applied on the 
#' category values based on the distances in $\tilde{C}$ would yield a 
#' grouping that we can use to split the node. The grouping can first be 
#' applied to the confusion matrix $C$ so that its order is now the same as
#' the number of groups, and this can then be used to evaluate the goodness
#' of the split.
#'
#' @param mValConfMat todo
#' @param nMinSplits todo
#' @param nMaxSplits todo
#'
#' @return todo
#'
#' @import data.table
#'
#' @export
OptimizeNominalFeatureChunks <- function(
   mValConfMat, 
   nMinSplits = 2, 
   nMaxSplits = 3
) {

   # Initialize
   vClassLabels = rownames(mValConfMat)
   nBestAvgClassErr = Inf
   dtSplitAnal = data.table()
   mValConfMat = mValConfMat[
      vClassLabels, 
      vClassLabels
   ]

   # Iterate through possible split sizes
   for ( nSplits in 
      min(
         nMinSplits, 
         length(vClassLabels)
      ):min(
         nMaxSplits, length(vClassLabels)
      )

   ) {

      # Distance metric and hierarchical clustering
      mDist = 1 -(mValConfMat + t(mValConfMat)) / (2 * sum(sum(mValConfMat)))
      dDist = dist(mDist)
      mClModel = hclust(dDist)
      #plot(mClModel)
      vClAlloc = cutree(
         mClModel, 
         k = nSplits
      )

      # Recalculate the confusion matrix
      mValConfMatNew = matrix(
         0, 
         nSplits, 
         nSplits
      )

      cSplits = rep("", times = nSplits)
      
      for ( i in 1:nSplits ) {

         cSplits[i] = paste0(
            "[", 
            paste0(
               which(vClAlloc == i), 
               collapse = ","
            ), 
            "]"
         )

         for (j in 1:nSplits) {
            #cat("Row", i, ": [", paste0(names(vClAlloc)[vClAlloc==i], collapse = ";"), "]")
            #cat("Column", j, ": [", paste0(names(vClAlloc)[vClAlloc==j], collapse = ";"), "]")

            mValConfMatNew[i,j] = sum(
               sum(
                  mValConfMat[
                     which(vClAlloc==i), 
                     which(vClAlloc==j)
                  ]
               )
            )

         }

      }

      # Evaluate the split
      vValClassErr = 1 - diag(mValConfMatNew)/sum(t(mValConfMatNew))
      nAvgClassErr = CalculatePerformanceFromConfusionMatrix(
         mValConfMatNew
      )

      dtSplitAnal = rbind(
         dtSplitAnal, 
         list(
            NumSplits = nSplits,
            ClAlloc = paste0(vClAlloc, collapse=";"),
            Splits = paste0(cSplits, collapse=';'),
            ClassErr = paste0(vValClassErr, collapse=';'),
            AvgClassErr = nAvgClassErr
         ),
         use.names = T,
         fill = T
      )

      if ( nAvgClassErr < nBestAvgClassErr ) {

         nBestAvgClassErr = nAvgClassErr
         mBestValConfMat = mValConfMatNew
         vBestClAlloc = vClAlloc

      }

   }
   # End iteration through splits

   # Get the chunking list ready to send back
   lVarGroups = list()
   for (r in 1:length(unique(vBestClAlloc)) ) {

      lVarGroups[[r]] = vClassLabels[vBestClAlloc == r]

   }

   lSplitAnal = list(
      nBestAvgClassErr = nBestAvgClassErr,
      mBestValConfMat = mBestValConfMat,
      lVarGroups = lVarGroups,
      dtSplitAnal = dtSplitAnal,
      vBestClAlloc = vBestClAlloc
   )

   return (lSplitAnal)

}