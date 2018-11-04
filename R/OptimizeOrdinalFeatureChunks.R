#' Figure out how to divide ordinal features into chunks
#'
#' Numeric features can be coarse-classed into bins (e.g. based on quantile 
#' limits) so that they resemble ordinal features. However, in the interest 
#' of not having too many splits on a node, adjacent bins may be combined
#' in order to arrive at fewer splits. If an ordinal feature has $p$ bins
#' and the maximum number of splits one wishes to have is $q$,
#' then the number of distinct combinations to be tried out is
#' $\sum_{r=1}^{q}{C(p-1,r-1)}$ -- this number can be reduced
#' if one can use domain knowledge to rule out some possibilities.
#' The optimal split of an ordinal feature can be determined in the same way
#' that the best features for splitting a node is determined: classifier performance.
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
OptimizeOrdinalFeatureChunks <- function(
   mValConfMat, 
   nMinSplits = 2, 
   nMaxSplits = 3
) {

   nVars = nrow(mValConfMat)
   vClassLabels = rownames(mValConfMat)

   dtSplitAnal = data.table()
   nBestAvgClassErr = Inf

   # Iterate through the number of splits
   for (
      nSplits in min(
         nMinSplits, 
         length(vClassLabels)
      ):min(
         nMaxSplits, 
         length(vClassLabels)
      )
   ) {

      # For a given number of splits, enumerate the splits themselves
      dtSplitBoundaries = data.table()
      nNumSplits = factorial(nVars-1) / (
         factorial(nSplits-1) * factorial(nVars-nSplits)
      )

      while (
         nrow(dtSplitBoundaries) < nNumSplits
      ) {

         dtSplitBoundaries = rbind(
            dtSplitBoundaries, 
            data.table(
               t(
                  sort(
                     sample(
                        1:(nVars-1), 
                        nSplits-1, 
                        replace = F
                     )
                  )
               )
            )
         )

         setkeyv(
            dtSplitBoundaries, 
            colnames(dtSplitBoundaries)
         )
         
         dtSplitBoundaries = unique(dtSplitBoundaries)

      }

      #dtSplitBoundaries = dtSplitBoundaries + 1

      # For each split, recalculate the confusion matrix by merging the  
      # relevant classes
      for ( r in 1:nrow(dtSplitBoundaries) ) {

         # Based on the split boundaries, figure out the bin groups
         #print(dtSplitBoundaries[r,])
         lb = 0
         ub = 0
         dtVarGroups = data.table()

         for (c in 1:nSplits) {

            lb = ub + 1
            
            ub = ifelse(
               c == nSplits, 
               nVars, 
               as.numeric(
                  dtSplitBoundaries[
                     r, 
                     paste0('V',c), 
                     with = F
                  ]
               )
            )

            # cat(lb, ", ", ub, "")
            dtVarGroups = rbind(
               dtVarGroups, 
               data.table(
                  t(
                     c(
                        lb, 
                        ub
                     )
                  )
               )
            )

         }

         dtVarGroups[, V3 := paste0(V1, '-', V2)]

         # Recalculate the confusion matrix
         mValConfMatNew = matrix(
            0, 
            nSplits, 
            nSplits
         )

         for ( i in 1:nSplits ) {

            for ( j in 1:nSplits ) {

               mValConfMatNew[i,j] = sum(
                  sum(
                     mValConfMat[
                        dtVarGroups[i,]$V1:dtVarGroups[i,]$V2,
                        dtVarGroups[j,]$V1:dtVarGroups[j,]$V2
                     ]
                  )
               )

            }

         }

         # todo, do bodmas?
         # todo, why the t in sum( t(mValConfMatNew) )
         vValClassErr = 1 - diag(mValConfMatNew) / sum( t(mValConfMatNew) )
         
         nAvgClassErr = CalculatePerformanceFromConfusionMatrix(
            mValConfMatNew
         )

         dtSplitAnal = rbind(
            dtSplitAnal, 
            list(
               NumSplits = nSplits,
               Splits = paste0(dtVarGroups$V3, collapse=';'),
               ClassErr = paste0(vValClassErr, collapse=';'),
               AvgClassErr = nAvgClassErr
            ),
            use.names = T,
            fill = T
         )

         if ( nAvgClassErr < nBestAvgClassErr ) {

            nBestAvgClassErr = nAvgClassErr
            mBestValConfMat = mValConfMatNew
            dtBestVarGroups = dtVarGroups
            cBestSplit = paste0(dtVarGroups$V3, collapse=';')

         }
         
      }
      # End iteration through splits (for a given number of splits)

   }
   # End iteration through number of splits

   lVarGroups = list()

   for ( r in 1:nrow(dtBestVarGroups) ) {
      
      lVarGroups[[r]] = vClassLabels[dtBestVarGroups[r,]$V1:dtBestVarGroups[r,]$V2]

   }

   print(lVarGroups)

   lSplitAnal = list(
      nBestAvgClassErr = nBestAvgClassErr,
      mBestValConfMat = mBestValConfMat,
      lVarGroups = lVarGroups,
      dtSplitAnal = dtSplitAnal,
      cBestSplit = cBestSplit
   )

   return (lSplitAnal)

}