#' Feature selection for node split using RF/rpart models
#'
#' This function determines which feature to use to split a node in the tree.
#' The intuition behind it is, if a particular feature can be well-predicted 
#' using the other features then it means that there is a clear demarcation
#' of behaviour for various values of this feature. So the way we do it is to
#' consider various node candidates and see if they can be predicted well
#' as a function of the others. The one that gives the best predictive ability
#' is returned.
#'
#' Caveats:
#' 1. If there is only one feature left to use as a predictor, then RF cannot
#' be used. This is because the randomForest package does not consider this
#' degenerate case -- it makes no sense to build a forest of stumps, I guess?
#' So, in those cases, we use rpart with 3-fold CV.
#' 2. If a feature has a lot of categories, or for ordinal categories like 
#' APbin, we currently do not do any grouping. For massively categorical 
#' features, grouping makes sense. For ordinal features, grouping of adjacent
#' categories makes sense. This needs to be figured out -- how exactly can we
#' do this in an efficient manner? Would analysis of 'confused' groups of 
#' classes in the confusion matrix help?
#'
#' @param dtDataset todo
#' @param vRows todo
#' @param vNodeCandidates todo
#' @param vPredictorFeatures todo
#' @param vKeyFeatures todo
#' @param dtFeatureChunking todo
#' @param cClassifier todo
#' @param bUseOptimalChunkPerformance todo
#'
#' @return todo
#'
#' @import data.table
#'
#' @export

EvaluateFeaturesUsingClassifier = function(
   dtDataset,
   vRows,
   vNodeCandidates,
   vPredictorFeatures,
   vKeyFeatures,
   dtFeatureChunking,
   cClassifier,
   bUseOptimalChunkPerformance = TRUE
) {

   cat(
      length(vRows), 
      ' rows available to split at this node. Features to evaluate for split: ',
      paste0(
         vNodeCandidates, 
         collapse = ', '
      ), 
      '...'
   )

   # Create a dataset ready for RF application
   matLabeledDataForClassifier = as.data.frame(
      dtDataset[
         vRows,
         c(vKeyFeatures, vPredictorFeatures),
         with = F
      ]
   )

   matLabeledDataForClassifier = as.data.frame(
      as.matrix(
         matLabeledDataForClassifier
      ), 
      stringsAsFactors = T
   )


   # Initialize a list object to store the node evaluation results
   lNodeEval = list()
   vClassErrByFeat = rep(
      0, 
      length(vNodeCandidates)
   )
   names(vClassErrByFeat) = vNodeCandidates

   # Iterate through the features
   for (cPredictedFeature in vNodeCandidates) {

      # Predictors: Everything else
      vPredictorFeaturesForClassifier = setdiff(
         vPredictorFeatures, 
         cPredictedFeature
      )

      cat(
         'Evaluate: ',
         cPredictedFeature,
         ' = f(',
         paste0(
            vPredictorFeaturesForClassifier, 
            collapse = ', '
         ), 
         ')'
      )
      print('vPredictorFeaturesForClassifier')
      print(vPredictorFeaturesForClassifier)

      # Check if there are distinct values (otherwise, why use RF, and why split on that node?)
      vPredictedVals = sort(
         unique(
            matLabeledDataForClassifier[, cPredictedFeature]
         )
      )

      # Check if there is more than one predictor feature. Then run RF.
      # (Bug in ramdomForest package doesn't cover degenerate case)
      if ( length(vPredictorFeaturesForClassifier) > 1 ) {

         if ( cClassifier == "RF" ) {
            # Evaluate using RF
            lClassifierPerf = EvaluateNodeCandidateUsingRF(
               matLabeledDataForClassifier,
               vPredictorFeaturesForClassifier,
               cPredictedFeature
            )

         } else { 
            #Else, using Rpart

            lClassifierPerf = EvaluateNodeCandidateUsingRpart(
               matLabeledDataForClassifier,
               vPredictorFeaturesForClassifier,
               cPredictedFeature
            )

         }

         vClassErrByFeat[cPredictedFeature] = lClassifierPerf$nAvgClassErr

      } else {
         # Else: Use rpart (classification tree) with 3-fold cross-validation

         # Evaluate using Rpart
         lClassifierPerf = EvaluateNodeCandidateUsingRpart(
            matLabeledDataForClassifier,
            vPredictorFeaturesForClassifier,
            cPredictedFeature
         )

         vClassErrByFeat[cPredictedFeature] = lClassifierPerf$nAvgClassErr
      }
      # End condition block if there is only one feature to use for prediction

      print(lClassifierPerf$mConfusion)

      # Dealing with ordinal or large nominal categories is done using just the confusion matrix
      # This way, it is more efficient and one doesn't have to change the classifier call


      # If there is no value specified for MaxNumChunks, then set it to the number of unique values (no chunking)
      nMaxChunksFromParam = dtFeatureChunking[
         NodeCandidate == cPredictedFeature,
      ]$MaxNumChunks

      nChunksFromData = length(vPredictedVals)
      if (is.na(nMaxChunksFromParam)) {

         nMaxChunksFromParam = nChunksFromData

      }

      # If there is no value specified for MinNumChunks, then set it to the number of unique values (no chunking)
      nMinChunksFromParam = dtFeatureChunking[
         NodeCandidate == cPredictedFeature,
      ]$MinNumChunks

      nChunksFromData = length(vPredictedVals)
      if (is.na(nMinChunksFromParam)) {

         nMinChunksFromParam = nChunksFromData

      }

      # Code block to deal with ordinal features -- chunk it up
      if (
         (
            dtFeatureChunking[NodeCandidate == cPredictedFeature,]$FeatureType == "Ordinal"
         ) && (
            dtFeatureChunking[NodeCandidate == cPredictedFeature,]$Chunk == TRUE
         )
      ) {

         print(dtFeatureChunking)
         cat(
            "Optimizing split for ordinal feature: ",
            cPredictedFeature,
            ""
         )
         cat(nMinChunksFromParam)
         cat(nMaxChunksFromParam)

         # Function call to find optimal chunks
         lSplitAnal = OptimizeOrdinalFeatureChunks(
            mValConfMat = lClassifierPerf$mConfusion,
            nMinSplits = nMinChunksFromParam,
            nMaxSplits = nMaxChunksFromParam
         )

         # Assigning chunked groups and associated metrics back in lClassifierPerf
         lClassifierPerf$Confusion = lSplitAnal$mBestValConfMat
         lClassifierPerf$nAvgClassErr = lSplitAnal$nBestAvgClassErr
         lClassifierPerf$dtBestVarGroups = lSplitAnal$dtBestVarGroups
         lClassifierPerf$lVarGroups = lSplitAnal$lVarGroups

         # When deciding on whether to split by a particular feature,
         # should we consider the performance at the optimally chunked level or the overall level?
         if (bUseOptimalChunkPerformance == TRUE) {

            vClassErrByFeat[cPredictedFeature] = lSplitAnal$nBestAvgClassErr

         }

      } # End condition to chunk ordinal classes

      # Code block to deal with ordinal features -- chunk it up
      if (
         (
            dtFeatureChunking[NodeCandidate == cPredictedFeature,]$FeatureType == "Nominal"
         ) && (
            dtFeatureChunking[NodeCandidate == cPredictedFeature,]$Chunk == TRUE
         )
      ) {

         cat(
            "Optimizing split for nominal feature: ",
            cPredictedFeature,
            ""
         )

         # Function call to find optimal chunks
         lSplitAnal = OptimizeNominalFeatureChunks(
            mValConfMat = lClassifierPerf$mConfusion,
            nMinSplits = nMinChunksFromParam,
            nMaxSplits = nMaxChunksFromParam
         )

         # Assigning chunked groups and associated metrics back in lClassifierPerf
         lClassifierPerf$Confusion = lSplitAnal$mBestValConfMat
         lClassifierPerf$nAvgClassErr = lSplitAnal$nBestAvgClassErr
         lClassifierPerf$vBestClAlloc = lSplitAnal$vBestClAlloc
         lClassifierPerf$lVarGroups = lSplitAnal$lVarGroups

         # When deciding on whether to split by a particular feature,
         # should we consider the performance at the optimally chunked level or the overall level?
         if (bUseOptimalChunkPerformance == TRUE) {

            vClassErrByFeat[cPredictedFeature] = lSplitAnal$nBestAvgClassErr

         }

      } # End condition to chunk ordinal classes

      # Put the classifier performance summary into lNodeEval
      lNodeEval[[cPredictedFeature]] = lClassifierPerf

      # Add a similar function call and code block for large nominal groups

   } # End iteration through predictor candidates

   # Pick the most effective feature and return it (if tied, pick the first one; can change this later if reqd)
   lNodeEval[['vClassErrByFeat']] = vClassErrByFeat
   cBestFeat = names(
      which(
         vClassErrByFeat == min(vClassErrByFeat, na.rm = T)
      )
   )[1]

   lNodeEval[['cBestFeat']] = cBestFeat

   print(vClassErrByFeat)

   cat('Choose: ', lNodeEval$cBestFeat, ' to split')

   return (lNodeEval)

}