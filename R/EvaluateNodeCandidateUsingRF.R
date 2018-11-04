#' Evaluate a feature using random forest
#'
#' Classifier performance is measured as the average class error on OOB sample
#' Since RF uses bagging to pick a sample to build each tree, each data point
#' is unused in roughly 35% of the trees. So the OOB performance basically
#' takes each point and measures the prediction accuracy on the trees it 
#' wasn't used to build.
#' @param matLabeledDataForClassifier todo
#' @param vPredictorFeaturesForClassifier todo
#' @param cPredictedFeature todo
#'
#' @return todo
#'
#' @import data.table
#'
#' @export
EvaluateNodeCandidateUsingRF <- function(
   matLabeledDataForClassifier,
   vPredictorFeaturesForClassifier,
   cPredictedFeature
) {

   # Train an RF
   # todo, these are being assigned to the global space? Can't avoid that?
   matLabeledDataForClassifier <<- matLabeledDataForClassifier
   vPredictorFeaturesForClassifier <<- vPredictorFeaturesForClassifier
   cPredictedFeature <<- cPredictedFeature

   modRF = randomForest(
      x = matLabeledDataForClassifier[, vPredictorFeaturesForClassifier],
      y = matLabeledDataForClassifier[, cPredictedFeature],
      ntree = 300,
      confusion = T
   )

   vClassLabels = sort(
      unique(
         matLabeledDataForClassifier[, cPredictedFeature]
      )
   )

   dtBestVarGroups = data.table(
      matrix(
         rep(
            seq(
               1,
               length(vClassLabels)
            ), 
            2
         ),
         length(vClassLabels), 
         2
      )
   )

   lVarGroups = list()

   for (r in 1:nrow(dtBestVarGroups)) {

      lVarGroups[[r]] = vClassLabels[
         dtBestVarGroups[r,]$V1:dtBestVarGroups[r,]$V2
      ]

   }

   # Evaluate the feature's distinctness: Look at average class error for the 
   # predicted feature based on OOB performance
   lClassifierPerf = list()
   lClassifierPerf[['vClassLabels']] = vClassLabels
   lClassifierPerf[["modClassifier"]] = modRF
   lClassifierPerf[['mConfusion']] = modRF$confusion
   lClassifierPerf[['vValClassErr']] = modRF$confusion[, 'class.error']
   lClassifierPerf[['nAvgClassErr']] = CalculatePerformanceFromConfusionMatrix(
      modRF$confusion
   )
   lClassifierPerf[['vPredictorFeatures']] = vPredictorFeaturesForClassifier
   lClassifierPerf[['lVarGroups']] = lVarGroups

   return (lClassifierPerf)

}