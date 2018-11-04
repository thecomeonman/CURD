# todo, make one function for evaludation and pass rpart / rf as an argument?
#' Evaluate a feature using classification tree models
#'
#' Classifier performance is measured as the average class error on OOB sample
#' This is done using 3-fold cross-validation, which is roughly the equivalent
#' of the OOB performance measured in RF
#'
#' @param matLabeledDataForClassifier todo
#' @param vPredictorFeaturesForClassifier todo
#' @param cPredictedFeature todo
#'
#' @return todo
#'
#' @import data.table
#'
#' @export
EvaluateNodeCandidateUsingRpart <- function(
   matLabeledDataForClassifier,
   vPredictorFeaturesForClassifier,
   cPredictedFeature
) {

   # Initialize values required for k-fold validation
   nFolds = 3
   nFoldSize = round(nrow(matLabeledDataForClassifier)/nFolds)
   lb = 0
   ub = 0
   vPermRows = sample(
      nrow(matLabeledDataForClassifier), 
      nrow(matLabeledDataForClassifier), 
      replace = F
   )

   # Initialize the confusion matrix
   vClassLabels = sort(
      unique(matLabeledDataForClassifier[, cPredictedFeature])
   )

   mValConfMat = matrix(
      0,
      nrow = length(vClassLabels),
      ncol = length(vClassLabels)
   )

   colnames(mValConfMat) = vClassLabels
   rownames(mValConfMat) = vClassLabels
   lClassifierModels = list()

   # Iterate through folds
   for ( fold in 1:nFolds ) {

      # Find the kth fold, which will be used for out-of-sample prediction, while the rest is used for training
      lb = ub + 1
      ub = ifelse(
         lb+nFoldSize <= nrow(matLabeledDataForClassifier), 
         lb+nFoldSize, 
         nrow(matLabeledDataForClassifier)
      )
      vOutSample = vPermRows[lb:ub]

      # Train the classification tree
      rpartObject = rpart(
         formula = as.formula(
            paste(
               cPredictedFeature, 
               '~ ', 
               paste0(
                  vPredictorFeaturesForClassifier, 
                  collapse = '+'
               )
            )
         ),
         data = matLabeledDataForClassifier[-vOutSample,]
      )

      # Predict values on the kth fold
      vRpartPrediction = as.character(
         predict(
            object = rpartObject,
            newdata = matLabeledDataForClassifier[vOutSample,],
            type = 'class'
         )
      )

      # get the actuals for the kth fold
      vRpartActual = as.character(
         unlist(
            matLabeledDataForClassifier[
               vOutSample,
               cPredictedFeature
            ]
         )
      )

      # Update the confusion matrix
      for (obs in vClassLabels) {
         for (pred in vClassLabels) {
            mValConfMat[obs, pred] = mValConfMat[obs, pred] +
               length(
                  which(
                     (vRpartActual == obs) & (vRpartPrediction == pred)
                  )
               )
         }
      }

      lClassifierModels[[fold]] = rpartObject

   }
   # End iteration through folds

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

   lClassifierPerf = list()
   lClassifierPerf[['vClalssLabels']] = vClassLabels
   lClassifierPerf[['modClassifier']] = lClassifierModels
   lClassifierPerf[['mConfusion']] = mValConfMat
   lClassifierPerf[['vValClassErr']] = CalculatePerformanceFromConfusionMatrix(
      mValConfMat
   )
   lClassifierPerf[['nAvgClassErr']] = mean(lClassifierPerf[['vValClassErr']])
   lClassifierPerf[['lVarGroups']] = lVarGroups

   return (lClassifierPerf)

}