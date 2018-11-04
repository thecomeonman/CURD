#' Builds on TreeStretchedToBottom and adds additional details
#'
#' @param lTree Output of RecursivePartitioningTreeClustering
#' @param dtDataset The original dataset fed to the clustering algo
#' @param cClassLabelFeat The name of the column with the assignedclass in the 
#'   original dataset
#' @return
#'  NodeLabel ParentNodeIndex           NodeColumnName NodeColumnValue LeafNode Members LayerNbr Stretch NodePrediction Proportion
#'     Root>1               0            elsalvadoraid               n    FALSE     104        1   FALSE       democrat  1.0000000
#'   Root>1>1          Root>1 religiousgroupsinschools               n    FALSE      74        2   FALSE       democrat  0.8706897
#' Root>1>1>1        Root>1>1   aidtonicaraguancontras               n     TRUE       1        3   FALSE       democrat  0.8663793
#' Root>1>1>1        Root>1>1   aidtonicaraguancontras               n     TRUE       1        4    TRUE       democrat  0.8663793
TreeStretchedToBottomWithDetails = function(
   lTree = lTree,
   dtDataset = dtDataset,
   cClassLabelFeat     
) {

   # Getting tree in a tabular format
   dtNewClusteringObject = TreeStretchedToBottom(
      lTree = lTree,
      iParentNodeIndex = 0,
      bStretch = T
   )

   # Getting node assignment to each row at each level
   dtDatasetPredicted = AssignCluster(
      lTree,
      dtDataset
   )

   # getting quality of clustering
   dtClassClustering = QualityOfClustering(
      dtDatasetPredicted,
      cClassLabelFeat
   )

   dtNewClusteringObject = merge(
      dtNewClusteringObject,
      setnames(dtClassClustering[, list(
         NodePrediction = NodeLabel[which.max(LayerCumPct)],
         Proportion = max(LayerCumPct)
      ), 
      list(Node)
      ], 'Node','NodeLabel'),
      'NodeLabel'
   )

}