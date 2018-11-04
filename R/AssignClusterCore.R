#' Backend for AssignCluster
#' 
#' Recursive function that traverses the tree and assigns rows of a
#' dataset to the respective nodes. AssignCluster is the wrapper 
#' function for this
#' 
#' @param lTree is an output of RecursivePartitioningTreeClustering
#' @param dtDataset a dataset to assign clusters to. Must have same or more 
#' features as lTree
#' @param iLayerNbr Keeps track of where in the tree the function is. 
#' Should initiliase as 1
AssignClusterCore = function (
   lTree,
   dtDataset,
   iLayerNbr
) {

   # Finds the conditions in each of the child node and then continues
   # from there
   rbindlist(

      lapply(

         lTree$lChildren,

         function (lChild) {

            # subsetting data as per the condition in this child node
            dtDatasetNode = dtDataset[
               get(as.character(lChild$cParentSplitFeat)) %in%
                  lChild$vParentSplitVals
               ]

            # Tracking the UniqueSNO which fall in this subset...
            dtNode = dtDatasetNode[,
               list(
                  UniqueSNO,
                  Layer = iLayerNbr,
                  Node = lChild$cNodeLabel,
                  LeafNode = is.null(lChild$lChildren)
               )
            ]

            # ... and passing this subset for furter partitioning from the
            # lower layers
            if (!is.null(lChild$lChildren)) {

               as.data.table(

                  rbind(

                     dtNode,

                     AssignClusterCore(
                        lChild,
                        dtDatasetNode,
                        iLayerNbr = iLayerNbr + 1
                     )

                  )

               )

            } else {

               dtNode

            }

         }
      )

   )

}