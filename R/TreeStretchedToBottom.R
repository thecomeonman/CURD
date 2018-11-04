#' Basically adds a Stretch column to the output of NewClusteringNodes
#' 
#' @param lTree is an output of RecursivePartitioningTreeClustering
#' @param iParentNodeIndex Keeps track of where in the tree the function is.
#' @param iLayerNbr Keeps track of where in the tree the function is. 
#' Should initiliase as 1
#' @param bStretch Setting it to T ensures that leaf nodes are mentioned in all
#' layers below the layer of the leaf node as well.
#'
#' @return
#'  NodeLabel ParentNodeIndex           NodeColumnName NodeColumnValue LeafNode Members LayerNbr Stretch
#'     Root>1               0            elsalvadoraid               n    FALSE     104        1   FALSE
#'   Root>1>1          Root>1 religiousgroupsinschools               n    FALSE      74        2   FALSE
#' Root>1>1>1        Root>1>1   aidtonicaraguancontras               n     TRUE       1        3   FALSE
#' Root>1>1>1        Root>1>1   aidtonicaraguancontras               n     TRUE       1        4    TRUE
#'
#' @import data.table
#'
#' @export
TreeStretchedToBottom = function(
   lTree,
   iParentNodeIndex = 0,
   iLayerNbr = 0,
   bStretch = T
) {

   # go look
   dtNewClusteringObject = NewClusteringNodes(
      lTree = lTree,
      iParentNodeIndex = iParentNodeIndex,
      iLayerNbr = iLayerNbr
   )

   # If the farthest leaf node ends at height h, then any other
   # leaf node that ends before height h will be just 'vertically'
   # stretched to height h so that it is easy to view as a tree
   if ( bStretch == T ) {

      # Getting the leaf nodes which don't end at height h
      dtStretch = dtNewClusteringObject[
         LeafNode == T &
            LayerNbr < max(LayerNbr)
      ]

      # if there are any such leaf nodes then do the stretching
      if ( nrow(dtStretch) > 0 ) {

         # Add the extra leaf nodes between the leaf node's height and h
         dtStretch = dtStretch[,
            list(
               LayerNbr = (LayerNbr+1):dtNewClusteringObject[, max(LayerNbr)]
            ),
            list(
               ParentNodeIndex,
               NodeLabel,
               NodeColumnName,
               NodeColumnValue,
               LeafNode,
               Members
            )
         ]

         # mark it out as having been stretched
         dtStretch[, Stretch := T]

         # add it back to the set of leaf nodes which didn't need stretching
         dtNewClusteringObject = rbind(
            dtNewClusteringObject[, Stretch := F],
            dtStretch
         )

         setDT(dtNewClusteringObject)


      # If there aren't any that need stretching then just say nothing needs stretching
      } else {

         dtNewClusteringObject[, Stretch := F]

      }
   }

   return (dtNewClusteringObject)

}
