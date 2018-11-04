#' Function to return details of each node in a tabular format. 
#' TreeStretchedToBottom is a wrapper to this function
#' @param lTree is an output of RecursivePartitioningTreeClustering
#' @param iParentNodeIndex = 0 is used to trace the recursive calls. When user is calling 
#' the function, he/she needn't declare a value for this.
#' @param iLayerNbr = 0 is used to trace the recursive calls. When user is calling 
#' the function, he/she needn't declare a value for this.
#' @param value
#' (The members column is wrong in this example. It will actually be members in that node.)
#'    ParentNodeIndex    NodeLabel         NodeColumnName NodeColumnValue LeafNode Members LayerNbr
#' 1:               0       Root>1          elsalvadoraid               n    FALSE     232        1
#' 2:          Root>1     Root>1>1     handicappedinfants               n    FALSE     232        2
#' 3:        Root>1>1   Root>1>1>1 aidtonicaraguancontras               n     TRUE     232        3
#' 4:        Root>1>1   Root>1>1>2 aidtonicaraguancontras               y    FALSE     232        3
#' 5:      Root>1>1>2 Root>1>1>2>1              mxmissile               n     TRUE     232        4
#' 6:      Root>1>1>2 Root>1>1>2>2              mxmissile               y    FALSE     232        4
NewClusteringNodes = function(
   lTree,
   iParentNodeIndex = 0,
   iLayerNbr = 0
) {

   # if this node has childern then return details about the child
   # this function wil start from the root node, which doesn't really need
   # its details to be returned, so each call to this function is actually
   # going to return details about the children and not the later itself
   if ( ! is.null(lTree$lChildren) ) {

      # will recursively call the same function for each layer
      # and brind them into a data.table
      rbindlist(
         lapply(
            lTree$lChildren,
            function(lChild) {

               # various details about the child node
               dtThisNode = data.table(
                  ParentNodeIndex = iParentNodeIndex,
                  NodeLabel = lChild$cNodeLabel,
                  NodeColumnName = lChild$cParentSplitFeat,
                  NodeColumnValue = paste(lChild$vParentSplitVals, collapse = ';'),
                  LeafNode = is.null(lChild$lChildren),
                  Members = length(lChild$vRows),
                  LayerNbr = iLayerNbr + 1
               )

               # recursive call to the child
               as.data.table(
                  rbind(
                     dtThisNode,
                     NewClusteringNodes(
                        lTree = lChild,
                        iParentNodeIndex = lChild$cNodeLabel,
                        iLayerNbr = iLayerNbr + 1
                     )
                  )
               )

            }
         )
      )

   # if it is a leaf node then no children so nothing to return.
   } else {

      return (data.table())

   }

}