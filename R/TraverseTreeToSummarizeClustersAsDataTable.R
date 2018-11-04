#' Summarise tree as data.table with a row for each node
#' This function will traverse the tree to various depths and create a dataset
#' with a row for each node in the tree. This dataset will contain details of
#' how many points that node contains, whether or not it is a leaf node
#' and the rule that generates that node
#' Depth-first traversal of the tree happens
#'
#' @param dtTreeSummary todo
#' @param dtDataset todo
#' @param lTree todo
#'
#' @return todo
#'
#' @import data.table
#'
#' @export
TraverseTreeToSummarizeClustersAsDataTable = function(
   dtTreeSummary,
   dtDataset,
   lTree
) {

   # Summarize this node in the tree
   dtTreeSummary = rbind(
      dtTreeSummary,
      list(
         NodeLabel = lTree$cNodeLabel,
         NumRows = length(lTree$vRows),
         Depth = lTree$nDepth,
         LeafNode = ifelse(
            'lChildren' %in% names(lTree), 
            FALSE, 
            TRUE
         ),
         NodeRule = lTree$cClusteringRule
      ),
      use.names = T,
      fill = T
   )

   print(dtTreeSummary)

   # If this is not a leaf node, then traverse through the children
   if ( 'lChildren' %in% names(lTree) ) {
      for ( nChild in 1:length(lTree$lChildren) ) {

         dtTreeSummary = TraverseTreeToSummarizeClustersAsDataTable(
            dtTreeSummary = dtTreeSummary,
            dtDataset = dtDataset,
            lTree = lTree$lChildren[[nChild]]
         )
         
      }
   } 
   # Traverse through children

   return (dtTreeSummary)

}