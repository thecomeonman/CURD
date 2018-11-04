#' Tag clusters by depth
#' Iterate through depth levels
#' For a given depth level, find all the nodes of the tree that are either
#' nodes at that depth or leaves higher up the tree. Tag them as clusters
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
TagClustersByDepth = function(
   dtTreeSummary,
   dtDataset, 
   lTree
) {

   # Iterate through depth levels
   nMaxDepth = max(dtTreeSummary$Depth)
   
   for (depth in 2:nMaxDepth) {
         
      # Initialize a column with this feature name
      cFeatName = paste0('NodeLabelAtDepth.',depth)
      dtDataset[, eval(cFeatName) := ""]

      # Find the various node labels for nodes at this depth (or leaf 
      # nodes higher up)
      vNodesAtDepth = dtTreeSummary[
         (Depth == depth)  | 
         (
            (LeafNode == TRUE) & 
            (Depth <= depth)
         ),
      ]$NodeLabel

      # Iterate through the node labels
      for (cLabel in vNodesAtDepth) {

         # Find the rows where these node labels apply (anything that 
         # begins with this node label, essentially)
         vRows = dtDataset[
            substring(
               NodeLabel, 
               1, 
               nchar(cLabel)
            ) == cLabel, 
            which = TRUE
         ]

         # Tag those rows with this label -- that is now the cluster name
         dtDataset[
            vRows, 
            eval(cFeatName) := cLabel
         ]
      }
      # End iteration through node labels

   }
   # End iteration through depth levels

   return (dtDataset)
} 