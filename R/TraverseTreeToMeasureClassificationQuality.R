#' FUNCTION: TEST PERFORMANCE OF CLUSTER TREE ON LABELED DATA
#' This function will traverse the tree to various depths and create a dataset
#' with a row for each node in the tree. This dataset will contain details of
#' how many points that node contains, whether or not it is a leaf node
#' and what the proportion of various classes is on that node.
#' Further, on the assumption that a node is assigned to a particular class, it
#' tells us how accurate each node is (based on the impurity level)
#' Depth-first traversal of the tree happens
#'
#' @param dtTreeSummary todo
#' @param dtDataset todo
#' @param lTree todo
#' @param cClassLabelFeat todo
#'
#' @return todo
#'
#' @import data.table
#'
#' @export  
TraverseTreeToMeasureClassificationQuality = function(
   dtTreeSummary,
   dtDataset,
   lTree,
   cClassLabelFeat
) {

   # Summarize this node in the tree
   cmix = table(
      dtDataset[
         lTree$vRows, 
         get(cClassLabelFeat)
      ]
   )
   
   vClassMix = as.numeric(cmix)
   names(vClassMix) = names(cmix)
   cNodeMajorityClass = names(vClassMix)[
      vClassMix == max(vClassMix)
   ][1]

   nNodeMajorityPc = max(vClassMix) / sum(vClassMix)

   dtTreeSummary = rbind(
      dtTreeSummary,
      list(
         NodeLabel = lTree$cNodeLabel,
         NumRows = length(lTree$vRows),
         Depth = lTree$nDepth,
         LeafNode = ifelse('lChildren' %in% names(lTree), FALSE, TRUE),
         NodeMajorityClass = cNodeMajorityClass,
         NodeMajorityPc = nNodeMajorityPc,
         NodeClasses = paste0(names(vClassMix), collapse = ';'),
         NodeSplit = paste0(vClassMix, collapse = ';')
      ),
      use.names = T,
      fill = T
   )


   # If this is not a leaf node, then traverse through the children
   if ( 'lChildren' %in% names(lTree) ) {

      for ( nChild in 1:length(lTree$lChildren) ) {
         dtTreeSummary = TraverseTreeToMeasureClassificationQuality(
            dtTreeSummary = dtTreeSummary,
            dtDataset = dtDataset,
            lTree = lTree$lChildren[[nChild]],
            cClassLabelFeat = cClassLabelFeat
         )
      }
   }
   # Traverse through children

   return (dtTreeSummary)

}