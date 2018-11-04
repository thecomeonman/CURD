#' Classification accuracy at various levels
#' 
#' At each layer of the tree, calculates the proportion of rows correctly
#' assigned to a particular label. This is calculated across the effective tree
#' if you were to cut all the layers off below the specified layer, i.e. all 
#' leaf nodes upto that layer, and all non-leaf nodes in that layer. 
#' 
#' @param dtTreeSummary Output of TraverseTreeToMeasureClassificationQuality
#'
#' @return A summary of the predictions at a particular cut in the tree
#' 
#'    Depth NodeMajorityClass NumRows NodeMajorityPc
#' 1:     2          democrat     104      0.9519231
#' 2:     2        republican     128      0.8046875
#' 3:     2               All     232      0.8706897
#' 4:     3          democrat     104      0.9519231
#' 5:     3        republican     128      0.8046875
#'
#' @import data.table
#'
#' @export
GetClassificationAccuracyByClassDepth = function(
   dtTreeSummary
) {

   # initialise an empty data.table. 
   # We'll populate it layer by layer of the tree.
   dtClassificationAccuracyByClassDepth = data.table()

   # Looping through the layers
   for ( d in 2:max(dtTreeSummary$Depth) ) {

      # Counting all leaf nodes upto this layer
      # ------------------------------------------------------------------------

      # Remember that leaves higher up the tree ought to be counted as well
      dtLeaf = dtTreeSummary[
         (
            (LeafNode == TRUE) & 
            (Depth <= d)
         ),
      ]

      # Getting total rows matched correctly / total rows for all 
      # leaf nodes until this depth
      dtLeafSummaryByClass = dtLeaf[, 
         list(
            NumRows = sum(NumRows),
            NodeMajorityPc = sum(NumRows*NodeMajorityPc) / sum(NumRows),
            LeafNodes = TRUE,
            Depth = d
         ),
         list(NodeMajorityClass)
      ]

      dtLeafSummaryOverall = dtLeaf[, 
         list(
            NumRows = sum(NumRows),
            NodeMajorityPc = sum(NumRows*NodeMajorityPc)/sum(NumRows),
            LeafNodes = TRUE,
            NodeMajorityClass = "All",
            Depth = d
         )
      ]

      # Counting all non-leaf nodes at this layer since they will get
      # considered as leaf layers if you were to cut the tree here
      # ------------------------------------------------------------------------

      # Whereas, non-leaves are to be counted only if they are precisely at that 
      # depth
      dtNonLeaf = dtTreeSummary[
         (
            (LeafNode == FALSE) & (Depth == d)
         ),
      ]

      dtNonLeafSummaryByClass = dtNonLeaf[, 
         list(
            NumRows = sum(NumRows),
            NodeMajorityPc = sum(NumRows*NodeMajorityPc)/sum(NumRows),
            LeafNodes = FALSE,
            Depth = d
         ),
         list(NodeMajorityClass)
      ]

      dtNonLeafSummaryOverall = dtNonLeaf[, 
         list(
            NumRows = sum(NumRows),
            NodeMajorityPc = sum(NumRows*NodeMajorityPc)/sum(NumRows),
            LeafNodes = FALSE,
            NodeMajorityClass = "All",
            Depth = d
         )
      ]


      # putting leaf and non-leaf summary toether
      # ------------------------------------------------------------------------

      # Summarize data at that depth
      dtDepthSummary = data.table()
      if (nrow(dtLeaf) > 0) {

         dtDepthSummary = rbind(
            dtDepthSummary,
            rbind(
               dtLeafSummaryByClass,
               dtLeafSummaryOverall
            )
         )

      }

      if (nrow(dtNonLeaf) > 0) {

         dtDepthSummary = rbind(
            dtDepthSummary,
            rbind(
               dtNonLeafSummaryByClass,
               dtNonLeafSummaryOverall
            )
         )

      }

      dtDepthSummary = dtDepthSummary[, 
         list(NumRows = sum(NumRows),
         NodeMajorityPc = sum(NumRows*NodeMajorityPc)/sum(NumRows)),
         by = list(Depth, NodeMajorityClass)
      ]


      # Append this to the data about classification accuracy by depth
      dtClassificationAccuracyByClassDepth = rbind(
         dtClassificationAccuracyByClassDepth,
         dtDepthSummary
      )


   }

   dtClassificationAccuracyByClassDepth
   
}
