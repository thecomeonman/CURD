#' AssignClusterCore needs a shell function around it for formatting, etc.
#'
#' @param lTree is an output of RecursivePartitioningTreeClustering
#' @param dtDataset a dataset to assign clusters to. Must have same or more features as lTree
#'
#' @return Returns the rest of the dataset along with node labels at each layer for each row
#'    NodeLabel Depth Layer1   Layer2     Layer3       Layer4         Layer5           Layer6
#' 1: Root>2>1>2>2>1>1     7 Root>2 Root>2>1 Root>2>1>2 Root>2>1>2>2 Root>2>1>2>2>1 Root>2>1>2>2>1>1
#' 2: Root>2>1>1>1>1>2     7 Root>2 Root>2>1 Root>2>1>1 Root>2>1>1>1 Root>2>1>1>1>1 Root>2>1>1>1>1>2
#' 3: Root>1>2>2>2>1>2     7 Root>1 Root>1>2 Root>1>2>2 Root>1>2>2>2 Root>1>2>2>2>1 Root>1>2>2>2>1>2
#' 4: Root>1>2>2>2>1>2     7 Root>1 Root>1>2 Root>1>2>2 Root>1>2>2>2 Root>1>2>2>2>1 Root>1>2>2>2>1>2
#' 5: Root>1>2>2>2>2>1     7 Root>1 Root>1>2 Root>1>2>2 Root>1>2>2>2 Root>1>2>2>2>2 Root>1>2>2>2>2>1
#' 6: Root>1>2>2>2>1>2     7 Root>1 Root>1>2 Root>1>2>2 Root>1>2>2>2 Root>1>2>2>2>1 Root>1>2>2>2>1>2
#'
#' @import data.table
#' 
#' @export
AssignCluster = function (
   lTree,
   dtDataset
) {

   # adds the UniqueSNO column needed to track each row uniquely
   dtTempDataset = copy(dtDataset)[, UniqueSNO := .I]

   # calls AssignClusterCore only if there are children to the root node
   if ( !is.null(lTree$lChildren) ) {

      dtDatasetFit = AssignClusterCore(
         lTree,
         dtTempDataset,
         iLayerNbr = 1
      )

      # formatting layer values such that it can be sorted in the right order
      # i.e. 10 will fall before 2, but 10 will fall after 02
      dtDatasetFit[, Layer :=
         paste0(
            'Layer',
            formatC(Layer, digits = max(Layer) %/% 10, flag = '0', format = "d")
         )
      ]

      # merge the assignment information back to the original dataset
      dtTempDataset = merge(
         dtTempDataset,
         as.data.table(dcast(
            dtDatasetFit,
            UniqueSNO ~ Layer,
            value.var = 'Node'
            # fun.aggregate = paste
         )),
         c('UniqueSNO'),
         all = T
      )

   }

   dtTempDataset[, UniqueSNO := NULL]


   # if a certain branch doesn't go below a certain layer then there will be
   # NAs for those layers. We can carry over the node from the last layer to 
   # all subsequent layers
   vcLayerColumns = sort(
      grep(
         colnames(dtTempDataset),
         pattern = 'Layer',
         value = T
      )
   )

   for ( i in seq(length(vcLayerColumns) - 1)) {

      dtTempDataset[
         is.na(get(vcLayerColumns[i+1])),
         c(vcLayerColumns[i+1]) := get(
            vcLayerColumns[i]
         )
      ]

   }


   return ( dtTempDataset )


}