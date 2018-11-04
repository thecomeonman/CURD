#' Measures quality of clustering at various levels
#' @param dtDatasetPredicted, a result of AssignCluster
#' @return   
#'         Class     Node   N  Layer       PctOfNode  NodeLabel Correct     LayerCumPct
#' 1: republican   Root>1   5 Layer1 0.04807692   democrat   FALSE 0.021551724
#' 2:   democrat   Root>2  25 Layer1 0.19531250 republican   FALSE 0.129310345
#' 3: republican   Root>2 103 Layer1 0.80468750 republican    TRUE 0.573275862
#' 4:   democrat   Root>1  99 Layer1 0.95192308   democrat    TRUE 1.000000000
#' 5: republican Root>1>2   1 Layer2 0.02439024   democrat   FALSE 0.004310345
#' 6:   democrat Root>2>2   5 Layer2 0.04629630 republican   FALSE 0.025862069
#'
#' @import data.table
#'
#' @export
QualityOfClustering = function (
   dtDatasetPredicted,
   cClassLabelFeat
) {


   # Getting the column names where each the node for each layer is there
   vcLayerColumns = grep(
      colnames(dtDatasetPredicted),
      pattern = 'Layer',
      value = T
   )

   # Preparing count of each label at each node, layer level
   dtClassClustering = rbindlist(
      lapply(
         vcLayerColumns,
         function(cLayerColumn) {

            dtPrediction = dtDatasetPredicted[,
               list(Count = .N),
               c(cClassLabelFeat,cLayerColumn)
            ]

            setnames(
               dtPrediction,
               cLayerColumn,
               'Node'
            )

            dtPrediction[, Layer := cLayerColumn]

            dtPrediction



         }
      )
   )

   # Calculating proportion of contribution at each node, layer level
   dtClassClustering[,
      PctOfNode := Count / sum(Count),
      list(Node,Layer)
   ]

   # Assigning node label as the highest proportion labael in that node
   setkey(dtClassClustering, Layer, Node)
   dtClassClustering[, NodeLabel := get(cClassLabelFeat)[which.max(PctOfNode)], list(Layer, Node)]

   # calculating total accuracy at a layer level
   dtClassClustering[, Correct := get(cClassLabelFeat) == NodeLabel]
   setkey(dtClassClustering, Layer, Correct, PctOfNode)
   dtClassClustering[,
      LayerCumPct := cumsum(Count) / sum(Count),
      list(Layer)
   ]

   return ( dtClassClustering )

}