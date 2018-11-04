#' Draws the tree with labels, etc. 
#' @export 
DrawTree = function(
   dtNewClusteringObject
) {

   ggplot(dtNewClusteringObject[Stretch == F]) +
      geom_segment(aes(y = LayerNbr - 1, yend = LayerNbr, x = ParentNodeIndex, xend = NodeLabel))  +
      geom_point(aes(y = LayerNbr, x = NodeLabel, color = NodeColumnValue)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_text_repel(aes(y = LayerNbr, x = NodeLabel, label = paste(NodeColumnName,':',NodeColumnValue), color = NodeColumnValue)) +
      # geom_text_repel(aes(y = LayerNbr, x = NodeLabel, label = paste(NodeLabel,':',NodeColumnName,':',NodeColumnValue))) +
      scale_colour_discrete(guide = 'none') +
      labs(
         list(
            title = 'The tree'
         )
      )

}