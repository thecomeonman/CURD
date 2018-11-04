#' Calculate performance metric from confusion matrix
#' Right now, we're just calculating average class error
#'
#' @param mConfusion
#' @param cMetric
#'
#' @import data.table
#'
#' @export
CalculatePerformanceFromConfusionMatrix <- function(
   mConfusion, 
   cMetric = 'AvgClassErr'
) {

   nClassErr = 0

   for (i in 1:nrow(mConfusion)) {

      if ( sum(mConfusion[i,1:nrow(mConfusion)]) > 0 ) {

         nClassErr = nClassErr + (
            1 - (
               mConfusion[i, i] / sum(
                  mConfusion[
                     i, 
                     1:nrow(mConfusion)
                  ]
               )
            )
         )

      }

   }

   # todo: name variable
   ToReturn1 = nClassErr / nrow(mConfusion)

   return ( ToReturn1 )

}