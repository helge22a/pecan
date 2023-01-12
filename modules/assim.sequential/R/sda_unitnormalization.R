#' SDA unit normalization
#'
#' @param X matrix with state variables created in build_x
#' @param method FALSE = forward, TRUE = reverse
#'
#' @return
#' @export
#'
#' @examples
sda_unitnormalization <- function(X, Y = NULL, R = NULL, Pa = NULL, mu.a = NULL, method = FALSE){
  
  if(method){
    #reverse mode, after analysis step to revert unit normalization before states are updated
    
  }else{
    #forward mode, before analysis step to normalize unit range
    
  }
  
}