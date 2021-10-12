#' Function to create operator that mimics an unary operator
#' Not meant to be called directly.
#'
#' @param name name of function to construct
#' @param body body of the function of operator to create
#'
#' @return seemingly unary infix function
`%.%` <- function(name, body) {
  `%paste%` <- paste0
  fun_name <- "%" %paste% name %paste% "%"
  assign(
    x = fun_name,
    value = function(x, comment) body(x),
    envir = parent.frame()
  )
}

`%analyze%` <- ">>" %.% function(x) {
  s <- {

  }
}
