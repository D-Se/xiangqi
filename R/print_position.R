#' Print method to show the current position as a matrix in the console
#' @param pos Default is the starting position, otherwise a named list of length 90.
#' @param style one of \code{c("international", "traditional", "UCCI")}
#'
#' @description
#' see \code{position_move} for detailed information on how to construct positions.
#' @return 10*9 matrix depicting a xiangqi position.
#' @export
print_position <- function(pos = "startpos", style = "UCCI"){
  if (identical(pos, "startpos")) {
    pos <- matrix(unlist(position_start_list, use.names = F),
           ncol = 9, dimnames = list(9:0, letters[1:9]))
  }
  #style <- match.arg(style)
  #switch(style,
   #      "international" = {
    #       cn <-
    #    })

    # position is stored as a list for hashing purposes.
    matrix(unlist(pos, use.names = F),
           ncol = 9, dimnames = list(9:0, letters[1:9]))
}
