#' Print method to show the current position as a matrix in the console
#' @param pos Default is the starting position, otherwise a named list of length 90.
#' @param style one of \code{c("international", "traditional", "UCCI")}
#'
#' @description
#' see \code{position_move} for detailed information on how to construct positions.
#' @return 10*9 matrix depicting a xiangqi position.
#' @export
print_position <- function(pos = "startpos", style = "pval"){
  if (identical(pos, "startpos")) {
    pos <- matrix(unlist(position_start_list, use.names = F),
           ncol = 9, dimnames = list(9:0, letters[1:9]))
  }
  ### TODO: make it print characters of desired notation system
  # style <- match.arg(style, choices = c("pval", "int", "trad"))
  # style <- switch(style,
  #                 "pval" = "",
  #                 "int" = "")
  # print_f <- rlang::expr(!!paste0("t_piece", style))
    matrix(unlist(pos, use.names = F),
           ncol = 9, dimnames = list(9:0, letters[1:9]))
}

