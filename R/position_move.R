#' Create a xiangqi position
#' @param moves vector length n, each element with uci(lan) notation, no whitespaces*
#' @param pos by default the starting position, otherwise named list of length 90.
#'
#' @description
#'
#' @details
#'
#' The notation system used in this position function is the one described by
#' the Universal Chinese Chess Protocol **UCCI**, that which most xiangqi engines
#' use as the internal structure.
#' @export
#' @examples
#'position <- moves[1:49] %>% translate_notation("san") %>% clean_moves() %>% position_move()
position_move <- function(moves, pos = "start"){
  L <- if (!identical(pos, "start")) pos else position_start_list
  start <- substr(moves, 1,2)
  end <- substr(moves, 3, 4)
  for (i in seq_along(moves)) {
    L[[end[i]]] <- L[[start[i]]]
    L[[start[i]]] <- 0
  }
  L
}
