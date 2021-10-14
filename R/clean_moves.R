#' Parse a move string in a machine-readable format.
#' @description UCCI engines require the move string to be clear of punctuation.
#'
#' @param moves chr vec of length 1
#' @return vector of length n where n is the number of moves in the game.
#' @export
clean_moves <- function(moves){
  moves <- unlist(
    strsplit(
      trimws(
        ### TODO: remove entire comment from pgn entry
        gsub(x = moves, pattern = "[0-9]+\\.|[*]?m", replacement = "", perl = TRUE)
      ), split = " ", fixed = T
    ), use.names = FALSE
  )
  # some files end in * to indicate resignation or include m for comments
  # in game analysis, NA is returned when checkmate is possible
  if (!is.na(moves) && stringi::stri_detect_regex(moves[length(moves)],
                                 pattern = "[[:punct:]]|=")) {
    moves[-length(moves)]
  } else {
    moves
  }
}
