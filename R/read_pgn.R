#' load a PGN file.
#'
#' @param path filepath.
#' @param keep boolean. Should metadata be kept?
#' @description
#' A convenience exists to read in multiple files, see \code{read_bulk}.
#'
#' @export
#' @return list of length 2, 1 element with metadata, one with moves.
### TODO: add keep utility
read_pgn <- function(path, keep = TRUE){
  tmcn::setchs()
  ll <- vector(mode = "list", length = 2) ; names(ll) <- c("Metadata", "Moves")
  game <- readLines(path)
  # start of move list
  ind <- which(stringi::stri_detect_regex(game,
                                          pattern = "^  \\d.",
                                          max_count = 1, negate = FALSE))
  ll[[1]] <- game[1:ind-1]
  ll[[2]] <- game[ind:length(game)] %>% clean_moves()
  ll
}

#' Read a dense PGN file with manygames in a single file
#'
#' @param path filepath
#'
#' @return list of length n games
read_pgn_bulk <- function(path){
  game <- readLines(path)
  ind <- which(stringi::stri_detect_regex(game,
                                          pattern = "^\\d.", negate = FALSE))
  ll <- vector(mode = "list", length = length(ind))
  ll[[1]] <- game[1:ind[1]]
  ll[[length(ll)]] <- game[ind[length(ind-1)]:length(game)]
  for (i in seq_along(ind[-c(1, length(ind))])) {
    ll[[i]] <- game[ind[1]+1:ind[2]]
  }
}

library(tidyverse)



rm(dt)

file.path(file.choose())

game <- readLines("C:\\Users\\D\\Documents\\R\\Xiangqi Cheat\\bulk-dpxq_san.pgn")

game <- stringi::stri_remove_empty(game)

read_pgn_bulk <- function(path){
  game <- readLines(path)
  x <- split(game, cut(seq_along(game), (length(game) / 14), labels = FALSE))
  lapply(x, )
}

y <- split(game, cut(seq_along(game), (length(game) / 14), labels = FALSE))
y[[1]]
y[[1]]
(length(game) / 14)
