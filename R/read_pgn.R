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

#' Read dense PGN game fiels that have moves of multiple games in a single file.
#' @param filepath filepath to .PGN file
#'
#' @details
#' @return List of length n
#' TODO add parallel support ?
read_pgn_bulk <- function(path){
  game <- readLines(path)
  game <- stringi::stri_remove_empty(game)
  ll <- vector(mode = "list", length = length(game) / 14L)
  ll <- split(game, cut(seq_along(game), (length(game) / 14L), labels = FALSE))
  ll <- lapply(ll, function(x){
    ind <- which(stringi::stri_detect_regex(x,
                                            pattern = "^? \\d.",
                                            max_count = 1, negate = FALSE))
    l <- vector(mode = "list", length = 2L)
    names(l) <- c("Metadata", "Moves")
    tryCatch({ # might be some parsing errors in the metadata or move list
      l[[1]] <- x[1:(ind-1)]
      l[[2]] <- x[ind:length(x)] %>% clean_moves() %>% stringi::stri_remove_empty()
    }, error = function(e) e )
    l
  })
  ll
}

