#' Estimate your play strength based on move accuracy
#'
#' @section
#' Performance evaluation of xiangqi gameplay is hard to verify. Different from
#' chess, only a select set of ratings, both historical and contemporary, are known
#' for the top few players. Internationally, federations keep different rating lists
#' and rule sets for the game.
#'
#' This function attempts to estimate the players' performance compared to an
#' engine evaluation of game play, comprared to performance of known rating bands
#' of top top players.

estimate_rating <- function(){
  ### TODO: implement this metric >>
  # https://sci-hub.se/10.3233/icg-2012-35102
  x <- if (player == 1) {
    df %>%
      select(move, cpl) %>%
      dplyr::filter(!move %% 2 == 0) %>%
      dplyr::pull(cpl)
  } else { # black player
    df %>%
      select(move, cpl) %>%
      dplyr::filter(move %% 2 == 0) %>%
      dplyr::pull(cpl)
  }
  x <- x[!is.na(x)]
  u <- ifelse(x == 0, 100, 100-x)
  mean((u-min(u))/(max(u)-min(u)) * 100)
}
