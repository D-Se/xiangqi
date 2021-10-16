#' Create summary statistics of a game evaluation.
#'
#' @description
#' `describe()` makes it easy to get an overview of game progression at a glance.
#'
#' @param game a list of game evaluation. Minimal needed elements are score
#' @returns
#' `describe()` returns a list of length 3; a tibble of game data, a tibble of
#' performance metrics and a tibble of summary statistics.
#' @export
describe <- function(game){
  df <- tibble::tibble(move = seq_along(game),
                       score = unlist(lapply(game, function(x) x$score), use.names = F)
  ) %>%
    dplyr::mutate(score = dplyr::case_when(score < -3000 ~ -3000,
                                           score > 3000 ~ 3000,
                                           TRUE ~ score),
                  advantage = dplyr::if_else(score > 0, "red", "black"), # for graph plot colors
                  advantage = as.factor(advantage),
                  cpl = -(dplyr::lag(score) - score), # centipawn loss
                  abs_cpl = abs(cpl),
                  perf = dplyr::case_when(
                    abs_cpl > 300 ~ "blunder", # equal to value of 1 canon
                    abs_cpl > 100 & abs_cpl <= 300 ~ "Mistake",
                    abs_cpl > 50 & abs_cpl <= 100 ~ "Inaccuracy",
                    abs_cpl > 20 & abs_cpl <= 50 ~ "Good",
                    abs_cpl > 5 & abs_cpl <= 20 ~ "Excellent",
                    abs_cpl <= 5 ~ "Best"),
    )

  out <- vector(mode = "list", length = 3)
  out[[1]] <- df
  out[[2]] <- tibble::tibble("red" = accuracy(df, 1), "black" = accuracy(df, 2))
  out[[3]] <- dplyr::inner_join(x = move_summary(df, 1), y = move_summary(df, 2), by = "perf") %>%
    dplyr::rename(Type = perf, Red = n.x, Black = n.y) %>%
    dplyr::slice(1,3,4,5,6,2)
  names(out) <- c("stats", "accuracy", "counts")
  out
}

# Helpers -----------------------------------------------------------------

move_summary <- function(dq, player){
  player <- if (player == 1) rlang::expr(!move) else rlang::expr(move)
  dq %>%
    dplyr::filter(!!player %% 2 == 0) %>%
    dplyr::group_by(perf) %>%
    tidyr::drop_na() %>%
    dplyr::summarise(n = dplyr::n())
}

# input: score chart df of game. Ratio of mistakes per number of moves in game
# number of mistakes per number of moves played, chess.com CAPS
accuracy <- function(df, player){
  player <- if (player == 1) rlang::expr(!move) else rlang::expr(move)
  m <- df %>%
    dplyr::filter(!!player %% 2 == 0) %>%
    dplyr::filter(perf == "Mistake") %>%
    nrow
  t <- round(nrow(df) / 2, 0)
  100*(1-m/t)
}

# an implementation of chess.com's CAPS2-like 0-100 performance based on player
# rating.
performance <- function(df, player){
  player <- if (player == 1) rlang::expr(!move) else rlang::expr(move)
  x <- df %>%
    dplyr::filter(!!player %% 2 == 0) %>%
    dplyr::pull(cpl)
  x <- x[!is.na(x)]
}
