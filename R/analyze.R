#' Analyze a xiangqi game with a UCCI-compliant engine
#'
#' @param moves chr vector of length n moves in lan format **h2he2**
#' @param engine path to UCCI engine to use for obtaining evaluations
#' @param multipv int how many principle variations should be returned
#'
analyze_game <- function(moves, engine, multipv = 1){
  old_loc <- Sys.getlocale(category = "LC_CTYPE")
  Sys.setlocale(category = "LC_CTYPE", locale = "chs")
  message("Initializing engine")
  if (is.character(engine)) e <- bigchess::uci_engine(engine) else e <- engine
  tgt <- detect_input_type(moves)
  if (tgt == "san"){
    lan <- moves %>% clean_moves
    san <- moves %>% clean_moves %>% translate("lan") %>% clean_moves
  } else {
    lan <- moves %>%
      clean_moves %>%
      translate("san")
    san <- moves %>% clean_moves
  }
  ll <- vector(mode = "list", length = 1)
  if (requireNamespace("progressr", quietly = TRUE)) {
    pr <- progressr::progressor(along = lan)
    progressr::handlers(global = T)
  }
  message("Analyzing moves")
  p <- 1
  for (i in seq_along(lan)) {
    t <- list()
    t$curr_move_lan <- lan[i]
    t$curr_move_san <- san[i]
    t$curpos_lan <- stringi::stri_c(lan[1:i], collapse = " ")
    t$curpos_san <- t$curpos_lan %>% clean_moves %>% translate("lan")
    #e <- bigchess::uci_ucinewgame(e)
    if (multipv > 1) {
      e <- bigchess::uci_cmd(engine = e, command = paste("setoption", "name",
                                               "MultiPV", "value", multipv))
    }
    e <- bigchess::uci_position(e, moves = t$curpos_lan, startpos = TRUE)
    e <- bigchess::uci_go(e, depth = 10)
    t$ucilog <- uci_read(e)$temp
    #invisible(new_uci_read(e))
    t$score <- uci_parse(t$ucilog, "score")
    if (i %% 2 != 0) t$score <- t$score * -1
    t$bestmove <- uci_parse(t$ucilog)
    t$bestline_lan <- uci_parse(t$ucilog, "bestline")
    l2s <- stringi::stri_c(t$curpos_lan, t$bestline_lan, sep = " ") %>%
      clean_moves %>%
      translate("lan")

    t$bestline_san <- substr(l2s, nchar(t$curpos_san) + 2, nchar(l2s))
    t$bestmove_san <- substr(t$bestline_san, 1, 4)
    t$comment <- ""
    t$fen = make_fen(position_move(clean_moves(t$curpos_lan)), p = p, n = i)
    e$temp <- "x"
    pr(message = sprintf("Added %s", lan[i]))
    p <- -p
    ll[[i]] <- t
  }
  bigchess::uci_quit(e)
  message("Done!")
  ll
}

#games <- read_pgn_bulk("C:\\Users\\D\\Documents\\R\\Xiangqi Cheat\\bulk-dpxq_san.pgn")
#test <- analyze_game(games[[1]]$Moves, engine = eng, 1)

uci_read <- function(engine){
  prs <- engine$pipe$read_output_lines()
  if (length(prs) > 1)
    engine$temp <- c(prs)
  return(engine)
}
detect_input_type <- function(moves){
  if (length(moves) > 1) moves <- moves[1]
  if (stringi::stri_detect_regex(moves, "^[a-z]", max_count = 1)) "san" else "lan"
}


uci_parse <- function(ucilog, filter = "bestmove"){
  ind <- which(stringi::stri_detect_regex(ucilog, "^bestmove", max_count = 1))
  switch(filter,
         "bestmove" = {
           rslt <- ucilog[ind]
           substr(rslt, 10, 13)
         },
         "score" = {
           rslt <- ucilog[stringi::stri_detect_regex(ucilog, "^info")]
           rslt <- stringi::stri_match_first_regex(rslt[length(rslt)],
                                          pattern = "score\\s*(.*?)\\s*nodes")[,2]
           as.integer(rslt)
         },
         "bestline" = {
           rslt <- ucilog[ind-1]
           stringi::stri_extract_first_regex(rslt, "(?<= pv )[a-z0-9 ]+")
         }, NA)
}

score_chart <- function(game){
    tibble::tibble(move = seq_along(game),
                   score = unlist(lapply(game, function(x) x$score), use.names = F)
    ) %>%
    dplyr::mutate(advantage = dplyr::if_else(score > 0, "red", "black"), # for graph plot colors
                  advantage = as.factor(advantage),
                  cpl = -(dplyr::lag(score) - score), # centipawn loss
                  abs_cpl = abs(cpl),
                  perf = dplyr::case_when(
                    abs_cpl > 300 ~ "blunder", # equal to value of 1 canon
                    abs_cpl > 100 & abs_cpl <= 300 ~ "mistake",
                    abs_cpl > 50 & abs_cpl <= 100 ~ "inaccuracy",
                    abs_cpl > 20 & abs_cpl <= 50 ~ "good",
                    abs_cpl > 5 & abs_cpl <= 20 ~ "excellent",
                    abs_cpl <= 5 ~ "best"))
}

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

move_summary <- function(dq, player){
  player <- if (player == 1) rlang::expr(!move) else rlang::expr(move)
  dq %>%
    dplyr::filter(!!player %% 2 == 0) %>%
    dplyr::group_by(perf) %>%
    tidyr::drop_na() %>%
    dplyr::summarise(n = dplyr::n())
}

# input: score chart df of game. Ratio of mistakes per number of moves in game
accuracy <- function(df, player){
  player <- if (player == 1) rlang::expr(!move) else rlang::expr(move)
  m <- df %>%
    dplyr::filter(!!player %% 2 == 0) %>%
    dplyr::filter(perf == "Mistake") %>%
    nrow
  t <- round(nrow(df) / 2, 0)
  100*(1-m/t)
}

performance <- function(df, player){
  player <- if (player == 1) rlang::expr(!move) else rlang::expr(move)
  x <- df %>%
    dplyr::filter(!!player %% 2 == 0) %>%
    dplyr::pull(cpl)
  x <- x[!is.na(x)]
}

estimate_rating <- function(){
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

# input from describe
visualize <- function(ll){
  df <- ll[[1]]
  stages <- c("Middlegame" = 15, "Endgame" = 40)
  cols <- c("black" = "black", "red" = "red")
  tbl_ind <-
  if (abs(max(df$score, na.rm = T)) < abs(min(df$score, na.rm = T))) {
    -min(df$score, na.rm = T)-500
  } else {
    max(df$score, na.rm = T)
  }
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = move, y = score, color = advantage)) +
    ggplot2::geom_path(ggplot2::aes(group = 1), size = 1, linejoin = "round") +
    ggplot2::geom_hline(yintercept = 0, color = "black", size = 1, alpha = .5) +
    sapply(stages, function(xint) ggplot2::geom_vline(xintercept = stages)) +
    ggplot2::annotate("text", x = 15, y = 500, label = "opening", hjust = 1.1) +
    ggplot2::annotate("text", x = 40, y = 500, label = "endgame", hjust = 1.1) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                limits = c(1,NA),
                                breaks = c(seq(from = 0, to = nrow(df), by = 5))) +
    ggplot2::scale_y_continuous(limits = symmetric_limits) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    #ggpp::geom_table(x = 1, y = tbl_ind, label = list(ll[[3]]))
    ggplot2::annotate(geom = "table",
                      x = 1,
                      y = tbl_ind, label = list(ll[[3]]))
    # ggplot2::annotate(geom = "text", x = 1, y = -500, label = list(ll[[2]]))
}
#scale_y_continuous(trans = ggallin::pseudolog10_trans)

symmetric_limits <- function(x) {
  max <- max(abs(x))
  c(-max, max)
}

plot_board <- function(){
  library(png)
  library(grid)
  file.path(file.choose())
  img <- readPNG("C:\\Users\\D\\Desktop\\Board.png")
  img <- readPNG(system.file("img", "Rlogo.png", package="png"))
  g <- rasterGrob(img, interpolate=TRUE)

  qplot(1:10, 1:10, geom="blank") +
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    theme_void()
}
# file.path(file.choose())
# library(png)
# pnh <- readPNG("F:\\Video\\Pictures and logos\\Recording Images\\Chess logo.png")

# game <- analyze_game(games[[1]]$Moves, engine = eng, 1)
# game %>%
#   describe() %>%
#   visualize()
# analyze_game(games[[1]]$Moves, engine = eng, 1) %>%
#   describe() %>%
#   visualize()
