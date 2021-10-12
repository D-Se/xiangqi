#' Analyze a xiangqi game with a UCCI-compliant engine
#'
#' @param moves chr vector of length n moves in lan format **h2he2**
#' @param engine path to UCCI engine to use for obtaining evaluations
#' @param multipv int how many principle variations should be returned
#'
analyze_game <- function(moves, engine, multipv = 1){
  old_loc <- Sys.getlocale(category = "LC_CTYPE")
  Sys.setlocale(category = "LC_CTYPE", locale = "chs")
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
    e <- bigchess::uci_go(e, depth = 20)
    t$ucilog <- new_uci_read(e)$temp
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
    e$temp <- "x"
    ll[[i]] <- t
  }
  bigchess::uci_quit(e)
  message("Done!")
  ll
}

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
  tibble::tibble(move = seq_along(ll),
         score = unlist(lapply(game, function(x) x$score), use.names = F)
         )
}

#df <- score_chart(ll)
#library(ggplot2)

#df$color = ifelse(df$score > 0, "red", "black")
#df$color <- as.factor(df$color)


#library(dplyr)
#df %>%
 # mutate(color = if_else(score > 0, "red", "black"),
#         color = as.factor(color)) %>%
#  ggplot(aes(x = move, y = score, color = color)) +
 # geom_path(aes(group = 1)) +
#  geom_point()


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


