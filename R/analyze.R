#' Analyze a xiangqi game with a UCCI-compliant engine
#'
#' @param moves chr vector of length n moves in lan format **h2he2**
#' @param engine path to UCCI engine to use for obtaining evaluations
#' @param multipv int how many principle variations should be returned
#'
analyze_game <- function(moves, engine, multipv = 1){
  old_loc <- Sys.getlocale(category = "LC_CTYPE")
  Sys.setlocale(category = "LC_CTYPE", locale = "chs")
  if (is.character(engine)) e <- uci_engine(engine) else e <- engine
  tgt <- detect_input_type(moves)
  if (tgt == "san"){
    lan <- moves %>% clean_moves
    san <- moves %>% clean_moves %>%
      translate_notation %>%
      strsplit(" ") %>%
      `[[`(1)
  } else {
    lan <- moves %>%
      clean_moves %>%
      translate_notation("san") %>%
      stri_split_fixed(" ", simplify = T)
    san <- moves %>% clean_moves
  }
  ll <- vector(mode = "list", length = 1)
  for (i in 1:length(lan)) {
    t <- list()
    t$curr_move_lan <- lan[i]
    t$curr_move_san <- san[i]
    t$curpos_lan <- stringi::stri_c(lan[1:i], collapse = " ")
    t$curpos_san <- t$curpos_lan %>% clean_moves %>% translate_notation("lan")

    if (multipv > 1) {
      e <- uci_cmd(engine = e, command = paste("setoption", "name",
                                               "MultiPV", "value", multipv))
    }
    e <- uci_position(e, moves = t$curpos_lan)
    e <- uci_go(e, depth = 10)
    t$ucilog <- uci_read(e)$temp
    t$score <- new_uci_parse(t$ucilog, "score")
    if (i %% 2 == 0) t$score <- -t$score
    t$bestmove <- new_uci_parse(t$ucilog)
    t$bestline_lan <- new_uci_parse(t$ucilog, "bestline")
    l2s <- stri_c(t$curpos_lan, t$bestline_lan, sep = " ") %>%
      clean_moves %>%
      translate_notation("lan")

    t$bestline_san <- substr(l2s, nchar(t$curpos_san) + 2, nchar(l2s))
    t$bestmove_san <- substr(t$bestline_san, 1, 4)
    t$comment <- ""

    ll[[i]] <- t
  }
  uci_quit(e)
  message("Done!")
  ll
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
           stri_extract_first_regex(rslt, "(?<= pv )[a-z0-9 ]+")
         }, NA)
}
