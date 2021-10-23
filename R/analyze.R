#' Analyze a xiangqi game with a UCCI-compliant engine
#'
#' @description
#' Game analysis consists of three parts
#'
#' @section
#' Engine evaluation of opening positions at a relatively low search depth,
#' anything below 30, is unreliable. For this reason an extensive opening book
#' is used that has pre-calculated evaluation for many
#' @param moves chr vector of length n moves in lan format **h2he2**
#' @param engine path to UCCI engine to use for obtaining evaluations
#' @param multipv int how many principle variations should be returned
#'
#' @returns list of lists. Length n, n equal to number of moves, each move described by 11-element list
#'
#'
#' TODO logic pass - place items outside the loop <hashed fen string>
#' TODO logic pass - hash table size
#' TODO vectorize FEN maker
#' TODO convert "t" to an environment, no copy on modification, less memory use
#'
#' @export
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
  #e <- bigchess::uci_position(e, moves = "h2e2", startpos = TRUE)
  #e <- bigchess::uci_go(e, infinite = T, stoptime = 2)
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
    ### TODO: test minimum 15, else engine doesnt put out lines?!! fix.
    e <- bigchess::uci_go(e, depth = 15)
    # Reading the temp log empties the temp log, only 1 chance to read it.
    # Sometimes the engine goes to specified extremely fast, if there is for
    # example a forced checkmate sequence possible. In that situation an erroneous
    # output is recorded from the prompt, resulting in integer(0)
    #t$ucilog <- e$temp
    t$ucilog <- uci_read(e)$temp
    t$score <- uci_parse(t$ucilog, "score")
    if (rlang::is_empty(t$score)) t$score <- 0 # bandaid fix
    if (i %% 2 != 0) t$score <- t$score * -1
    t$bestmove <- uci_parse(t$ucilog)
    t$bestline_lan <- uci_parse(t$ucilog, "bestline")
    l2s <- stringi::stri_c(t$curpos_lan, t$bestline_lan, sep = " ") %>%
      clean_moves %>%
      translate(target = "lan")

    t$bestline_san <- substr(l2s, nchar(t$curpos_san) + 2, nchar(l2s))
    t$bestmove_san <- substr(t$bestline_san, 1, 4)
    t$comment <- ""
    t$fen = make_fen2(position_move(clean_moves(t$curpos_lan)), p = p, n = i)
    e$temp <- "x"
    pr(message = sprintf("Added %s", lan[i]))
    p <- -p
    ll[[i]] <- t
  }
  bigchess::uci_quit(e)
  message("Done!")
  ll
}

# Helpers -----------------------------------------------------------------

### TODO: create int detection
detect_input_type <- function(moves){
  if (length(moves) > 1) moves <- moves[1]
  if (stringi::stri_detect_regex(moves, "^[a-z]", max_count = 1)) "san" else "lan"
}

# uci_x functions have been adapted from the **bigchess** package to be
# applicable to engines that use UCCI protocol
uci_read <- function(engine){
  prs <- engine$pipe$read_output_lines()
  if (length(prs) > 1)
    engine$temp <- c(prs)
  return(engine)
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



