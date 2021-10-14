#' Translate move list to other notation systems
#'
#' @param moves chr vec of n elements equal to n moves
#' @param target one of \code{c("san", "lan")}
#' @param pos list of length 90. See \code{position_move} to generate positions.
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' #' \tabular{lllll}{
#'  1\tab \strong{pawn} \tab 兵 卒 \tab P\cr
#'  2\tab \strong{canon} \tab 炮 \tab C\cr
#'  3\tab \strong{rook} \tab 车 \tab R\cr
#'  4\tab \strong{horse} \tab 马 \tab H\cr
#'  5\tab \strong{elephant} \tab 相 象 \tab E\cr
#'  5\tab \strong{advisor} \tab 仕 士 \tab A \cr
#'  5\tab \strong{general} \tab 帅 将 \tab K \cr
#' }
#'
#' @return chr vec of parsed notation, length 1.
#' @export
#' @examples
#' san <- c("炮二进二", "炮２进７", "炮八平二", "３进１")
#' lan <- c("h2h4", "b7b0", "b2h2", "f9e8")
#'
#' translate(lan)
#' translate(san, input_type = "san")
translate <- function(moves, target = "auto", pos = "startpos"){
  target <- match.arg(target, c("auto", "san", "lan", "xiangqi"))
  if (target == "auto") target <- tgt(moves[1])
  old_loc <- Sys.getlocale(category = "LC_CTYPE")
  Sys.setlocale(category = "LC_CTYPE", locale = "chs")
  if (identical(pos, "startpos")) pos <- STARTPOS
  p <- 1
  ll <- vector(mode = "list", length = length(moves))
  f <- rlang::expr(!!paste0("make_", target))
  tryCatch({
    for(i in seq_along(moves)){
      # lan (h2e2) to san (炮二平五) or reverse
      ll[[i]] <- rlang::eval_tidy(rlang::expr((!!f)(moves[i], pos, p)))
      p <- -p
      pos <- if (target == "lan") {
        position_move(moves[i], pos)
      } else {
        position_move(ll[[i]], pos)
      }
    }
  }, error = function(e) message(paste("Translation error in move: ", i, moves[i])))
  Sys.setlocale(category = "LC_CTYPE", old_loc)
  stringi::stri_c_list(ll, collapse =  " ")
}

tgt <- function(move){
  if (stringi::stri_detect_regex(substr(move, 1, 1), "^[a-z]", max_count = 1)) {
    "lan"
  } else{
    "san"
  }
}
