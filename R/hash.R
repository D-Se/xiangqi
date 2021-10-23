#' # Procedure to make large lookup hash table
#'
#' #' Scrape xiangqi.com to obtain a move list of a game.
#' #'
#' #' @keywords internal
#'
#' # library(magrittr)
#' # library(parallel)
#' # library(stringi)
#' #
#' # ll <- readr::read_rds("C:\\Users\\D\\Documents\\R\\Xiangqi Cheat\\book\\book.rds")
#' #
#' # make_hash <- function(x){
#' #   x <- fst::hash_fst(x, block_hash = T)
#' #   paste0(sprintf("%x", x), collapse = "")
#' # }
#' #
#' #
#' # fen_strings <- names(ll)
#' # nested_names_list <- lapply(ll, names)
#' # KEYS <- mapply(function(x, i) paste0(i, x),
#' #                nested_names_list,
#' #                names(nested_names_list)) %>%
#' #   unlist(use.names = F)
#' # KEYS <- as.list(KEYS)
#' # KEYS <- lapply(KEYS, charToRaw)
#' #
#' # cl <- makeCluster(detectCores())
#' # clusterEvalQ(cl = cl, {
#' #   library(fst)
#' #   make_hash <- function(x){
#' #     x <- fst::hash_fst(x, block_hash = T)
#' #     paste0(sprintf("%x", x), collapse = "")
#' #   }
#' # })
#' # KEYS <- parLapply(cl = cl, X = KEYS, fun = make_hash)
#' # KEYS <- unlist(KEYS, use.names = F)
#' #
#' # nested_scores_list <- lapply(ll, function(x) unlist(x, use.names = F))
#' # VALUES <- unlist(nested_scores_list, use.names = F)
#' # VALUES[-c(197356, 197357, 197358, 197359, 197360)]
#'
#' # Making hash table -------------------------------------------------------
#'
#'
#' library(rlang)
#'
#' VALUES <- readr::read_rds("VALUES.rds")
#' KEYS <- readr::read_rds("KEYS.rds")
#'
#' OPENBOOK <- list2(!!!setNames(VALUES, KEYS))
#' e <- env()
#' env_bind_lazy(e, !!!OPENBOOK)
#'
#'
#' openbook <- OPENBOOK[1:2]
#' e <- new.env(hash = TRUE, parent = empty_env(), size = length(openbook)) # 66MB mem
#'
#' en %<~% openbook
#' e %<~% !!!openbook
#' e <- env()
#'
#' rm(e)
#' var <- "a"
#' var2 <- -100
#' env <- env(!!var := "A")
#' env$a
#'
#' ?env_bind_lazy(env, )
#'
#' env2 <- env()
#'
#'
#' quo <- local({
#'   who <- "fievel"
#'   quo(paste(who, "mouse"))
#' })
#' var <- list("asdasd", "a1234")
#'
#' keys <- list(KEYS)
#'
#' env_bind_lazy(e, !!!keys %<~% !!quo)
#' env$asdasd
#' env_unbind(env = env)
#'
#' en <- env()
#'
#'
#' env_bind_lazy(e, !!! objs)
#' rm(e)
#' e <- new.env(hash = TRUE, parent = empty_env(), size = length(OPENBOOK)) # 66MB mem
#' env_bind_lazy(e, !!!openbook)
#' e[["32e1458e526f70fb"]]
#'
#' en %<~% !!!openbook
#'
#' lobstr::obj_size(e)
#'
#' OPENBOOK <- list2(!!!setNames(VALUES, KEYS))
#' list2
