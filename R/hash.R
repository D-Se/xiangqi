# Procedure to make large lookup hash table from bookdata_cmprs.rds


#' @param keys character vector of opening book names.
#' @param values values to attach to keys
make_book <- function(keys, values){
  ### TODO: optimize this.
  # VALUES <- readr::read_rds("VALUES.rds")
  # KEYS <- readr::read_rds("KEYS.rds")
  # OPENBOOK <- list2(!!!setNames(VALUES, KEYS))
  # e <- env()
  # env_bind_lazy(e, !!!OPENBOOK)
  # e <- new.env(hash = TRUE, parent = empty_env(), size = length(openbook)) # 66MB mem
  ll <- vector(mode = "list", length = length(keys))
  ll <- list2(!!!setNames(values, keys))
  e <- new.env(hash = TRUE, parent = emptyenv(), size = length(ll))
  env_bind_lazy(e, !!!ll) # promise, evaluation on call
}

make_hash <- function(x){
  x <- fst::hash_fst(x, block_hash = T)
  paste0(sprintf("%x", x), collapse = "")
}

#' @param ll nested named list
#' @details
#' FEN string$move$evaluation
#' Each element is a named list of move$evaluation.
#' No type checking is performed. Make sure that
#' 1: Evaluation are integers
#' 2: There are no spaces in the FEN string. use %20 as whitespace.
#'
#' NOTE: expensive operation, will take a while for large sets.
#' TODO if frequent task - optimize this
#'
#' @return vector of hashed FEN positions.
#' @keywords internal
.get_keys <- function(ll){
  require(magrittr)
  require(parallel)
  require(fst)
  nested_names_list <- lapply(ll, names)
  KEYS <- mapply(function(x, i) paste0(i, x),
                 nested_names_list,
                 names(nested_names_list)) %>%
    unlist(use.names = F)
  KEYS <- as.list(KEYS)
  KEYS <- lapply(KEYS, charToRaw)
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl = cl, {
    make_hash <- function(x){
      x <- fst::hash_fst(x, block_hash = T) #xxHash 64 bit algorithm
      paste0(sprintf("%x", x), collapse = "")
    }
  })
  KEYS <- parLapply(cl = cl, X = KEYS, fun = make_hash)
  unlist(KEYS, use.names = F)
}

#' @keywords internal
.get_values <- function(ll){
  unlist(lapply(ll, function(x) unlist(x, use.names = F)), use.names = F)
  # VALUES[-c(197356, 197357, 197358, 197359, 197360)] Donald parsing error
}





# ll <- readRDS("C:\\Users\\D\\Documents\\R\\Xiangqi Cheat\\book\\book.rds")
# ll <- readRDS("C:\\Users\\D\\Documents\\R\\Package\\xiangqi\\bookdata_cmprs.rds")
# KEYS <- .get_keys(ll)
# VALUES <- .get_values(ll)
