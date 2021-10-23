#' Function to create a repository of games, with many games in one file.
#'
#' @param path path to directory to
# path <- "F:\\Masters_D\\"
### TODO: make this generalizable & portable to end users
# write_pgn_bulk <- function(path, file){
#   files <- list.files(path = path)
#   ll <- vector(mode = "list", length = length(files))
#   names(ll) <- files
#
#   errors <- list()
#   tmcn::setchs()
#
#   for (i in seq_along(files)) {
#     if (i %% 1000 == T) {
#       message(i, " done\n")
#     }
#     possible_error <- tryCatch({
#       game <- read_pgn(paste0("F:\\Masters_D\\", files[i]))
#
#       moves <- game$Moves %>%
#         clean_moves()
#     }, error = function(e) { errors[[i]] <<- list(file = files[i],
#                                                   error = "read_pgn")
#     return(e)})
#     tryCatch({
#       moves <- translate_notation(moves, "san")
#     }, error = function(e) { errors[[i]] <<- list(file = files[i],
#                                                   error = "translate_notation")
#     return(e)})
#
#     if(!inherits(possible_error, "error")){
#       #REAL WORK
#       moves <- moves %>%
#         stringi::stri_split_fixed(" ") %>%
#         unlist(use.names = F)
#     }
#     tryCatch({
#       moves[seq(1, length(moves), 2)] <-
#         trimws(
#           stringi::stri_c(
#             paste0("  ", as.character(seq(1, round(length(moves)/2, digits = 0), 1)), ". "),
#             moves[seq(1, length(moves), 2)]
#           )
#         )
#     }, error = function(e) { errors[[i]] <<- list(file = files[i],
#                                                   error = "invalid move list")
#     return(e)})
#     pgn_string <-
#       c("", game$Metadata, "", stringi::stri_c(moves, collapse = " "))
#     ll[[i]] <- pgn_string
#   }
#
#   errors <- errors[!sapply(errors, is.null)]
#   string <- sapply(errors, function(x){x$file})
#   error_files <- which(files %in% string)
#   ll <- ll[-error_files]
#   long <- unlist(ll, use.names = F)
#   #vroom::vroom_write_lines(long, "bulk-dpxq_san.pgn", append = T, )
#   writeLines(long, con = path)
# }
