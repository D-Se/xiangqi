
# make_san ----------------------------------------------------------------

# 炮二平五 (san) to h2e2 (UCI)
# *input: chr vector of length 1 without whitespaces*
# bench > ~24 ms for 150 move game
make_san <- function(move, pos, p){
  ptrns <- c("^[炮车帅将]", "^[马]", "^[相仕象士]", "^[兵卒]", "^[前后]")
  fp <- which(stringi::stri_detect_regex(move, pattern = ptrns, max_count = 1, negate = F))
  sm <- strsplit(move,"")[[1]]
  # 1. pawn 2. canon 3. rook 4. horse 5. elephant 6. advisor 7. general
  pval <- t_piece(sm[1], p)
  sq <- names(which(pos == pval)) # indices containing piece of interest
  dir <- t_dir(sm[3])
  switch(fp,
         "1" = { # orthogonal movement
           i <- sq[which(substr(sq, 1,1) == t_col(sm[2], p))]
           c1 <- substr(i, 1, 1)
           r1 <- substr(i, 2, 2)
           c2 <- switch(dir,
                        "forward" = c1,
                        "backward" = c1,
                        "lateral" = t_col(sm[4], p))
           r2 <- switch(dir,
                        "forward" = dir_fwd(r1, sm[4], p),
                        "backward" = dir_bck(r1, sm[4], p),
                        "lateral" = r1)
         },
         "2" = { # diagonal movement
           c1 <- t_col(sm[2], p)
           c2 <- t_col(sm[4], p)
           r1 <- substr(sq[which(substr(sq, 1, 1) == c1)], 2, 2)
           r2 <- horse_move(c1, c2, r1, p, dir)},
         "3" = { # advisor / elephant move
           c1 <- t_col(sm[2], p)
           c2 <- t_col(sm[4], p)
           i <- names(pos[which(pos == pval)])
           r1 <-
             if (length(i) > 1 && length(unique(substr(sq, 1,1))) > 1) {
               substr(sq[which(substr(i, 1, 1) == c1)], 2, 2)
             } else {
               diag_test(sq, pval, dir, p)
             }
           r2 <- diagonal_move(r1, pval, dir, p)
         },
         "4" = {  # pawn moves
           c1 <- t_col(sm[2], p)
           c2 <- if (dir == "forward") c1 else t_col(sm[4], p)
           r1 <- substr(sq[which(substr(sq, 1, 1) == c1)], 2, 2)
           r2 <- switch(dir,
                        "forward" = dir_fwd(r1, sm[4], p),
                        "lateral" = r1)},
         "5" = { # pieces on the same file
           pval <- t_piece(sm[2], p)
           sq <- names(pos[which(pos == pval)])
           x <- as.integer(substr(sq, 2, 2))
           r1 <- if (p == 1) { # returns integer, coerced to character in end
             if (sm[1] == "前") x[which(x == max(x))] else x[which(x == min(x))]
           } else {
             if (sm[1] == "前") x[which(x == min(x))] else x[which(x == max(x))]
           }
           c1 <- substr(sq[which(x == r1)], 1, 1)
           c2 <-
             if (dir=="lateral"||abs(pval) == 4) t_col(sm[4], p) else c1
           r2 <- switch(as.character(abs(pval)),
                        "1" = switch(dir,
                                     "forward" = dir_fwd(r1, sm[4], p),
                                     "lateral" = r1),
                        "2" = switch(dir,
                                     "forward" = dir_fwd(
                                       r1, sm[4], p),
                                     "backward" = dir_bck(
                                       r1, sm[4], p),
                                     "lateral" = r1
                        ),
                        "3" = switch(dir,
                                     "forward" = dir_fwd(
                                       r1, sm[4], p),
                                     "backward" = dir_bck(
                                       r1, sm[4], p),
                                     "lateral" = r1),
                        "4" = horse_move(c1, c2, r1, p, dir),
                        "5" = diagonal_move(r1, pval, dir, p),
                        "6" = diagonal_move(r1, pval, dir, p)
           )
         }
  )
  paste0(c1,r1,c2,r2)
}

t_col <- function(index, p){
  if (p == 1) {
    switch(index,
           # N = 2586705
           "五" = "e", # 362217
           "七" = "c", # 333487
           "二" = "h", # 330484
           "八" = "b", # 333084
           "三" = "g", # 317999
           "六" = "d", # 256164
           "四" = "f", # 247073
           "九" = "a", # 226310
           "一" = "i" # 179887
    )
  } else {
    switch(index,
           "３" = "c", # 328055
           "７" = "g", # 327831
           "８" = "h", # 327607
           "２" = "b", # 321326
           "５" = "e", # 320640
           "４" = "d", # 269091
           "６" = "f", # 233653
           "１" = "a", # 221565
           "９" = "i" # 205135
    )
  }
}
t_nr <- function(index, p){
  if (p == 1) {
    switch(index,
           "五" = 5, # 362217
           "七" = 7, # 333487
           "二" = 2, # 330484
           "八" = 8, # 333084
           "三" = 3, # 317999
           "六" = 6, # 256164
           "四" = 4, # 247073
           "九" = 9, # 226310
           "一" = 1, # 179887
    )
  } else {
    switch(index,
           "３" = 3, # 328055
           "７" = 7, # 327831
           "８" = 8, # 327607
           "２" = 2, # 321326
           "５" = 5, # 320640
           "４" = 4, # 269091
           "６" = 6, # 233653
           "１" = 1, # 221565
           "９" = 9 # 205135
    )
  }
}
t_piece <- function(index, p){
  if (p == 1) {
    # optimized for grandmaster pieces moved / game N = 65k
    switch(index,
           "车" = 3, # 12.2
           "炮" = 2, # 9.22
           "马" = 4, # 9.03
           "兵" = 1, # 5.66
           "帅" = 7, # 2.71
           "相" = 5, # 2.28
           "仕" = 6 # 1.95
    ) # 1. pawn 2. canon 3 rook 4 horse 5 elephant 6 advisor 7 general
  } else {
    switch(index,
           "车" = -3, # 11.6
           "炮" = -2, # 9.16
           "马" = -4, # 8.98
           "卒" = -1, # 5.17
           "将" = -7, #3.07
           "象" = -5, # 2.55,
           "士" = -6 # 2.15
    )
  }
}
t_dir <- function(index){
  switch(index,
         "进" = "forward",
         "平" = "lateral",
         "退" = "backward",
  )
}
horse_move <- function(c1, c2, r1, p, dir){
  r1 <- as.integer(r1)
  as.character(
    if (abs(CN[c2]-CN[c1]) == 1) {
      if (dir == "forward") {
        if (p == 1) r1+2 else r1-2
      } else { # backward
        if (p == 1) r1-2 else r1+2
      }
    } else { # abs value difference of 2
      if (dir == "forward") {
        if (p == 1) r1+1 else r1-1
      } else { # backward
        if (p == 1) r1-1 else r1+1
      }
    }
  )
}
dir_fwd <- function(index, n, p){
  index <- as.integer(index)
  as.character(
    if (p == 1) {
      index + t_nr(n, p)
    } else {
      index - t_nr(n, p)
    }
  )
}
dir_bck <- function(index, n, p){
  index <- as.integer(index)
  as.character(
    if (p == 1) {
      index - t_nr(n, p)
    } else {
      index + t_nr(n, p)
    }
  )
}
diagonal_move <- function(r1, pval, dir, p){
  r1 <- as.integer(r1)
  as.character(
    if (abs(pval) == 5) { # elephant move
      if (dir == "forward") {
        if (p == 1) r1+2 else r1-2
      } else { # backward elephant
        if (p == 1) r1-2 else r1+2
      }
    } else { # advisor move
      if (dir == "forward") {
        if (p == 1) r1+1 else r1-1
      } else { # backward elephant
        if (p == 1) r1-1 else r1+1
      }
    }
  )
}
# process of elimination for when two pieces are on the same file
diag_test <- function(index, pval, dir, p){
  x <- as.integer(substr(index, 2, 2))
  as.character(
    switch(dir,
           "forward" = {
             switch(as.character(p),
                    "1" = {
                      switch(as.character(pval),
                             "5" = x[!x + 2 > 4],
                             "6" = x[!x + 1 > 2])
                    },
                    "-1" = {
                      switch(as.character(pval),
                             "-5" = x[!x - 2 < 5],
                             "-6" = x[!x - 1 < 7])
                    })
           },
           "backward" = {
             switch(as.character(p),
                    "1" = {
                      switch(as.character(pval),
                             "5" = x[!x - 2 < 0],
                             "6" = x[!x - 1 < 0])
                    },
                    "-1" = {
                      switch(as.character(pval),
                             "-5" = x[!x + 2 > 9],
                             "-6" = x[!x + 1 > 9])
                    })
           })
  )
}


# make_lan ----------------------------------------------------------------


# h2e2 (UCI) (san) to 炮二平五 (san)
#' *input: chr vector of length 1 without whitespaces*
#' TODO logic pass, restructure func.
make_lan <- function(move, pos, p){
  sq1 <- substr(move, 1, 2)
  sm <- strsplit(move, "")[[1]]
  c1 = sm[1] ; r1 = sm[2]; c2 = sm[3] ; r2 = sm[4]
  pval <- pos[[sq1]]
  out <- vector(mode = "character", length = 4)
  ind <- names(pos[which(pos == pval)])
  # regular pieces following piece | c1 | direction | target
  if (any(abs(pval) == 5:7)) {
    out[1] <- t_piece_rev(pval, p)
    out[2] <- t_col_rev(sm[1], p)
    direction <- if (abs(pval) == 7) {
      if (!CN[sm[1]] == CN[sm[3]]) "lateral" else get_dir(r1, r2, p)
    } else {
      get_dir(sm[2], sm[4], p)
    }
    out[3] <- t_dir_rev(direction)
    out[4] <- switch(out[1],
                     "仕" = ,
                     "士" = ,
                     "象" = ,
                     "相" = {
                       switch(direction,
                              "forward" = t_col_rev(sm[3], p),
                              "backward" = t_col_rev(sm[3], p))
                     },
                     "将" = ,
                     "帅" = {
                       switch(direction,
                              "lateral" = t_col_rev(sm[3], p),
                              "forward" = t_nr_rev(1, p),
                              "backward" = t_nr_rev(1, p))
                     })
  } else if (length(ind) > 1 && substr(ind[1],1,1) == substr(ind[2], 1, 1)) {
    i <- as.integer(substr(ind, 2, 2))
    r1 <- as.integer(sm[2])
    r2 <- as.integer(sm[4])
    if (p == 1) {
      out[1] <- if (max(i) == r1) "前" else "后"
    } else {
      out[1] <- if (max(i) == r1) "后" else "前"
    }
    out[2] <- t_piece_rev(pval, p)
    direction <-
      if (!CN[sm[1]] == CN[sm[3]]) "lateral" else get_dir(r1, r2, p)
    out[3] <- t_dir_rev(direction)
    out[4] <- switch(out[2],
                     "马" = {
                       switch(direction,
                              "forward" = t_col_rev(sm[3], p),
                              "backward" = t_col_rev(sm[3], p))
                     },
                     "炮" = ,
                     "车" = {
                       switch(direction,
                              "lateral" = t_col_rev(sm[3], p),
                              "forward" = t_nr_rev(abs(r2-r1), p),
                              "backward" = t_nr_rev(abs(r2-r1), p))
                     },
                     "卒" = ,
                     "兵" = {
                       switch(direction,
                              "lateral" = t_col_rev(sm[3], p),
                              "forward" = t_nr_rev(1, p))
                     })
  } else {
    out[1] <- t_piece_rev(pval, p)
    out[2] <- t_col_rev(sm[1], p)
    r1 <- as.integer(sm[2])
    r2 <- as.integer(sm[4])
    direction <- get_dir(r1, r2, p)
    out[3] <- t_dir_rev(direction)
    out[4] <- switch(out[1],
                     "马" = {
                       switch(direction,
                              "forward" = t_col_rev(sm[3], p),
                              "backward" = t_col_rev(sm[3], p))
                     },
                     "炮" = ,
                     "车" = {
                       switch(direction,
                              "lateral" = t_col_rev(sm[3], p),
                              "forward" = t_nr_rev(abs(r2-r1), p),
                              "backward" = t_nr_rev(abs(r2-r1), p))
                     },
                     "卒" = ,
                     "兵" = {
                       switch(direction,
                              "lateral" = t_col_rev(sm[3], p),
                              "forward" = t_nr_rev(1, p))
                     })
  }
  out
}

t_piece_rev <- function(index, p){
  index <- as.character(index)
  if (p == 1) {
    # optimized for grandmaster pieces moved / game N = 65k
    switch(index,
           "3" = "车", # 12.2
           "2" = "炮", # 9.22
           "4" = "马", # 9.03
           "1" = "兵", # 5.66
           "7" = "帅", # 2.71
           "5" = "相", #2.28
           "6" = "仕" #1.95
    ) # 1. pawn 2. canon 3 rook 4 horse 5 elephant 6 advisor 7 general
  } else {
    switch(index,
           "-3" = "车", # 11.6
           "-2" = "炮", # 9.16
           "-4" = "马", # 8.98
           "-1" = "卒", # 5.17
           "-7" = "将", #3.07
           "-5" = "象", # 2.55,
           "-6" = "士" # 2.15
    )
  }
}
t_col_rev <- function(index, p){
  index <- as.character(index)
  if (p == 1) {
    switch(index,
           # grandmaster game position N = 2586705
           "e" = "五", # 362217
           "c" = "七", # 333487
           "h" = "二", # 330484
           "b" = "八", # 333084
           "g" = "三", # 317999
           "d" = "六", # 256164
           "f" = "四", # 247073
           "a" = "九", # 226310
           "i" = "一" # 179887
    )
  } else {
    switch(index,
           "c" = "３", # 328055
           "g" = "７", # 327831
           "h" = "８", # 327607
           "b" = "２",  # 321326
           "e" = "５", # 320640
           "d" = "４", # 269091
           "f" = "６", # 233653
           "a" = "１", # 221565
           "i" = "９" # 205135
    )
  }
}
t_nr_rev <- function(index, p){
  index <- as.character(index)
  if (p == 1) {
    switch(index,
           "5" = "五", # 362217
           "7" = "七", # 333487
           "2" = "二", # 330484
           "8" = "八", # 333084
           "3" = "三", # 317999
           "6" = "六", # 256164
           "4" = "四", # 247073
           "9" = "九", # 226310
           "1" = "一" # 179887
    )
  } else {
    switch(index,
           "3" = "３", # 328055
           "7" = "７", # 327831
           "8" = "８", # 327607
           "2" = "２", # 321326
           "5" = "５", # 320640
           "4" = "４", # 269091
           "6" = "６", # 233653
           "1" = "１", # 221565
           "9" = "９" # 205135
    )
  }
}
t_dir_rev <- function(index){
  switch(index,
         "forward" = "进",
         "lateral" = "平",
         "backward" = "退",
  )
}
get_dir <- function(r1, r2, p){
  if (p == 1) {
    if (r1 < r2) {
      "forward"
    } else if (r1 > r2) {
      "backward"
    } else{
      "lateral"
    }
  } else {
    if (r1 > r2) {
      "forward"
    } else if (r1 < r2) {
      "backward"
    } else{
      "lateral"
    }
  }
}




# make_fen ----------------------------------------------------------------
### TODO: make no-capture counter

# position to FEN string  "rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR"
# see https://www.chessprogramming.org/Forsyth-Edwards_Notation
make_fen2 <- function(pos, p, cloud_db = TRUE, ...){
  p <- if (p == 1) "w" else "b"
  pos <- pos[FEN]
  x <- vector(mode = "list", length = 10L)
  x <- unlist(pos, use.names = F)
  x <- split(x, cut(seq_along(x), 10L, labels = FALSE)) # row by row
  x <- lapply(x, function(x){ # rowwise counting of piece sequences
    y <- x[-1L] != x[-9L] # rle function stripped
    i <- c(which(y), 9L)
    x <- list(lengths = diff(c(0L, i)),
              values = x[i])
    ind_0 <- .Internal(which(x[[2]] == 0L)) # identify zeros to count / replace
    ind_ix <- which(x[[2]] != 0L)
    ind_ix <- ind_ix[which(x[[1]][ind_ix] > 1)]
    if (!rlang::is_empty(ind_ix)) { # pieces of same color on adjecent files
      ind_vx <- t_fen(as.character((x[[2]][ind_ix])))
    }
    string <- t_fen(as.character(x[[2]]))
    char_ind <- as.character(x[[1]][ind_0])
    string[ind_0] <- char_ind
    if (exists(x = "ind_vx")) {
      for (i in seq_along(ind_vx)) { ### TODO: for-loop not necessary here?
        string <- append(string,
                         values = ind_vx[i],
                         which(string == ind_vx[i]) - 1)
      }
    }
    string
  })
  if (cloud_db) { # fen for chessdb.cn cloud book search, %20 is a space
    paste0(stringi::stri_c_list(x, collapse = "/"), "%20", p)
  } else { # ... is used to attach a move counter, as in cyclone engine
    paste(stringi::stri_c_list(x, collapse = "/"), p, "- - 0", n, sep = " ")
  }
}

# placeholder in case new make_fen2 breaks
make_fen <- function(pos, p, cloud_db = TRUE, ...){
  p <- if (p == 1) "w" else "b"
  pos <- pos[FEN]
  x <- vector(mode = "list", length = 10L)
  x <- unlist(pos, use.names = F)
  x <- split(x, cut(seq_along(x), 10L, labels = FALSE)) # row by row
  x <- x$`3`
  x <- lapply(x, function(x){
    y <- x[-1L] != x[-9L] # rle function stripped
    i <- c(which(y), 9L)
    x <- list(lengths = diff(c(0L, i)),
              values = x[i])
    ind_0 <- .Internal(which(x[[2]] == 0L))
    string <- t_fen(as.character(x[[2]]))
    char_ind <- as.character(x[[1]][ind_0])
    string[ind_0] <- char_ind
    string
  })
  if (cloud_db) { # fen for chessdb.cn cloud book search
    paste0(stringi::stri_c_list(x, collapse = "/"), "%20", p)
  } else { # ... is used to attach a move counter, as in cyclone engine
    paste(stringi::stri_c_list(x, collapse = "/"), p, "- - 0", n, sep = " ")
  }
}

t_fen <- Vectorize(function(ind){
  switch(ind,
         "0" = "0",
         "1" = "P",
         "-1" = "p",
         "2" = "C",
         "-2" = "c",
         "3" = "R",
         "-3" = "r",
         "4" = "N",
         "-4" = "n",
         "5" = "B",
         "-5" = "b",
         "6" = "A",
         "-6" = "a",
         "7" = "K",
         "-7" = "k")
})


# make_xiangqi ------------------------------------------------------------

### TODO: Reduce the code for make_xiangqi
# make_fen(position_move(test[1:10]), p = 1, n = 10)
make_xiangqi <- function(move, pos, p){
  ptrns <- c("^[cCrRkK]", "^[hH]", "^[eEaA]", "^[pP]", "^.[+-]") # 2nd pos
  fp <- which(stringi::stri_detect_regex(move, pattern = ptrns, max_count = 1, negate = F))
  sm <- strsplit(move,"")[[1]]
  # 1. pawn 2. canon 3. rook 4. horse 5. elephant 6. advisor 7. general
  pval <- t_piece_xq(sm[1], p)
  sq <- names(which(pos == pval)) # indices containing piece of interest
  dir <- t_dir_xq(sm[3])
  switch(fp,
         "1" = { # orthogonal movement
           i <- sq[which(substr(sq, 1,1) == t_col_xq(sm[2], p))]
           c1 <- substr(i, 1, 1)
           r1 <- substr(i, 2, 2)
           c2 <- switch(dir,
                        "forward" = c1,
                        "backward" = c1,
                        "lateral" = t_col_xq(sm[4], p))
           r2 <- switch(dir,
                        "forward" = dir_fwd_xq(r1, sm[4], p),
                        "backward" = dir_bck_xq(r1, sm[4], p),
                        "lateral" = r1)
         },
         "2" = { # diagonal movement
           c1 <- t_col_xq(sm[2], p)
           c2 <- t_col_xq(sm[4], p)
           r1 <- substr(sq[which(substr(sq, 1, 1) == c1)], 2, 2)
           r2 <- horse_move_xq(c1, c2, r1, p, dir)},
         "3" = { # advisor / elephant move
           c1 <- t_col_xq(sm[2], p)
           c2 <- t_col_xq(sm[4], p)
           i <- names(pos[which(pos == pval)])
           r1 <-
             if (length(i) > 1 && length(unique(substr(sq, 1,1))) > 1) {
               substr(sq[which(substr(i, 1, 1) == c1)], 2, 2)
             } else {
               diag_test_xq(sq, pval, dir, p)
             }
           r2 <- diagonal_move_xq(r1, pval, dir, p)
         },
         "4" = {  # pawn moves
           c1 <- t_col_xq(sm[2], p)
           c2 <- if (dir == "forward") c1 else t_col_xq(sm[4], p)
           r1 <- substr(sq[which(substr(sq, 1, 1) == c1)], 2, 2)
           r2 <- switch(dir,
                        "forward" = dir_fwd_xq(r1, sm[4], p),
                        "lateral" = r1)
           },
         "5" = { # pieces on the same file
           sm <- sm[c(2,1,3,4)]
           pval <- t_piece_xq(sm[2], p)
           sq <- names(pos[which(pos == pval)])
           x <- as.integer(substr(sq, 2, 2))
           r1 <- if (p == 1) { # returns integer, coerced to character in end
             if (sm[1] == "+") x[which(x == max(x))] else x[which(x == min(x))]
           } else {
             if (sm[1] == "-") x[which(x == min(x))] else x[which(x == max(x))]
           }
           c1 <- substr(sq[which(x == r1)], 1, 1)
           c2 <-
             if (dir=="lateral"||abs(pval) == 4) t_col_xq(sm[4], p) else c1
           r2 <- switch(as.character(abs(pval)),
                        "1" = switch(dir,
                                     "forward" = dir_fwd_xq(r1, sm[4], p),
                                     "lateral" = r1),
                        "2" = switch(dir,
                                     "forward" = dir_fwd_xq(
                                       r1, sm[4], p),
                                     "backward" = dir_bck_xq(
                                       r1, sm[4], p),
                                     "lateral" = r1
                        ),
                        "3" = switch(dir,
                                     "forward" = dir_fwd_xq(
                                       r1, sm[4], p),
                                     "backward" = dir_bck_xq(
                                       r1, sm[4], p),
                                     "lateral" = r1),
                        "4" = horse_move_xq(c1, c2, r1, p, dir),
                        "5" = diagonal_move_xq(r1, pval, dir, p),
                        "6" = diagonal_move_xq(r1, pval, dir, p)
           )
         }
  )
  paste0(c1,r1,c2,r2)
}

t_col_xq <- function(index, p){
  if (p == 1) {
    switch(index,
           # N = 2586705
           "5" = "e", # 362217
           "7" = "c", # 333487
           "2" = "h", # 330484
           "8" = "b", # 333084
           "3" = "g", # 317999
           "6" = "d", # 256164
           "4" = "f", # 247073
           "9" = "a", # 226310
           "1" = "i" # 179887
    )
  } else {
    switch(index,
           "3" = "c", # 328055
           "7" = "g", # 327831
           "8" = "h", # 327607
           "2" = "b", # 321326
           "5" = "e", # 320640
           "4" = "d", # 269091
           "6" = "f", # 233653
           "1" = "a", # 221565
           "9" = "i" # 205135
    )
  }
}
t_nr_xq <- function(index, p){
  if (p == 1) {
    switch(index,
           "5" = 5, # 362217
           "7" = 7, # 333487
           "2" = 2, # 330484
           "8" = 8, # 333084
           "3" = 3, # 317999
           "6" = 6, # 256164
           "4" = 4, # 247073
           "9" = 9, # 226310
           "1" = 1, # 179887
    )
  } else {
    switch(index,
           "3" = 3, # 328055
           "7" = 7, # 327831
           "8" = 8, # 327607
           "2" = 2, # 321326
           "5" = 5, # 320640
           "4" = 4, # 269091
           "6" = 6, # 233653
           "1" = 1, # 221565
           "9" = 9 # 205135
    )
  }
}
t_piece_xq <- function(index, p){
  if (p == 1) {
    # optimized for grandmaster pieces moved / game N = 65k
    switch(index,
           "R" = 3, # 12.2
           "C" = 2, # 9.22
           "H" = 4, # 9.03
           "P" = 1, # 5.66
           "K" = 7, # 2.71
           "E" = 5, # 2.28
           "A" = 6 # 1.95
    ) # 1. pawn 2. canon 3 rook 4 horse 5 elephant 6 advisor 7 general
  } else {
    switch(index,
           "r" = -3, # 11.6
           "c" = -2, # 9.16
           "h" = -4, # 8.98
           "p" = -1, # 5.17
           "k" = -7, #3.07
           "e" = -5, # 2.55,
           "a" = -6 # 2.15
    )
  }
}
t_dir_xq <- function(index){
  switch(index,
         "+" = "forward",
         "=" = "lateral",
         "-" = "backward",
  )
}
horse_move_xq <- function(c1, c2, r1, p, dir){
  r1 <- as.integer(r1)
  as.character(
    if (abs(CN[c2]-CN[c1]) == 1) {
      if (dir == "forward") {
        if (p == 1) r1+2 else r1-2
      } else { # backward
        if (p == 1) r1-2 else r1+2
      }
    } else { # abs value difference of 2
      if (dir == "forward") {
        if (p == 1) r1+1 else r1-1
      } else { # backward
        if (p == 1) r1-1 else r1+1
      }
    }
  )
}
dir_fwd_xq <- function(index, n, p){
  index <- as.integer(index)
  as.character(
    if (p == 1) {
      index + t_nr_xq(n, p)
    } else {
      index - t_nr_xq(n, p)
    }
  )
}
dir_bck_xq <- function(index, n, p){
  index <- as.integer(index)
  as.character(
    if (p == 1) {
      index - t_nr_xq(n, p)
    } else {
      index + t_nr_xq(n, p)
    }
  )
}
diagonal_move_xq <- function(r1, pval, dir, p){
  r1 <- as.integer(r1)
  as.character(
    if (abs(pval) == 5) { # elephant move
      if (dir == "forward") {
        if (p == 1) r1+2 else r1-2
      } else { # backward elephant
        if (p == 1) r1-2 else r1+2
      }
    } else { # advisor move
      if (dir == "forward") {
        if (p == 1) r1+1 else r1-1
      } else { # backward elephant
        if (p == 1) r1-1 else r1+1
      }
    }
  )
}
# process of elimination for when two pieces are on the same file
diag_test_xq <- function(index, pval, dir, p){
  x <- as.integer(substr(index, 2, 2))
  as.character(
    switch(dir,
           "forward" = {
             switch(as.character(p),
                    "1" = {
                      switch(as.character(pval),
                             "5" = x[!x + 2 > 4],
                             "6" = x[!x + 1 > 2])
                    },
                    "-1" = {
                      switch(as.character(pval),
                             "-5" = x[!x - 2 < 5],
                             "-6" = x[!x - 1 < 7])
                    })
           },
           "backward" = {
             switch(as.character(p),
                    "1" = {
                      switch(as.character(pval),
                             "5" = x[!x - 2 < 0],
                             "6" = x[!x - 1 < 0])
                    },
                    "-1" = {
                      switch(as.character(pval),
                             "-5" = x[!x + 2 > 9],
                             "-6" = x[!x + 1 > 9])
                    })
           })
  )
}
