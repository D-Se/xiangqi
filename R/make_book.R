file.path(file.choose())

games <- read_pgn_bulk("C:\\Users\\D\\Documents\\R\\Xiangqi Cheat\\bulk-dpxq_san.pgn")

ll <- vector(mode = "list", length = length(games))
ll <- lapply(games, `[[`, 2)
rm(games)


ll[[1]][1:30]
lapply(ll, function(x){
  x <- x[1:30]
  pos <- position_move(x)
  fen <- make_fen(pos)
})

x <- ll[[1]][1:30]
h <- character(length = 30)
for (i in seq_along(x)) {
  pos <- position_move(x[1:i])
  h[i] <- make_fen(pos)
}
ll[[1]]
pos <- position_move(ll[[1]][1:30])
print_position(pos)

make_fen(pos)

make_book <- function(){

}
bitwXor()
as.raw("rnbakabnr/9/1c5c1/p3p1p1p/2p6/6P2/P1P1P3P/1C5C1/9/RNBAKABNR")
xor(h, "rnbakabnr/9/1c5c1/p3p1p1p/2p6/6P2/P1P1P3P/1C5C1/9/RNBAKABNR")
h
a <- "2baka2r/r4n3/1cn1b1c2/p3p3p/2p6/5NB2/P1P1P3P/4C2C1/3R5/RNBAKA3"
b <- "rnbakabnr/9/1c5c1/p3p1p1p/2p6/6P2/P1P1P3P/1C5C1/9/RNBAKABNR"
q <- charToRaw("rnbakabnr/9/1c5c1/p3p1p1p/2p6/6P2/P1P1P3P/1C5C1/9/RNBAKABNR")
w <- charToRaw("2baka2r/r4n3/1cn1b1c2/p3p3p/2p6/5NB2/P1P1P3P/4C2C1/3R5/RNBAKA3")

q <- rawToBits(q)
w <- rawToBits(w)
xor(q, w)
bitwXor(pryr::bits(a), pryr::bits(b))
pryr::bits
bitwXor()
bitwXor(q, w)
pryr::bits(b, F)


x <- unlist(pos[FEN], use.names = F)
test <- ll[[1]]

xor(unlist(position_move(test[1:29])[FEN], use.names = F), x)
out <- !xor(x, unlist(position_move(test[1:29])[FEN], use.names = F))

unique_pos <- all(out)


get_fen <- function(engine){
  engine <- bigchess::uci_cmd(engine = e, command = "d")
  x <- uci_read(e)$temp
  fen = substr(fen, start = 6, stop = nchar(fen))
  key = a[25]
}
substr(fen, start = 6, stop = nchar(fen))



make_book <- function(moves, engine){
  if (is.character(engine)) e <- bigchess::uci_engine(engine) else e <- engine
  lan <- moves %>% clean_moves
  if (length(lan) > 30L) lan <- lan[1:30]
  # consider the first 30 moves of the game as possible book moves
  ll <- vector(mode = "list", length = length(lan))
  for (i in seq_along(lan)) {
    curpos <- stringi::stri_c(lan[1:i], collapse = " ")
    e <- bigchess::uci_position(e, moves = curpos, startpos = TRUE)
    e <- bigchess::uci_cmd(engine = e, command = "d")
    x <- uci_read(e)$temp
    ll[[i]][[1]] = substr(x[24], start = 6, stop = nchar(x[24]))
    ll[[i]][[2]] = substr(x[25], start = 6, stop = nchar(x[25]))
  }
  bigchess::uci_quit(e)
  ll
}

q <- make_book(moves, engine)
q
moves
lan


engine <- eng
moves <- LAN %>% clean_moves()
