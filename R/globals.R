Sys.setlocale(category = "LC_CTYPE", locale = "chs")
# initial board positionusing Universal Chinese Chess Protocol indices
# Piece values:
# 1. pawn 2. canon 3 rook 4 horse 5 elephant 6 advisor 7 general
STARTPOS <-
  list(a9 = -3L, a8 = 0L, a7 = 0L, a6 = -1L, a5 = 0L, a4 = 0L,
       a3 = 1L, a2 = 0L, a1 = 0L, a0 = 3L, b9 = -4L, b8 = 0L, b7 = -2L,
       b6 = 0L, b5 = 0L, b4 = 0L, b3 = 0L, b2 = 2L, b1 = 0L, b0 = 4L,
       c9 = -5L, c8 = 0L, c7 = 0L, c6 = -1L, c5 = 0L, c4 = 0L, c3 = 1L,
       c2 = 0L, c1 = 0L, c0 = 5L, d9 = -6L, d8 = 0L, d7 = 0L, d6 = 0L,
       d5 = 0L, d4 = 0L, d3 = 0L, d2 = 0L, d1 = 0L, d0 = 6L, e9 = -7L,
       e8 = 0L, e7 = 0L, e6 = -1L, e5 = 0L, e4 = 0L, e3 = 1L, e2 = 0L,
       e1 = 0L, e0 = 7L, f9 = -6L, f8 = 0L, f7 = 0L, f6 = 0L, f5 = 0L,
       f4 = 0L, f3 = 0L, f2 = 0L, f1 = 0L, f0 = 6L, g9 = -5L, g8 = 0L,
       g7 = 0L, g6 = -1L, g5 = 0L, g4 = 0L, g3 = 1L, g2 = 0L, g1 = 0L,
       g0 = 5L, h9 = -4L, h8 = 0L, h7 = -2L, h6 = 0L, h5 = 0L, h4 = 0L,
       h3 = 0L, h2 = 2L, h1 = 0L, h0 = 4L, i9 = -3L, i8 = 0L, i7 = 0L,
       i6 = -1L, i5 = 0L, i4 = 0L, i3 = 1L, i2 = 0L, i1 = 0L, i0 = 3L)

# small lookup table used by helper functions
CN <- 1L:9L
names(CN) <- letters[1:9]

SAN <- "炮二进二 炮２进７ 炮八平二 士６进５ 前炮进一 象７进５ 马二进三 炮２退８ 帅五进一 卒３进１ 车一平二 卒３进１ 车九平八 卒３平４ 前炮平九 士５退６ 相三进一 马８进６ 相一退三 马６退８"
LAN <- "h2h4 b7b0 b2h2 f9e8 h4h5 g9e7 h0g2 b0b8 e0e1 c6c5 i0h0 c5c4 a0b0 c4d4 h5a5 e8f9 g0i2 h9f8 i2g0 f8h9"

eng <- "C:\\XQBase\\Cyclone\\cyclone-nnue-avx2.exe"

# fen goes from top left to top right, up to down
# "rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR"
FEN <- c("a9", "b9", "c9", "d9", "e9", "f9", "g9", "h9", "i9",
         "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8", "i8",
         "a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7", "i7",
         "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6", "i6",
         "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5", "i5",
         "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4", "i4",
         "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3", "i3",
         "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2", "i2",
         "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1", "i1",
         "a0", "b0", "c0", "d0", "e0", "f0", "g0", "h0", "i0")

