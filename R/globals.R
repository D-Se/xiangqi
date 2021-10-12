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

