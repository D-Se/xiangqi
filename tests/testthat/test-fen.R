test_that("FEN string conversion works", {
  pos <- STARTPOS
  expect_equal(
  make_fen2(pos[FEN], 1),  "rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR%20w")
})

test_that("FEN string conversion adjecent pieces works", {
  pos <- position_move(c("b2g2", "b7b2", "i0i1", "b2f2", "i1i0", "h7b7", "i0i1", "b7b2", "i1i0", "b2e2"))
  expect_equal(
    make_fen2(pos[FEN], 1),  "rnbakabnr/9/9/p1p1p1p1p/9/9/P1P1P1P1P/4ccCC1/9/RNBAKABNR%20w")
})
