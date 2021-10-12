test_that("san translation functions is parsing correctly", {
  expect_equal(
    "炮二进二 炮２进７ 炮八平二 士６进５ 前炮进一 象７进５ 马二进三 炮２退８ 帅五进一 卒３进１ 车一平二 卒３进１ 车九平八 卒３平４ 前炮平九 士５退６ 相三进一 马８进６ 相一退三 马６退８" %>%
    clean_moves() %>%
      translate(), "h2h4 b7b0 b2h2 f9e8 h4h5 g9e7 h0g2 b0b8 e0e1 c6c5 i0h0 c5c4 a0b0 c4d4 h5a5 e8f9 g0i2 h9f8 i2g0 f8h9")
})

test_that("lan translation functions is parsing correctly", {
  expect_equal(
    c("h2h4", "b7b0", "b2h2", "f9e8", "h4h5", "g9e7", "h0g2", "b0b8",
      "e0e1", "c6c5", "i0h0", "c5c4", "a0b0", "c4d4", "h5a5", "e8f9",
      "g0i2", "h9f8", "i2g0", "f8h9") %>%
      clean_moves() %>%
      translate(), san <- "炮二进二 炮２进７ 炮八平二 士６进５ 前炮进一 象７进５ 马二进三 炮２退８ 帅五进一 卒３进１ 车一平二 卒３进１ 车九平八 卒３平４ 前炮平九 士５退６ 相三进一 马８进６ 相一退三 马６退８"
    )
})

