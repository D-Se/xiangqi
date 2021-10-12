test_that("san translation functions is parsing correctly", {
  expect_equal(
    c("炮二进二", "炮2进7", "炮八平二", "士6进5", "前炮进一", "象7进5",
      "马二进三", "炮2退8", "帅五进一", "卒3进1", "车一平二", "卒3进1",
      "车九平八", "卒3平4", "前炮平九", "士5退6", "相三进一", "马8进6",
      "相一退三", "马6退8") %>%
    clean_moves() %>%
      translate("san"), "h2h4 b7b0 b2h2 f9e8 h4h5 g9e7 h0g2 b0b8 e0e1 c6c5 i0h0 c5c4 a0b0 c4d4 h5a5 e8f9 g0i2 h9f8 i2g0 f8h9")
})

test_that("lan translation functions is parsing correctly", {
  expect_equal(
    c("h2h4", "b7b0", "b2h2", "f9e8", "h4h5", "g9e7", "h0g2", "b0b8",
      "e0e1", "c6c5", "i0h0", "c5c4", "a0b0", "c4d4", "h5a5", "e8f9",
      "g0i2", "h9f8", "i2g0", "f8h9") %>%
      clean_moves() %>%
      translate("lan"), "炮二进二 炮2进7 炮八平二 士6进5 前炮进一 象7进5 马二进三 炮2退8 帅五进一 卒3进1 车一平二 卒3进1 车九平八 卒3平4 前炮平九 士5退6 相三进一 马8进6 相一退三 马6退8")
})

