
#game_id <- "o4aOJK"
get_game <- function(game_id){
  exCap <- list("moz:firefoxOptions" = list(args = list('--headless')))

  url <- paste0("https://play.xiangqi.com/game/", game_id)

  rD <- RSelenium::rsDriver(browser = "firefox", check = F, extraCapabilities = exCap)
  remDr <- rD$client
  remDr$setTimeout(type = 'page load', milliseconds = 300)

  remDr$navigate(url = url)

  webElem <- remDr$findElement("css selector", "#moves-tab")
  webElem$clickElement()
  Sys.sleep(2)
  webElem <- remDr$findElement("css selector", ".Wrapper__MovesTabWrapper-sc-13rqht3-2")
  txt <- webElem$getElementText()[[1]]

  txt <- strsplit(txt, "\n")[[1]]
  #moves-tab
  #moves-tab

  txt <- stringi::stri_replace_all_regex(txt, pattern = "^\\d?\\d", replacement = "") %>%
    stringi::stri_remove_empty()
  remDr$close()
  rD$server$stop()
  rm(rD, remDr)
  gc()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  out <- c()
  for (i in seq_along(txt)) {
    if (i %% 2 == 0) {
      out[i] <- tolower(txt[i])
    } else {
      out[i] <- toupper(txt[i])
    }
  }
  out
}

# test
#
# test <- get_game("o4aOJK")
# test2 <- strsplit(test, "")
# t_int_r <- Vectorize(function(moves){
#   switch(moves,
#          "c" = "炮",
#          "p" = "兵",
#          "r" = "车",
#          "a" = "仕",
#          "e" = "相",
#          "k" = "帅",
#          "h" = "马",
#          "+" = "进",
#          "-" = "退",
#          "=" = "平",
#          "1" = "一",
#          "2" = "二",
#          "3" = "三",
#          "4" = "四",
#          "5" = "五",
#          "6" = "六",
#          "7" = "七",
#          "8" = "八",
#          "9" = "九"
#          )
# }, vectorize.args = "moves")
#
# t_int_b <- Vectorize(function(moves){
#   switch(moves,
#          "C" = "炮",
#          "P" = "卒",
#          "R" = "车",
#          "A" = "士",
#          "E" = "象",
#          "K" = "将",
#          "H" = "马",
#          "+" = "进",
#          "-" = "退",
#          "=" = "平",
#          "1" = "１",
#          "2" = "２",
#          "3" = "３",
#          "4" = "４",
#          "5" = "５",
#          "6" = "６",
#          "7" = "７",
#          "8" = "８",
#          "9" = "９"
#   )
# }, vectorize.args = "moves")

# test
# out <- list()
# for (i in seq_along(test2)) {
#   out[[i]] <- if (i %% 2 == 0) {
#     t_int_b(test2[[i]])
#   } else {
#     t_int_r(test2[[i]])
#   }
# }
#

