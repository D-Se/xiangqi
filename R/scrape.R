# functions to scrape a set of key websites.

#' Scrape xiangqi.com to obtain a move list of a game.
#'
#' @keywords internal
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



