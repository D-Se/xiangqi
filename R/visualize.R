#' Create visualizations of game evaluation
#'
#' @param ll a list describing game progression, see `describe()`
#' @returns score progression chart
# input from describe
visualize <- function(ll){
  df <- ll[[1]]
  stages <- c("Middlegame" = 15, "Endgame" = 40)
  cols <- c("black" = "black", "red" = "red")
  tbl_ind <-
    if (abs(max(df$score, na.rm = T)) < abs(min(df$score, na.rm = T))) {
      -min(df$score, na.rm = T)-500
    } else {
      max(df$score, na.rm = T)
    }
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = move, y = score, color = advantage)) +
    ggplot2::geom_path(ggplot2::aes(group = 1), size = 1, linejoin = "round") +
    ggplot2::geom_hline(yintercept = 0, color = "black", size = 1, alpha = .5) +
    sapply(stages, function(xint) ggplot2::geom_vline(xintercept = stages)) +
    ggplot2::annotate("text", x = 15, y = 500, label = "opening", hjust = 1.1) +
    ggplot2::annotate("text", x = 40, y = 500, label = "endgame", hjust = 1.1) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                limits = c(1,NA),
                                breaks = c(seq(from = 0, to = nrow(df), by = 5))) +
    ggplot2::scale_y_continuous(limits = symmetric_limits) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
  #ggpp::geom_table(x = 1, y = tbl_ind, label = list(ll[[3]]))
  # ggplot2::annotate(geom = "table",
  #                  x = 1,
  #                 y = tbl_ind, label = list(ll[[3]]))
  # ggplot2::annotate(geom = "text", x = 1, y = -500, label = list(ll[[2]]))
}
#scale_y_continuous(trans = ggallin::pseudolog10_trans)


symmetric_limits <- function(x) {
  max <- max(abs(x))
  c(-max, max)
}


plot_board <- function(){
  library(png)
  library(grid)
  file.path(file.choose())
  img <- readPNG("C:\\Users\\D\\Desktop\\Board.png")
  img <- readPNG(system.file("img", "Rlogo.png", package="png"))
  g <- rasterGrob(img, interpolate=TRUE)

  qplot(1:10, 1:10, geom="blank") +
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    theme_void()
}
