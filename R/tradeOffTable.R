# (c) Kevin Dunn, 2018.

if (!requireNamespace("png", quietly = TRUE)) {
  stop(paste0("Package \"png\" is essential for this function to work. ",
              "Please install it."), call. = FALSE)
}

tradeOffTable <- function(){
  plot.new()
  img <- png::readPNG(system.file("trade-off-table.png", package = "pid"))
  grid::grid.raster(img)
}
