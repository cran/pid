# (c) Kevin Dunn, 2014 - 2018.

# This code draws a contour plot; it requires the "ggplot2" library
# install.packages("ggplot2", dependencies = TRUE)
# require(ggplot2)

contourPlot <- function(lsmodel, xlab=attr(lsmodel$terms, "term.labels")[1],
                        ylab=attr(lsmodel$terms, "term.labels")[2],
                        main="Contour plot",
                        N=25,
                        xlim=c(-3.2, 3.2),
                        ylim=c(-3.2, 3.2),
                        colour.function=terrain.colors){

  # Make this work to get the scipt uploaded into CRAN
  # https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  ..level.. <- NULL# Make codetools happy

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(paste0("Package \"ggplot2\" is essential for this function to work. ",
                "Please install it."), call. = FALSE)
  }

  # N <- 25: resolution of surface  (higher values give smoother plots)
  # xlim <- +/- 3.2: range of the coded variables to plot on the axes
  H.grid <- seq(xlim[1], xlim[2], length = N)
  V.grid <- seq(ylim[1], ylim[2], length = N)
  grd <- expand.grid(H.grid, V.grid)
  n <- dim(grd)[1]

  # Valid column names are only those that are not the response variable.
  # The response variable is model.frame(lsmodel)[, 1], so ignore that first
  # column.
  valid.names <- colnames(model.frame(lsmodel))[dim(model.frame(lsmodel))[2]:2]

  if (!is.character(xlab)){
    stop('The "xlab" input must be a character (string) name of a variable in the model.')
  }
  if (!is.character(ylab)){
    stop('The "ylab" input must be a character (string) name of a variable in the model.')
  }
  if (!(xlab %in% valid.names)){
    stop(paste('The variable "', toString(xlab),
               '" was not a variable name in the linear model.\n ',
               "Valid variable names are: ", toString(valid.names), sep = ""))
  }
  if (!(ylab %in% valid.names)){
    stop(paste('The variable "', toString(ylab),
               '" was not a variable name in the linear model.\n ',
               "Valid variable names are: ", toString(valid.names), sep = ""))
  }
  valid.names <- valid.names[valid.names != xlab]
  valid.names <- valid.names[valid.names != ylab]

  h.points <- model.frame(lsmodel)[, xlab]
  v.points <- model.frame(lsmodel)[, ylab]
  expt_points <- data.frame(xlab = h.points, ylab = v.points)
  colnames(grd) <- c(xlab, ylab)
  colnames(expt_points) <- c(xlab, ylab)
  grd <- rbind(grd, expt_points)
  n_points_grid <- dim(grd)[1]

  # Set any unspecified variables to zero.
  # TODO: improve by allowing these variables to be specified with the ... (ellipsis)
  # input to this function
  for (elem in valid.names){
    grd[[elem]] <- 0
  }

  # Predict directly from least squares model
  grd$y <- predict(lsmodel, grd)
  binwidth <- (max(grd$y) - min(grd$y)) / 20

  p <- ggplot2::ggplot(data = grd[1:n, ],
                       ggplot2::aes_string(x = xlab, y = ylab, z = "y")) +
    ggplot2::stat_contour(ggplot2::aes(color = ..level..), binwidth = binwidth) +
    ggplot2::scale_colour_gradientn(colours = colour.function(N)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")) +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(2))) +
    ggplot2::theme(axis.title = ggplot2::element_text(face = "bold",
                                                      size = ggplot2::rel(1.5))) +
    ggplot2::labs(title = main) +
    ggplot2::geom_point(data = grd[ (n + 1):n_points_grid, ],
                        ggplot2::aes_string(x = xlab, y = ylab), size = 5) +
    ggplot2::scale_x_continuous(breaks = seq(round(xlim[1]), round(xlim[2]), by = 1)) +
    ggplot2::scale_y_continuous(breaks = seq(round(ylim[1]), round(ylim[2]), by = 1))

  plot(p)    # Execute the plot (i.e. draw it!)
  return(p)  # Return the plot, so user can continue to modify it
}
# P <- c(-1,   +1,  -1, +1)
# T <- c(-1,   -1,  +1, +1)
# y <- c(715, 713, 733, 725)
# contourPlot(lm(y~T*P))
# P <- c(P,   0,   0,   0,   0)
# T <- c(T,   0,   0,   0,   0)
# y <- c(y, 732, 733, 737, 735)
# P.exact <- c(P,     0, -1.41,     0, +1.41)
# T.exact <- c(T, +1.41,     0, -1.41,     0)
# P.star.low  <- (1.38 - 1.63)/(0.5*0.36)
# P.star.high <- (1.88 - 1.63)/(0.5*0.36)
# T.star.low  <- (335 - 339)/(0.5*6)
# T.star.high <- (343 - 339)/(0.5*6)
# P <- c(P,           0, P.star.low,          0, P.star.high)
# T <- c(T, T.star.high,          0, T.star.low,           0)
# y <- c(y,         738,        717,        721,         710)
# model.7 <- lm(y ~ P*T + I(P^2) + I(T^2))
# summary(model.7)
# contourPlot(model.7, 'P', 'T', xlim=c(-1.5, 1.5), ylim=c(-2,3), N=50)
#
# y <- c(407, 193, 468, 310, 571, 529, 420,498)
# P <- c(  0,  -1,  +1,  -1,  +1, -1, 1.4, 2.)
# T <- c(  0,  -1,  -1,  +1,  +1, +1,1,1.5)
# mod.base.0 <- lm(y ~ P*T)
# #contourPlot(mod.base.0, "P", "T")
#
# Q <- P*T
# mod.base.1 <- lm(y ~ P*T*Q)
# summary(mod.base.1)
# #contourPlot(mod.base.1, "A")
# #contourPlot(mod.base.1, "P", "B")
# contourPlot(mod.base.1)
