# (c) Kevin Dunn, 2014-2018.

paretoPlot <- function(lsmodel, xlab="Effect name", ylab="Magnitude of effect",
                       main="Pareto plot", legendtitle="Sign of coefficients",
                       negative=c("Negative", "grey"),
                       positive=c("Positive", "black")){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(paste0("Package \"ggplot2\" is essential for this function to work. ",
                "Please install it."), call. = FALSE)
  }

  # Extract all the coefficients, except for the intercept
  coeff.full <- coef(lsmodel)[2:length(coef(lsmodel))]
  coeff.full <- na.omit(coeff.full)

  # Return the absolute values of the coefficients
  coeff.abs <- unname(abs(coeff.full))

  # Use "shell" sort, to avoid ties being reordered.
  coeff <- sort(coeff.abs, index.return = TRUE, method = "shell")
  grouping <- unname( (coeff.full > 0)[coeff$ix] )
  grouping[grouping == FALSE] <- negative[1]
  grouping[grouping == TRUE] <- positive[1]
  temp <- names(coeff.full)[coeff$ix]
  fnames <- factor(temp, levels = temp, ordered = TRUE)
  dat <- data.frame(label = fnames, value = coeff$x, group = grouping)

  # Make this work to get the scipt uploaded into CRAN
  # https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  label <- value <- group <- NULL # Setting the variables to NULL first

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = label, y = value, fill = group)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() + ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values = setNames(c(negative[2], positive[2]),
                               c(negative[1], positive[1])),
                               name = legendtitle) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(main)
  plot(p)    # Execute the plot (i.e. draw it!)
  return(p)  # Return the plot, so user can continue to modify it
}

if (FALSE){
  flip <- c(-1, 1)
  design <- expand.grid(D = flip, N = flip, P = flip)
  D <- design$D
  N <- design$N
  P <- design$P
  y <- c(64, 68, 72, 70, 78, 80, 82, 80)
  fit <- lm(y ~ D * N * P)
  paretoPlot(fit)

  T <- c(-1, +1, -1, +1) # centered and scaled temperature
  S <- c(-1, -1, +1, +1) # centered and scaled speed variable
  y <- c(69, 60, 24, 53) # conversion, is our response variable, y
  doe.model <- lm(y ~ T + S + T * S) # create a model with main effects, and interaction
  paretoPlot(doe.model)


  # Test case: all the coefficients are the same (positive) sign.
  # Ensure that it is correctly plotted.  Previous bug.
  R <- S <- F <-  c(-1, +1)
  design <- expand.grid(R = R, S = S, F = F)
  R <- design$R
  S <- design$S
  F <- design$F
  y <- c(6.2, 6.4, 6.1, 5.5, 6.7, 6.0, 7.5, 8.8)
  paretoPlot(lm(y ~ R * S * F))
  paretoPlot(lm(-y ~ R * S * F))
}
