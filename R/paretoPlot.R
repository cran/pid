# (c) Kevin Dunn, 2014-2015.

paretoPlot <- function(lsmodel, xlab="Effect name", ylab="Magnitude of effect",
                       main="Pareto plot", legendtitle="Sign of coefficient",
                       negative=c("Negative", "grey"),
                       positive=c("Positive", "black")){
  # This code draws a Pareto plot; it requires the "ggplot2" library
  # install.packages("ggplot2", dependencies = TRUE)
  # require(ggplot2)
  
  # Extract all the coefficients, except for the intercept
  coeff.full <- coef(lsmodel)[2:length(coef(lsmodel))]
  coeff.full <- na.omit(coeff.full)
  
  # Return the absolute values of the coefficients
  coeff.abs <- unname(abs(coeff.full))
  
  coeff <- sort(coeff.abs, index.return=TRUE)
  grouping <- unname((coeff.full>0)[coeff$ix])
  grouping[grouping==FALSE]=negative[1]
  grouping[grouping==TRUE]=positive[1]
  temp <- names(coeff.full)[coeff$ix]
  fnames <- factor(temp, levels=temp, ordered=TRUE)
  
  dat <- data.frame(
    label=fnames,
    value=coeff$x,
    group=grouping
  )
  
  # Make this work to get the scipt uploaded into CRAN
  # https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  label <- value <- group <- NULL # Setting the variables to NULL first
  
  p <- ggplot(dat, aes(x=label, y=value, fill=group)) + 
    geom_bar(stat="identity") +
    coord_flip() + theme_bw() +
    scale_fill_manual(values=c(negative[2], positive[2]), 
                      labels=c(negative[1], positive[1]), 
                      name = legendtitle) + 
    xlab(xlab) +
    ylab(ylab) + 
    ggtitle(main)
  p          # Execute the plot (i.e. draw it!)
  return(p)  # Return the plot, so user can continue to modify it
}


# T <- c(-1, +1, -1, +1) # centered and scaled temperature
# S <- c(-1, -1, +1, +1) # centered and scaled speed variable
# y <- c(69, 60, 24, 53) # conversion, is our response variable, y
# doe.model <- lm(y ~ T + S + T * S) # create a model with main effects, and interaction
# paretoPlot(doe.model)
