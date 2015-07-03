paretoPlot <- function(lsmodel){
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
  grouping[grouping==FALSE]="Negative"
  grouping[grouping==TRUE]="Positive" 
  temp <- names(coeff.full)[coeff$ix]
  fnames <- factor(temp, levels=temp, ordered=TRUE)
  group.colors <- c("Negative" = "grey", "Positive" = "black")
  
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
    scale_fill_manual(values=group.colors,name = "Sign of coefficient") +
    xlab("Effect") +
    ylab("Magnitude of effect") + 
    ggtitle("Pareto plot")
  p          # Execute the plot (i.e. draw it!)
  return(p)  # Return the plot, so user can continue to modify it
}
