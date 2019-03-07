  # this script containes functions that evaluate the model perfromance.

mad = function(y, yhat, w = NULL){
  if (is.null(w)){
    w = rep(1, length(y))
  }
  return(sum(abs(y-yhat) * w) / sum(w))
}

rmse = function(y, yhat, w = NULL){
  if (is.null(w)){
    w = rep(1, length(y))
  }
  return(sqrt(sum((y - yhat) ^ 2 * w) / sum(w)))
}

howgood = function(y, yhat, w = NULL){
  print(sprintf("RMSE: %6.2f MAD: %6.2f Rsum: %3.2f",
                rmse(y, yhat, w = w),
                mad(y, yhat, w = w),
                sum(yhat) / sum(y)))
}

plot_lorenz = function(y, yhat){
  require(ggplot2)
  require(dplyr)
  df = data_frame(y = y, yhat = yhat) %>%
    arrange(-yhat) %>%
    mutate(yhat_cumulative = (1:length(y))/ length(y), 
           y_cumulative = cumsum(y) / sum(y))
  ggplot(df) +
    geom_line(aes(yhat_cumulative, y_cumulative)) +
    geom_line(aes(yhat_cumulative, yhat_cumulative))
}

auc = function(y, yhat){
  require(dplyr)
  df = data_frame(y = y, yhat = yhat) %>%
    arrange(-yhat) %>%
    mutate(yhat_cumulative = (1:length(y)) / length(y),
           y_cumulative = cumsum(y) / sum(y))
  return(sum(df$y_cumulative) / nrow(df))
}

cross_entropy = function(y, yhat){
  return(-sum(y * log(yhat) + (1-y) * log(1 - yhat), na.rm = TRUE) / length(y))
}

plot_lorenz2 = function(y1, y1hat, y2, y2hat){
  require(ggplot2)
  require(dplyr)
  df = data_frame(y = y1, yhat = y1hat) %>%
    arrange(-yhat) %>%
    mutate(yhat_cumulative = (1:length(y))/ length(y), 
           y_cumulative = cumsum(y) / sum(y))
  plot(df$yhat_cumulative, df$y_cumulative,
       main = "CAP", xlab = "yhat (ratio)",
       ylab = "y (ratio)", type = "l", col = "blue",
       cex.axis = 1.3, cex.lab = 1.3)
  df = data_frame(y = y2, yhat = y2hat) %>%
    arrange(-yhat) %>%
    mutate(yhat_cumulative = (1:length(y))/ length(y), 
           y_cumulative = cumsum(y) / sum(y))
  lines(df$yhat_cummulative, df$y_cumulative,
        type = "l", col = "red")
  lines(df$yhat_cummulative, df$yhat_cumulative)
  legend(.8, .4, c("train", "test"), cex = 1.3,
         col = c("blue", "red"), pch = c("_", "_"))
}


normalizedGini <- function(aa, pp) {
  Gini <- function(a, p) {
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
    temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
    temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
    population.delta <- 1 / length(a)
    total.losses <- sum(a)
    null.losses <- rep(population.delta, length(a)) # Hopefully is similar to accumulatedPopulationPercentageSum
    accum.losses <- temp.df$actual / total.losses # Hopefully is similar to accumulatedLossPercentageSum
    gini.sum <- cumsum(accum.losses - null.losses) # Not sure if this is having the same effect or not
    sum(gini.sum) / length(a)
  }
  Gini(aa,pp) / Gini(aa,aa)
}