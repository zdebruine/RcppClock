#' RcppClock
#' 
#' Time Rcpp functions and summarize, print, and plot runtime statistics
#'
#' @rdname RcppClock
#' 
#' @section RcppClock functions:
#' See the vignette to learn how to use RcppClock in Rcpp code.
#' 
#' When the Rcpp `Rcpp::clock::write()` method is called in Rcpp code, an S3 \code{RcppClock} object
#' will be created in the global environment. This object contains three methods:
#' 
#' * \code{summary}: computes runtime summary statistics and returns a \code{data.frame}
#' * \code{print}:  runs \code{summary} and then prints the resulting \code{data.frame}
#' * \code{plot}:  a ggplot2 violin plot with jitter points showing runtimes for each expression
#' 
#' The \code{\link{fibonacci}} function is a simple example of how to use RcppClock. 
#' See the source code on \code{github.com/zdebruine/RcppClock/src/fibonacci.cpp}
#' 
#' @docType package
#' @name RcpClock
#' @useDynLib RcppClock
#' @importFrom stats aggregate sd
#' @import ggplot2
#' @seealso \code{\link{fibonacci}}
#' @md
#' @examples
#' library(RcppClock)
#' fibonacci(n = 25:35, reps = 10)
#' # this function creates a global environment variable "clock"
#' #   that is an S3 RcppClock object
#' clock
#' plot(clock)
#' summary(clock, units = "ms")
NULL

#' @rdname RcppClock
#' @param x RcppClock object
#' @param units nanoseconds (\code{"ns"}), microseconds (\code{"us"}), milliseconds (\code{"ms"}), seconds (\code{"s"}), or auto (\code{"auto"})
#' @export
#' 
summary.RcppClock <- function(x, units = "auto"){
  min_time <- min(x$timer[x$timer != 0])
  if(is.na(min_time)) min_time <- 0
  if(units == "auto"){
    if(min_time > 1e9){
      units <- "s"
    } else if(min_time > 1e6){
      units <- "ms"
    } else if(min_time > 1e3){
      units <- "us"
    } else {
      units <- "ns"
    }
  }
  if(units == "s"){
    x$timer <- x$timer / 1e9
  } else if (units == "ms") {
    x$timer <- x$timer / 1e6
  } else if (units == "us") {
    x$timer <- x$timer / 1e3
  }
  
  # summarize results
  x <- data.frame("timer" = x$timer, "ticker" = x$ticker)
  df2 <- aggregate(timer~ticker, x, mean)
  colnames(df2)[2] <- "mean"
  df2$sd <- aggregate(timer~ticker, x, sd)$timer
  df2$min <- aggregate(timer~ticker, x, min)$timer
  df2$max <- aggregate(timer~ticker, x, max)$timer
  x$timer <- 1
  df2$neval <- aggregate(timer~ticker, x, sum)$timer

  long_units <- c("seconds", "milliseconds", "microseconds", "nanoseconds")
  short_units <- c("s", "ms", "us", "ns")
  attr(df2, "units") <- long_units[which(short_units == units)]
  df2
}

#' @method print RcppClock
#' @rdname RcppClock
#' @export
print.RcppClock <- function(x){
  df <- summary(x, units = "auto")
  cat("Unit:", attr(df, "units"), "\n")
  print(df, digits = 4, row.names = FALSE)
  invisible(x)
}

#' @export
#' @method plot RcppClock
#' @rdname RcppClock
plot.RcppClock <- function(x) {
  min_time <- min(x$timer[x$timer != 0])
  if(is.na(min_time)) min_time <- 0
  if(min_time > 1e9) {
    units <- "s"
    x$timer <- x$timer / 1e9
  } else if(min_time > 1e6) {
    units <- "ms"
    x$timer <- x$timer / 1e6
  } else if(min_time > 1e3) {
    units <- "us"
    x$timer <- x$timer / 1e3
  } else {
    units <- "ns"
  }

  long_units <- c("seconds", "milliseconds", "microseconds", "nanoseconds")
  short_units <- c("s", "ms", "us", "ns")

  df <- data.frame("timer" = x$timer, "ticker" = x$ticker)

  suppressWarnings(print(ggplot(df, aes(y = ticker, x = timer)) + 
    geom_violin() + 
    geom_jitter(height = 0.1) + 
    theme_classic() + 
    scale_x_continuous(trans = "log10") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(y = "", x = paste0("runtime (", long_units[which(short_units == units)], ")"))))
}
