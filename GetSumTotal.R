Get_Sum_Total <- function(data) {
  data <- na.omit(data[, ncol(data)])
  return (sum(data))
}