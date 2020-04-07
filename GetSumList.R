Get_Sum_List <- function(data) {
  data_subset <- data[, 5: ncol(data)]
  sum_list <- c()
  for (i in 1:length(data_subset)) {
    sum_list <- c(sum_list, sum(data_subset[, i]))
  }
  return (sum_list)
}