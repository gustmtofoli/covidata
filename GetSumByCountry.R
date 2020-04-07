Get_Sum_By_Country <- function(file, country) {
  data <- subset(file, file$Country.Region == country)
  return (sum(data[ncol(data)]))
}