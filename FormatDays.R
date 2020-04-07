Format_Days <- function(data) {
  days <- colnames(data)[5:ncol(data)]
  days_formated <- c()
  for (label in days) {
    if (nchar(label) < 8) {
      label <- gsub("1.20", "01.20", label)
      label <- gsub("2.20", "02.20", label)
      label <- gsub("3.20", "03.20", label)
      label <- gsub("4.20", "04.20", label)
      label <- gsub("5.20", "05.20", label)
      label <- gsub("6.20", "06.20", label)
      label <- gsub("7.20", "07.20", label)
      label <- gsub("8.20", "08.20", label)
      label <- gsub("9.20", "09.20", label)
    }
    days_formated <- c(days_formated, label)
  }
  return (days_formated)
}