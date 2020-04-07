Download_Covid_Data <- function(url, output_file_name) {
  dest_file <- paste0("~/", output_file_name)
  write.csv(data.frame(), output_file_name)
  download.file(url, dest_file)
  data <- read.csv(url, header = TRUE, sep = ",")
  return (data)
}