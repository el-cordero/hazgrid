extdata_file <- function(filename) {
  system.file("extdata", filename, package = "hazgrid", mustWork = TRUE)
}
