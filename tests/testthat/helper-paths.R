extdata_file <- function(filename) {
  system.file("extdata", filename, package = "hazgrid", mustWork = TRUE)
}

demo_file <- function(filename) {
  system.file("extdata", "demo", filename, package = "hazgrid", mustWork = TRUE)
}
