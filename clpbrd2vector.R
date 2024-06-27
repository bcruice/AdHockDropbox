# excel column to vector
clpbrd2vector <- function(collapse = ", "){
  x <- readClipboard()
  x <- paste(x,collapse = collapse)
  return(x)
}
