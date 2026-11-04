# Return a fallback when an optional input is absent.
# Author: Yaoxiang Li
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
