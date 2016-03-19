#' S3 class: tbl_df
#'
#' A data frame tbl wraps a local data frame. The main advantage to using
#' a \code{tbl_df} over a regular data frame is the printing:
#' tbl objects only print a few rows and all the columns that fit on one
#' screen, describing the rest of it as text.
#'
#' @section Methods:
#'
#' \code{tbl_df} implements four important base methods:
#'
#' \describe{
#' \item{print}{By default only prints the first 10 rows (at most 20), and the
#'   columns that fit on screen; see \code{\link{print.tbl_df}}}
#' \item{\code{[}}{Never simplifies (drops), so always returns data.frame}
#' \item{\code{[[}, \code{$}}{Calls \code{\link{.subset2}} directly,
#'   so is considerably faster. Throws error if column does not exist.}
#' }
#' @export
#' @param data a data frame
#' @keywords internal
tbl_df <- function(data) {
  as_data_frame(data)
}

methods::setOldClass(c("tbl_df", "tbl", "data.frame"))

# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_df <- function(x, row.names = NULL, optional = FALSE, ...) {
  structure(x, class = "data.frame")
}

#' @rdname formatting
#' @export
print.tbl_df <- function(x, ..., n = NULL, width = NULL) {
  print(trunc_mat(x, n = n, width = width))
  invisible(x)
}

.check_names_df <- function(x, j){
  if( is.character(j) && any( wrong <- ! j %in% names(x) ) ){
    names <- j[wrong]
    stop( sprintf( "undefined columns: %s", paste(names, collapse = ", " ) ) ) ;
  }
}

#' @export
`[[.tbl_df` <- function(x, i, exact = TRUE) {
  if (is.character(i) && length(i) == 1L && !(i %in% names(x))) {
    stop("Unknown column '", i, "'", call. = FALSE)
  }
  if (!exact) {
    warning("exact ignored", call. = FALSE)
  }

  NextMethod()
}

#' @export
`$.tbl_df` <- function(x, i) {
  if (is.character(i) && !(i %in% names(x))) {
    stop("Unknown column '", i, "'", call. = FALSE)
  }

  .subset2(x, i)
}

#' @export
`[.tbl_df` <- function(x, i, j, drop = FALSE) {
  if (missing(i) && missing(j)) return(x)
  if (drop) warning("drop ignored", call. = FALSE)

  nr <- nrow(x)

  # Escape early if nargs() == 2L; ie, column subsetting
  if (nargs() == 2L) {
    .check_names_df(x,i)
    result <- .subset(x, i)
    attr(result, "row.names") <- .set_row_names(nr)
    return(as_data_frame.data.frame(result))
  }

  # First, subset columns
  if (!missing(j)) {
    .check_names_df(x,j)
    x <- .subset(x, j)
  }

  # Next, subset rows
  if (!missing(i)) {
    if (length(x) == 0) {
      nr <- length(attr(x, "row.names")[i])
    } else {
      x <- lapply(x, `[`, i)
      nr <- length(x[[1]])
    }
  }

  attr(x, "row.names") <- .set_row_names(nr)
  as_data_frame.data.frame(x)
}
