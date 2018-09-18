#' @include tibble.R
#'
#' @description
#'
#' `lst()` is similar to [list()], but like `tibble()`, it
#' evaluates its arguments lazily and in order, and automatically adds names.
#'
#' `lst_()` uses lazy evaluation and is deprecated. New code should use `lst()`
#' with [quasiquotation].
#'
#' @export
#' @examples
#' lst(n = 5, x = runif(n))
#'
#' # You can splice-unquote a list of quotes and formulas
#' lst(!!! list(n = rlang::quo(2 + 3), y = quote(runif(n))))
#'
#' @export
#' @rdname tibble
lst <- function(...) {
  lst_impl(quos(...))
}

lst_quos <- function(xs, recycle = FALSE) {
  n <- length(xs)
  if (n == 0) {
    return(list())
  }

  nms <- names2(inputs)
  result <- new_list(n, nms)

  # Create mask in which we'll assign each result so expressions can
  # refer to previous definitions
  mask <- new_data_mask(new_environment())

  # Used for creating rectangular outputs
  mask_nrows(mask) <- NULL

  # Evaluate each column in turn
  for (i in seq_len(n)) {
    value <- eval_tidy(inputs[[i]], mask)
    name <- nms[[i]]

    if (!is_null(value)) {
      result[[i]] <- value
      if (recycle) {
        results <- mask_recycle(mask, result, value, name)
      }
    }

    mask[[name]] <- value
  }

  result
}

mask_nrows <- function(mask) {
  mask[[".__tibble_nrows__."]]
}
`mask_nrows<-` <- function(mask, value) {
  mask[[".__tibble_nrows__."]] <- value
  mask
}

# Must update `result` as well!

mask_recycle <- function(mask, result, value, name) {
  nrows <- mask_nrows(mask)

  if (is_null(nrows)) {
    n <- NROW(value)
    if (n != 1L) {
      result <- mask_recycle_first(mask, result, n)
    }

    return(result)
  }

  browser()

  mask[[nms[[i]]]] <- recycle(res, length(result[[1]]))

  NULL
}

mask_ensure_recycled <- function(mask, result, n) {
  mask_nrows(mask) <- n

  for (i in env_names(mask)) {
    mask[[i]] <- recycle(mask[[i]], n)
  }

  map(result, recycle, n)
}

recycle <- function(x, to) {
  nrows <- NROW(x)

  if (nrows == to) {
    return(x)
  }

  dims <- dim(x)
  ones <- rep(1L, to)

  if (length(dims)) {
    return(x[ones, ])
  }

  if (nrows == 1L) {
    return(x[rep(1L, to)])
  }

  x
}

recycle_lst <- function(output, i) {
  idx_to_fix <- integer()
  if (i > 1L) {
    if (NROW(output[[i]]) == 1L && NROW(output[[1L]]) != 1L) {
      idx_to_fix <- i
      idx_boilerplate <- 1L
    } else if (NROW(output[[i]]) != 1L && NROW(output[[1L]]) == 1L) {
      idx_to_fix <- seq2(1L, i - 1L)
      idx_boilerplate <- i
    }
  }

  if (length(idx_to_fix) > 0L) {
    output[idx_to_fix] <- recycle_vecs(output[idx_to_fix], length(output[[idx_boilerplate]]))
  }

  output
}

recycle_vecs <- function(x, length) {
  ones <- rep(1L, length)
  map(x, `[`, ones)
}

#' @export
#' @usage NULL
#' @rdname tibble
lst_ <- function(xs) {
  xs <- compat_lazy_dots(xs, caller_env())
  lst(!!!xs)
}
