#' @importFrom stats reshape
#' @keywords internal
.reshape_to_long <- function(x, names_to = "group", values_to = "values", columns = colnames(x)) {
  if (is.numeric(columns)) columns <- colnames(x)[columns]
  dat <- stats::reshape(
    x,
    idvar = "id",
    ids = row.names(x),
    times = columns,
    timevar = names_to,
    v.names = values_to,
    varying = list(columns),
    direction = "long"
  )

  if (is.factor(dat[[values_to]]))
    dat[[values_to]] <- as.character(dat[[values_to]])

  dat[, 1:(ncol(dat) - 1), drop = FALSE]
}
