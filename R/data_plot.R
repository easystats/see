#' Prepare an object for plotting
#'
#' This function attempts to extract and tranform an object to be further plotted.
#'
#' @param x An object.
#' @param data The original data used to create this object. Can be a statistical model or such.
#' @param ... Arguments passed to or from other methods.
#' @export
data_plot <- function(x, data=NULL, ...){
  UseMethod("data_plot")
}



#' @method print data_plot
#' @importFrom graphics plot
#' @export
print.data_plot <- function(x, ...){
  return(plot(x))
}


#' @keywords internal
.add_plotinfo <- function(x){
  info <- attributes(x)$info
  out <- list(ylab(info$ylab),
              xlab(info$xlab))

  if(!is.null(info$legend_fill)){
    out[[length(out)+1]] <- labs(fill=info$legend_fill)
  }
  if(!is.null(info$legend_color)){
    out[[length(out)+1]] <- labs(color=info$legend_color)
  }

  return(out)
}












#' @keywords internal
.retrieve_data <- function(x){
  # retrieve model
  data <- tryCatch(
    {
      get(attributes(x)$object_name, envir = parent.frame())
    },
    error = function(e) { NULL }
  )

  if (is.null(data)) {
    stop("Failed at retrieving data :( Please provide original model or data through the `data` argument", call. = FALSE)
  }

  data
}