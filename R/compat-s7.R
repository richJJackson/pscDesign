# nocov start

ggplot_dollar_override <- function(x, name) {
  nm <- if (is.character(name)) name else as.character(name)

  if (inherits(x, "S7_object") && requireNamespace("S7", quietly = TRUE)) {
    return(tryCatch(S7::prop(x, nm), error = function(...) NULL))
  }

  if (typeof(x) %in% c("list", "environment")) {
    NextMethod()
  } else {
    msg <- sprintf("Can't get S7 properties with `$`. Did you mean `.@%s`?", nm)
    stop(msg, call. = FALSE)
  }
}

ggplot_dollar_assign_override <- function(x, name, value) {
  nm <- if (is.character(name)) name else as.character(name)

  if (inherits(x, "S7_object") && requireNamespace("S7", quietly = TRUE)) {
    current <- tryCatch(S7::prop(x, nm), error = function(...) NULL)
    if (!is.null(current) && nm == "labels" && is.list(value)) {
      current[names(value)] <- value
      value <- current
    }
    out <- tryCatch({
      S7::prop(x, nm) <- value
      x
    }, error = function(...) NULL)
    if (!is.null(out)) {
      return(out)
    }
  }

  if (typeof(x) %in% c("list", "environment")) {
    NextMethod()
  } else {
    msg <- sprintf("Can't set S7 properties with `$<-`. Did you mean `.@%s <- value`?",
                   nm)
    stop(msg, call. = FALSE)
  }
}

.onLoad <- function(libname, pkgname) {
  base::registerS3method("$", "ggplot", ggplot_dollar_override)
  base::registerS3method("$", "S7_object", ggplot_dollar_override)
  base::registerS3method("$<-", "ggplot", ggplot_dollar_assign_override)
  base::registerS3method("$<-", "S7_object", ggplot_dollar_assign_override)
}

# nocov end
