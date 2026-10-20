skip_on_cran()
skip_if_not_installed("rstanarm")
skip_if_not_installed("bayestestR", minimum_version = "0.20.0")
skip_if_not_installed("datawizard")
skip_if_not_installed("ggridges")

set.seed(123)
m_wt <- suppressWarnings(rstanarm::stan_glm(
  mpg ~ wt,
  data = mtcars,
  refresh = 0,
  chains = 1,
  iter = 500
))
m_hp <- suppressWarnings(rstanarm::stan_glm(
  mpg ~ hp,
  data = mtcars,
  refresh = 0,
  chains = 1,
  iter = 500
))

# Functions that make or plot results stand in for user code, so their
# environment is a child of the global environment and not of the test
# environment.
as_user_fn <- function(f) {
  environment(f) <- new.env(parent = globalenv())
  f
}

# Every ggplot that plot() returns must build: the plot itself, each element of
# a returned list, or each plot of a returned patchwork.
expect_draws <- function(p) {
  if (inherits(p, "gg")) {
    plots <- list(p)
    if (inherits(p, "patchwork")) {
      plots <- c(plots, p$patches$plots)
    }
  } else {
    plots <- p
  }
  expect_gt(length(plots), 0)
  for (one in plots) {
    expect_s3_class(one, "gg")
    expect_no_error(ggplot2::ggplot_build(one))
  }
  invisible(p)
}

# evaluates `code` while a global object `name` exists, then restores the
# global environment
with_global <- function(name, value, code) {
  had <- exists(name, envir = globalenv(), inherits = FALSE)
  if (had) {
    old <- get(name, envir = globalenv())
  }
  assign(name, value, envir = globalenv())
  on.exit(
    if (had) {
      assign(name, old, envir = globalenv())
    } else {
      rm(list = name, envir = globalenv())
    },
    add = TRUE
  )
  code
}

# parameters of the prior layer, which is drawn from the model that plot() uses
prior_parameters <- function(p) {
  layer_data <- lapply(p$layers, function(l) l$data)
  layer_data <- Filter(
    function(d) is.data.frame(d) && "Parameter" %in% names(d),
    layer_data
  )
  sort(unique(unlist(lapply(layer_data, function(d) as.character(d$Parameter)))))
}

model_parameters <- function(m) {
  sort(setdiff(insight::find_parameters(m)$conditional, "(Intercept)"))
}

# makes the result inside a user function and returns it unplotted, so that its
# stored name is unreachable afterwards
result_local <- as_user_fn(function(model_value, fun) {
  inner_mod <- model_value
  fun(inner_mod)
})

expect_unreachable <- function(result) {
  name <- attr(result, "object_name", exact = TRUE)
  expect_type(name, "character")
  expect_false(exists(name, envir = globalenv()))
}


# a `data` argument wins over the lookup ----------------------------------------

for (fun_name in c("p_direction", "p_significance", "point_estimate")) {
  test_that(
    sprintf("plot(data = ) works for `%s()` with an unreachable name", fun_name),
    {
      fun <- getExportedValue("bayestestR", fun_name)
      result <- suppressMessages(result_local(m_wt, fun))
      expect_unreachable(result)
      suppressMessages(expect_draws(plot(result, data = m_wt)))
    }
  )

  test_that(
    sprintf("plot(data = ) wins over another model of the stored name for `%s()`", fun_name),
    {
      fun <- getExportedValue("bayestestR", fun_name)
      result <- suppressMessages(result_local(m_wt, fun))
      p <- with_global(
        "inner_mod",
        m_hp,
        suppressMessages(plot(result, data = m_wt, priors = TRUE))
      )
      suppressMessages(expect_draws(p))
      expect_identical(prior_parameters(p), model_parameters(m_wt))
    }
  )

  test_that(
    sprintf("plot(data = <draws>) keeps the priors of the model for `%s()`", fun_name),
    {
      fun <- getExportedValue("bayestestR", fun_name)
      result <- suppressMessages(result_local(m_wt, fun))
      for (draws in list(as.data.frame(m_wt), as.matrix(m_wt))) {
        p <- with_global(
          "inner_mod",
          m_wt,
          suppressMessages(plot(result, data = draws, priors = TRUE))
        )
        suppressMessages(expect_draws(p))
        expect_identical(prior_parameters(p), model_parameters(m_wt))
      }
    }
  )
}


# describe_distribution() on a list -----------------------------------------

test_that("plot() works for `describe_distribution()` on a list", {
  x <- datawizard::describe_distribution(list(mtcars$mpg, mtcars$cyl))
  p <- plot(x)
  expect_type(p, "list")
  expect_length(p, 2)
  expect_draws(p)
})

test_that("plot() works for `describe_distribution()` on a list with negative and missing values", {
  x <- datawizard::describe_distribution(list(
    c(-1.5, 2, -3, 4, -Inf),
    c(1, 2, NA, 5.5, 3)
  ))
  expect_true(grepl("-1.5", paste(attr(x, "object_name"), collapse = " ")))
  p <- suppressWarnings(plot(x))
  expect_length(p, 2)
  suppressWarnings(expect_draws(p))
})

test_that("plot() works for `describe_distribution()` on a long list in a function", {
  set.seed(123)
  x <- datawizard::describe_distribution(list(rnorm(1000), rnorm(1000)))
  # the deparsed list is stored as several strings
  expect_gt(length(attr(x, "object_name")), 1)
  p <- as_user_fn(function(result) plot(result))(x)
  expect_length(p, 2)
  expect_draws(p)
})

test_that("a stored name with other calls than list() and c() is not evaluated", {
  x <- datawizard::describe_distribution(list(mtcars$mpg, mtcars$cyl))
  on.exit(
    if (exists("hit", envir = globalenv(), inherits = FALSE)) {
      rm("hit", envir = globalenv())
    },
    add = TRUE
  )
  for (name in c(
    "stop(\"boom\")",
    "list(stop(\"boom\"))",
    "list(c(stop(\"boom\")))",
    "list(assign(\"hit\", TRUE, envir = globalenv()))"
  )) {
    attr(x, "object_name") <- name
    expect_error(plot(x), "Failed at retrieving data")
  }
  expect_false(exists("hit", envir = globalenv(), inherits = FALSE))
})


# `data` arguments for estimate_density() and describe_distribution() --------

test_that("plot(data = ) works for `estimate_density()` with an unreachable name", {
  result <- result_local(m_wt, bayestestR::estimate_density)
  expect_unreachable(result)
  suppressMessages(expect_draws(plot(result, data = m_wt)))
})

test_that("plot(data = ) works for `describe_distribution()` with an unreachable name", {
  result_local_df <- as_user_fn(function(df_value) {
    inner_df <- df_value
    datawizard::describe_distribution(inner_df)
  })
  df <- mtcars[c("mpg", "cyl")]
  result <- result_local_df(df)
  expect_unreachable(result)
  expect_draws(plot(result, data = df))
})


# a model or data that exists only inside a function --------------------------

# makes the result and plots it inside a user function, which holds the model
plot_local <- as_user_fn(function(model_value, fun, ...) {
  inner_mod <- model_value
  result <- fun(inner_mod)
  list(result = result, plot = plot(result, ...))
})

for (fun_name in c("p_direction", "p_significance", "rope", "hdi", "point_estimate")) {
  test_that(
    sprintf("plot() finds a function-local model for `%s()`", fun_name),
    {
      fun <- getExportedValue("bayestestR", fun_name)
      out <- suppressMessages(plot_local(m_wt, fun))
      expect_identical(attr(out$result, "object_name"), "inner_mod")
      # not in the test environment, the global environment, or the search path
      expect_false(exists("inner_mod"))
      suppressMessages(expect_draws(out$plot))
    }
  )
}

test_that("plot() finds a function-local model for the priors of `p_direction()`", {
  out <- suppressMessages(plot_local(m_wt, bayestestR::p_direction, priors = TRUE))
  expect_false(exists("inner_mod"))
  suppressMessages(expect_draws(out$plot))
  expect_identical(prior_parameters(out$plot), model_parameters(m_wt))
})

test_that("plot() finds a function-local data frame for `describe_distribution()`", {
  plot_local_df <- as_user_fn(function(df_value) {
    inner_df <- df_value
    result <- datawizard::describe_distribution(inner_df)
    list(result = result, plot = plot(result))
  })
  out <- plot_local_df(mtcars[c("mpg", "cyl")])
  expect_identical(attr(out$result, "object_name"), "inner_df")
  expect_false(exists("inner_df"))
  expect_draws(out$plot)
})

test_that("a missing argument of the stored name gives the usual lookup error", {
  result_m <- as_user_fn(function(model_value) {
    m <- model_value
    bayestestR::p_direction(m)
  })
  wrap <- as_user_fn(function(result, m) plot(result))
  result <- result_m(m_wt)
  expect_identical(attr(result, "object_name"), "m")
  # other test files leave a global `m`
  if (exists("m", envir = globalenv(), inherits = FALSE)) {
    old_m <- get("m", envir = globalenv())
    rm("m", envir = globalenv())
    on.exit(assign("m", old_m, envir = globalenv()), add = TRUE)
  }
  expect_false(exists("m"))
  expect_error(wrap(result), "Failed at retrieving data")
})

test_that("plot() finds a function-local model past a wrapper with a missing argument of its name", {
  outer <- as_user_fn(function(model_value) {
    wrap <- function(result, m) plot(result)
    m <- model_value
    wrap(bayestestR::p_direction(m))
  })
  if (exists("m", envir = globalenv(), inherits = FALSE)) {
    old_m <- get("m", envir = globalenv())
    rm("m", envir = globalenv())
    on.exit(assign("m", old_m, envir = globalenv()), add = TRUE)
  }
  expect_false(exists("m"))
  suppressMessages(expect_draws(outer(m_wt)))
})

test_that("plot() finds a function-local model named like a local of tryCatch()", {
  # a tryCatch() around the lookup used to search its own locals, such as
  # `name` and `handler`
  plot_name <- as_user_fn(function(model_value) {
    name <- model_value
    plot(bayestestR::p_direction(name))
  })
  plot_handler <- as_user_fn(function(model_value) {
    handler <- model_value
    plot(bayestestR::p_direction(handler))
  })
  expect_false(exists("name"))
  expect_false(exists("handler"))
  suppressMessages(expect_draws(plot_name(m_wt)))
  suppressMessages(expect_draws(plot_handler(m_wt)))
})

test_that("plot() uses the model stored in a check_normality() result inside a function", {
  skip_if_not_installed("performance")
  # `obj_name` is a local of the lookup, and `name` was a local of the
  # tryCatch() that wrapped it
  plot_name <- as_user_fn(function(model_value) {
    name <- model_value
    plot(performance::check_normality(name))
  })
  plot_obj_name <- as_user_fn(function(model_value) {
    obj_name <- model_value
    plot(performance::check_normality(obj_name))
  })
  expect_false(exists("name"))
  expect_false(exists("obj_name"))
  model <- lm(mpg ~ wt, data = mtcars)
  expect_draws(plot_name(model))
  expect_draws(plot_obj_name(model))
})

test_that("plot() still fails for a function-local model named `x`, `data` or `obj_name`", {
  plot_x <- as_user_fn(function(model_value) {
    x <- model_value
    plot(bayestestR::p_direction(x))
  })
  plot_data <- as_user_fn(function(model_value) {
    data <- model_value
    plot(bayestestR::p_direction(data))
  })
  plot_obj_name <- as_user_fn(function(model_value) {
    obj_name <- model_value
    plot(bayestestR::p_direction(obj_name))
  })
  # the lookup finds `x` in see's own functions, `data` as `utils::data()`,
  # and `obj_name` in its own frame, not in the user's function
  expect_error(suppressMessages(plot_x(m_wt)), "undefined columns selected")
  expect_error(suppressMessages(plot_data(m_wt)), "argument is of length zero")
  expect_error(suppressMessages(plot_obj_name(m_wt)), "must be numeric")
})
