skip_on_cran()
skip_if_not_installed("rstanarm")
skip_if_not_installed("bayestestR")
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

# Functions that call plot() stand in for user code, so their environment is a
# child of the global environment and not of the test environment.
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

plotted_parameters <- function(p) {
  sort(unique(as.character(p$data$y)))
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

# makes the result inside a user function from a local variable, then plots it
plot_local <- as_user_fn(function(model_value, fun, ...) {
  inner_mod <- model_value
  result <- fun(inner_mod)
  list(result = result, plot = plot(result, ...))
})

# makes the result inside a user function and returns it unplotted, so that its
# stored name is unreachable afterwards
result_local <- as_user_fn(function(model_value, fun) {
  inner_mod <- model_value
  fun(inner_mod)
})

local_named <- function(name) {
  sym <- as.name(name)
  f <- eval(bquote(function(model_value) {
    .(sym) <- model_value
    result <- bayestestR::p_direction(.(sym))
    list(result = result, plot = plot(result))
  }))
  as_user_fn(f)
}

expect_unreachable <- function(result) {
  name <- attr(result, "object_name", exact = TRUE)
  expect_type(name, "character")
  expect_false(exists(name, envir = globalenv()))
}


# a local variable of the function that calls plot() --------------------------

for (fun_name in c(
  "p_direction",
  "p_significance",
  "rope",
  "hdi",
  "point_estimate"
)) {
  test_that(
    sprintf("plot() finds a function-local model for `%s()`", fun_name),
    {
      fun <- getExportedValue("bayestestR", fun_name)
      out <- suppressMessages(suppressWarnings(plot_local(m_wt, fun)))
      expect_identical(attr(out$result, "object_name"), "inner_mod")
      expect_unreachable(out$result)
      suppressMessages(expect_draws(out$plot))
    }
  )
}

test_that("plot() finds a function-local data frame for `describe_distribution()`", {
  plot_local_df <- as_user_fn(function(df_value) {
    inner_df <- df_value
    result <- datawizard::describe_distribution(inner_df)
    list(result = result, plot = plot(result))
  })
  out <- plot_local_df(mtcars[c("mpg", "cyl")])
  expect_identical(attr(out$result, "object_name"), "inner_df")
  expect_unreachable(out$result)
  expect_draws(out$plot)
})


test_that("plot() finds a model made in local() or eval() in user code", {
  plot_in_local <- as_user_fn(function(model_value) {
    local({
      local_mod <- model_value
      plot(bayestestR::p_direction(local_mod))
    })
  })
  expect_draws(plot_in_local(m_wt))

  plot_in_eval <- as_user_fn(function(model_value) {
    env <- new.env()
    assign("eval_mod", model_value, envir = env)
    eval(quote(plot(bayestestR::p_direction(eval_mod))), envir = env)
  })
  expect_draws(plot_in_eval(m_wt))
})


# local model before a global one of the same name ------------------------------

test_that("plot() prefers the function-local model over a global one of the same name", {
  expect_length(intersect(model_parameters(m_wt), model_parameters(m_hp)), 0)
  assign("inner_mod", m_hp, envir = globalenv())
  withr::defer(rm("inner_mod", envir = globalenv()))

  out <- plot_local(m_wt, bayestestR::p_direction)
  expect_draws(out$plot)
  expect_identical(plotted_parameters(out$plot), model_parameters(m_wt))
})


# local names that see's own functions also use ---------------------------------

for (name in c("x", "data", "model")) {
  test_that(sprintf("plot() finds a function-local model named `%s`", name), {
    out <- local_named(name)(m_wt)
    expect_identical(attr(out$result, "object_name"), name)
    expect_draws(out$plot)
    expect_identical(plotted_parameters(out$plot), model_parameters(m_wt))
  })
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
      assign("inner_mod", m_hp, envir = globalenv())
      withr::defer(rm("inner_mod", envir = globalenv()))
      p <- suppressMessages(plot(result, data = m_wt, priors = TRUE))
      suppressMessages(expect_draws(p))
      expect_identical(prior_parameters(p), model_parameters(m_wt))
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

test_that("a stored name with other calls than list() and c() is not evaluated", {
  x <- datawizard::describe_distribution(list(mtcars$mpg, mtcars$cyl))
  withr::defer(
    if (exists("hit", envir = globalenv(), inherits = FALSE)) {
      rm("hit", envir = globalenv())
    }
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
