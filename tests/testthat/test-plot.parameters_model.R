test_that("`plot.see_parameters_model()` works", {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  result <- parameters::model_parameters(m)
  expect_s3_class(plot(result), "gg")

  vdiffr::expect_doppelganger(
    title = "plot.model_parameters_1",
    fig = plot(result)
  )
  vdiffr::expect_doppelganger(
    title = "plot.model_parameters_no_dir",
    fig = plot(result, show_direction = FALSE)
  )
})


test_that("`plot.see_parameters_model()` errors for anova tables", {
  skip_if_not_installed("parameters")
  data(iris)
  m <- aov(Sepal.Length ~ Species, data = iris)
  mp <- parameters::model_parameters(m)
  expect_error(plot(mp), regex = "Plots cannot be created")
})


test_that("`plot.see_parameters_model()` random parameters works", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("parameters")

  data(sleepstudy, package = "lme4")
  s_mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

  out <- parameters::model_parameters(s_mod)
  vdiffr::expect_doppelganger(
    title = "plot.model_parameters_random_0",
    fig = plot(out)
  )

  out <- parameters::model_parameters(s_mod, group_level = TRUE)
  vdiffr::expect_doppelganger(
    title = "plot.model_parameters_random_1",
    fig = plot(out)
  )

  out <- parameters::model_parameters(s_mod, group_level = TRUE)
  vdiffr::expect_doppelganger(
    title = "plot.model_parameters_random_2",
    fig = plot(out, show_labels = TRUE)
  )

  out <- parameters::model_parameters(s_mod, group_level = TRUE)
  vdiffr::expect_doppelganger(
    title = "plot.model_parameters_random_3",
    fig = plot(out, show_labels = TRUE, size_text = 5)
  )

  out <- parameters::model_parameters(s_mod, group_level = TRUE)
  vdiffr::expect_doppelganger(
    title = "plot.model_parameters_random_4",
    fig = plot(out, sort = "ascending", show_labels = TRUE)
  )

  out <- parameters::model_parameters(s_mod, group_level = TRUE)
  vdiffr::expect_doppelganger(
    title = "plot.model_parameters_random_5",
    fig = plot(out, sort = "ascending", show_labels = TRUE, n_columns = 2)
  )

  out <- parameters::model_parameters(s_mod, group_level = TRUE)
  vdiffr::expect_doppelganger(
    title = "plot.model_parameters_random_6",
    fig = plot(
      out,
      sort = "ascending",
      show_labels = TRUE,
      show_intercept = FALSE
    )
  )
})


test_that("`plot.see_parameters_model()` random parameters works", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("parameters")
  data(sleepstudy, package = "lme4")

  set.seed(12345)
  sleepstudy$grp <- sample.int(5, size = 180, replace = TRUE)
  model <- lme4::lmer(
    Reaction ~ Days + (1 | grp) + (1 | Subject),
    data = sleepstudy
  )
  out <- parameters::model_parameters(model, group_level = TRUE)
  vdiffr::expect_doppelganger(
    title = "plot.model_parameters_doublerandom",
    fig = plot(out)
  )
})


test_that("`plot.see_parameters_model()` random parameters works", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("parameters")

  data(mtcars)
  m <- mgcv::gam(mpg ~ s(wt) + cyl + gear + disp, data = mtcars)
  result <- parameters::model_parameters(m)
  vdiffr::expect_doppelganger(
    title = "plot.model_parameters_gam",
    fig = plot(result)
  )
})
