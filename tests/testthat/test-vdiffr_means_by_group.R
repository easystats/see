test_that("datawizard means_by_group rendered correctly", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`plot.means_by_group()` works - 1",
    fig = plot(
      datawizard::means_by_group(
        iris$Sepal.Width,
        iris$Species
      )
    )
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`plot.means_by_group()` works - 2",
    fig = plot(
      datawizard::means_by_group(
        iris$Sepal.Width,
        iris$Species
      ),
      title = "group means",
      ci = FALSE,
      caption = FALSE
    )
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`plot.means_by_group()` works - 3",
    fig = plot(
      datawizard::means_by_group(
        iris,
        c("Sepal.Width", "Petal.Width"),
        "Species"
      )
    )
  )
})
