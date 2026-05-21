skip_if_not_installed("vdiffr")
skip_if_not_installed("discovr")
skip_if_not_installed("performance")
skip_if_not_installed("qqplotr")

test_that("check_model, chapter 8.12", {
  m <- lm(sales ~ adverts + airplay + image, data = discovr::album_sales)
  set.seed(123)
  out <- performance::check_model(m)
  vdiffr::expect_doppelganger(
    title = "plot.check_model_discovr_8.12",
    fig = plot(out)
  )
})
