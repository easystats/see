skip_on_cran()
skip_if_not_installed("vdiffr")

# Snapshot tests for check_model() plots
# These tests verify that diagnostic plots render consistently across versions.
# Related to #236 (snapshot tests) and #420 (large dataset performance).

# Helper function for reproducible vdiffr tests
expect_doppelganger_with_seed <- function(title, fig, seed = 123) {
  set.seed(seed)
  vdiffr::expect_doppelganger(title = title, fig = fig)
}

test_that("plot.see_check_model() renders correctly", {
  skip_if_not_installed("performance")
  skip_if_not_installed("DHARMa")
  skip_if_not_installed("glmmTMB")

  set.seed(123)

  data(Salamanders, package = "glmmTMB")
  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = poisson,
    data = Salamanders
  )
  expect_doppelganger_with_seed(
    title = "check_overdispersion_poisson",
    fig = plot(performance::check_overdispersion(m))
  )
  expect_doppelganger_with_seed(
    title = "check_overdispersion_poisson-2",
    fig = plot(performance::check_overdispersion(m), type = 2)
  )

  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::nbinom1(),
    data = Salamanders
  )
  expect_doppelganger_with_seed(
    title = "check_overdispersion_nbinom1",
    fig = plot(performance::check_overdispersion(m))
  )
  expect_doppelganger_with_seed(
    title = "check_overdispersion_nbibom1-2",
    fig = plot(performance::check_overdispersion(m), type = 2)
  )

  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::nbinom2(),
    data = Salamanders
  )
  expect_doppelganger_with_seed(
    title = "check_overdispersion_nbinom2",
    fig = plot(performance::check_overdispersion(m))
  )
  expect_doppelganger_with_seed(
    title = "check_overdispersion_nbinom2-2",
    fig = plot(performance::check_overdispersion(m), type = 2)
  )

  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    zi = ~mined,
    family = poisson,
    data = Salamanders
  )
  expect_doppelganger_with_seed(
    title = "check_overdispersion_poisson_zi",
    fig = plot(performance::check_overdispersion(m))
  )
  expect_doppelganger_with_seed(
    title = "check_overdispersion_poisson_zi-2",
    fig = plot(performance::check_overdispersion(m), type = 2)
  )

  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    zi = ~mined,
    family = glmmTMB::nbinom1(),
    data = Salamanders
  )
  expect_doppelganger_with_seed(
    title = "check_overdispersion_nbinom1_zi",
    fig = plot(performance::check_overdispersion(m))
  )
  expect_doppelganger_with_seed(
    title = "check_overdispersion_nbinom1_zi-2",
    fig = plot(performance::check_overdispersion(m), type = 2)
  )

  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    zi = ~mined,
    family = glmmTMB::nbinom2(),
    data = Salamanders
  )
  expect_doppelganger_with_seed(
    title = "check_overdispersion_nbinom2_zi",
    fig = plot(performance::check_overdispersion(m))
  )
  expect_doppelganger_with_seed(
    title = "check_overdispersion_nbinom2_zi-2",
    fig = plot(performance::check_overdispersion(m), type = 2)
  )
})
