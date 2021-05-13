test_that("`bluebrown_colors()` works", {
  expect_equal(
    as.vector(bluebrown_colors("blue", "brown")),
    c("#5B93AE", "#61381A")
  )
})


test_that("`material_colors()` works", {
  expect_equal(
    as.vector(material_colors("indigo", "lime")),
    c("#3F51B5", "#CDDC39")
  )
})


test_that("`social_colors()` works", {
  expect_identical(
    as.vector(social_colors("dark red", "teal")),
    c("#b92b27", "#00b489")
  )
})

test_that("`see_colors()` works", {
  expect_equal(as.vector(see_colors("indigo", "lime")), c("#303960", "#f7fbe1"))
})

test_that("`flat_colors()` works", {
  expect_equal(as.vector(flat_colors("dark red", "teal")), c("#c0392b", "#16a085"))
})
