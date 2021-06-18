test_that("Open file works", {
  expect_silent(
    drilldowndemo_open("details")
  )
})

test_that("Run file works", {
  expect_silent(
    drilldowndemo_run("details")
  )
})

test_that("Non-existent app give error", {
  expect_error(
    drilldowndemo_run("notanapp")
  )
})
