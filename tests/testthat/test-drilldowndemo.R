test_that("Open file works", {
  expect_silent(
    drilldowndemo_open("details")
  )
})

test_that("Run file works", {
  expect_silent(
    drilldowndemo_run("details")
  )

  expect_output(
    drilldowndemo_run(),
    "Available apps"
  )
})

test_that("Non-existent app give error", {
  expect_error(
    drilldowndemo_run("notanapp")
  )
})
