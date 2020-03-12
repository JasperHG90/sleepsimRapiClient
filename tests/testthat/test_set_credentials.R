## Tests for non api functions

context("Setting credentials")

test_that("User can set password, username and host", {
  set_usr_pwd("abcd", "efgh")
  set_host("fakehost")
  expect_equal(Sys.getenv("SLEEPSIMR_API_PASSWORD"), "abcd")
  expect_equal(Sys.getenv("SLEEPSIMR_API_USERNAME"), "efgh")
  expect_equal(Sys.getenv("SLEEPSIMR_MASTER_HOST"), "fakehost")
})
