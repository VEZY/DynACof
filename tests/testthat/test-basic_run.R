context("Test simple examples")

Sys.setenv(TZ="UTC")
test_that("DynACof example works", {
  # DynACof::write.results(DynACof(Period= as.POSIXct(c("1979-01-01", "1980-01-02")),
  #                         Outpath = "tests/testthat")
  ref_sim= readRDS("ref_sim.RData")
  tested= DynACof(Period= as.POSIXct(c("1979-01-01", "1980-01-02")))
  expect_identical(object = tested$Sim, expected = ref_sim$Sim)
})
