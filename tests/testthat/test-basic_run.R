context("Test simple examples")

Sys.setenv(TZ="UTC")
test_that("DynACof runs for full sun", {
  # ref= DynACof(Period= as.POSIXct(c("1979-01-01", "1980-01-02")))
  # DynACof::write.results(ref,Outpath = "tests/testthat",Simulation_Name = "ref_sim")
  ref_sim= readRDS("ref_sim.RData")
  tested= DynACof(Period= as.POSIXct(c("1979-01-01", "1980-01-02")))
  expect_equal(object = tested$Sim, expected = ref_sim$Sim)
})
