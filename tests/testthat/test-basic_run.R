context("Test simple examples")

Sys.setenv(TZ="UTC")
test_that("DynACof runs for full sun", {
  # ref= DynACof(Period= as.POSIXct(c("1979-01-01", "1980-01-02")))
  # DynACof::write.results(ref,Outpath = "tests/testthat",Simulation_Name = "ref_sim")
  ref_sim= readRDS("ref_sim.RData")
  tested= DynACof(Period= as.POSIXct(c("1979-01-01", "1980-01-02")))
  expect_equal(object = tested$Sim, expected = ref_sim$Sim)
})

test_that("Test parameters", {
  test_parameters(Parameters= Import_Parameters())
  params= Import_Parameters()
  params$REWc= NULL
  expect_error(test_parameters(Parameters= params),
               "Missing parameter in soil parameter file:  REWc")

})
