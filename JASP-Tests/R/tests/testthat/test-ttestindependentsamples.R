context("Independent Samples TTest")

# does not test
# - missing values exclusion
# - error handling of plots

test_that("Main table results match", {
  options <- jasptools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$welchs <- TRUE
  options$mannWhitneyU <- TRUE
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["ttest"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", "Student", 98, 0.448976320466698, 0.163364220743842,
         0.15401876311258, -0.263105943067512, 0.589834384555196, 0,
         0, 0.214904085649005, 0.760172707980336, 1, "contNormal", "Welch",
         93.4114683704755, 0.441326472332004, 0.163364220743842, 0.155340050635411,
         -0.256150680877671, 0.582879122365355, 0, 0, 0.211269449004155,
         0.773250564688269, 1, "contNormal", "Mann-Whitney", "", 0.617539087467476,
         0.0932984248674163, 0.0591133004926108, -0.269303374938764,
         0.553341045241524, -0.169577908162339, 0.281763520076616, "",
         1290, 1)
  )
})

test_that("Normality table matches", {
  options <- jasptools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$normalityTests <- TRUE
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["assumptionChecks"]][["shapiroWilk"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 0, 0.933547444665698, 0.00342000811150064, "TRUE",
         "contNormal", 1, 0.972586424088514, 0.401705854633909, "FALSE")
  )
})

test_that("Equality of variances table matches", {
  options <- jasptools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$equalityOfVariancesTests<- TRUE
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["assumptionChecks"]][["levene"]][["data"]]
  expect_equal_tables(table, list("contNormal", 0.474760708390762, 1, 0.492433247088434))
})

test_that("Descriptives table matches", {
  options <- jasptools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptives <- TRUE
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["descriptives"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 0, 58, -0.120135614827586, 1.10575982846952, 0.145193378675912,
         "TRUE", "contNormal", 1, 42, -0.283499835571429, 0.994612407217046,
         0.15347202634745)
  )
})
#
# test_that("Descriptives plot matches", {
#   options <- jasptools::analysisOptions("TTestIndependentSamples")
#   options$variables <- "contNormal"
#   options$groupingVariable <- "contBinom"
#   options$descriptivesPlots <- TRUE
#   results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "descriptives", dir="TTestIndependentSamples")
# })

test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("TTestIndependentSamples")

  options$variables <- "debInf"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- "debSame"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$variables <- "debMiss99"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")

  options$dependent <- "contNormal"
  options$groupingVariable <- "debSame"
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  msg <- results[["results"]][["errorMessage"]]
  expect_true(any(grepl("levels", msg, ignore.case=TRUE)), label = "1-level factor check")
})

# Below are the unit tests for Andy Field's book

# Chapter 4
test_that("Fields Book - Chapter 4 results match", {
  options <- jasptools::analysisOptions("TTestIndependentSamples")
  dataset <- rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_04/www/Puppies Dummy.sav")
  datasetC12 <- dataset[dataset$Dose != 3, ]
  datasetC13 <- dataset[dataset$Dose != 2, ]
  datasetC23 <- dataset[dataset$Dose != 1, ]
  options$variables <- "Happiness"
  options$groupingVariable <- "Dose"
  options$effectSize <- TRUE
  options$effectSizeSD <- "effectSizeSDGroup1"
  resultsPuppiesCohensD30Control <- jasptools::run("TTestIndependentSamples", datasetC13, options, view=FALSE, quiet=TRUE)
  resultsPuppiesCohensD15Control <- jasptools::run("TTestIndependentSamples", datasetC12, options, view=FALSE, quiet=TRUE)
  options$effectSizeSD <- "effectSizeSDPooled"
  resultsPuppiesCohensD3015 <- jasptools::run("TTestIndependentSamples", datasetC23, options, view=FALSE, quiet=TRUE)
  
  resultsCohensD <- list(resultsPuppiesCohensD30Control[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]],
                         resultsPuppiesCohensD15Control[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]],
                         resultsPuppiesCohensD3015[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]])
  expect_equal_tables(tableOutput2,
                      list(-2.147502, -0.766965, -1.242118))

  options <- jasptools::analysisOptions("TTestIndependentSamples")
  dataset <- rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_04/www/Superhero.sav")
  datasetC12 <- dataset[dataset$hero == 1 | dataset$hero == 2, ]
  datasetC13 <- dataset[dataset$hero == 1 | dataset$hero == 3, ]
  datasetC14 <- dataset[dataset$hero == 1 | dataset$hero == 4, ]
  datasetC23 <- dataset[dataset$hero == 2 | dataset$hero == 3, ]
  datasetC24 <- dataset[dataset$hero == 2 | dataset$hero == 4, ]
  datasetC34 <- dataset[dataset$hero == 3 | dataset$hero == 4, ]
  options$variables <- "injury"
  options$groupingVariable <- "hero"
  options$effectSize <- TRUE
  options$effectSizeSD <- "effectSizeSDGroup2"
  resultsHeroCohensDConSuperSpider <- jasptools::run("TTestIndependentSamples", datasetC12, options, view=FALSE, quiet=TRUE)
  resultsHeroCohensDConSuperHulk <- jasptools::run("TTestIndependentSamples", datasetC13, options, view=FALSE, quiet=TRUE)
  resultsHeroCohensDConSuperNinja <- jasptools::run("TTestIndependentSamples", datasetC14, options, view=FALSE, quiet=TRUE)
  resultsHeroCohensDConSpiderHulk <- jasptools::run("TTestIndependentSamples", datasetC23, options, view=FALSE, quiet=TRUE)
  resultsHeroCohensDConSpiderNinja <- jasptools::run("TTestIndependentSamples", datasetC24, options, view=FALSE, quiet=TRUE)
  resultsHeroCohensDConHulkNinja <- jasptools::run("TTestIndependentSamples", datasetC34, options, view=FALSE, quiet=TRUE)
  resultsCohensDCon <- list(resultsHeroCohensDConSuperSpider[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]],
                            resultsHeroCohensDConSuperHulk[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]],
                            resultsHeroCohensDConSuperNinja[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]],
                            resultsHeroCohensDConSpiderHulk[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]],
                            resultsHeroCohensDConSpiderNinja[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]],
                            resultsHeroCohensDConHulkNinja[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]])
  expect_equal_tables(resultsCohensDCon,
                      list(1.532004, 1.864822, 4.179566, 0.4669839, 1.885403, 1.118979)
  )
  
  options$effectSizeSD <- "effectSizeSDPooled"
  resultsHeroCohensDPooSuperSpider <- jasptools::run("TTestIndependentSamples", datasetC12, options, view=FALSE, quiet=TRUE)
  resultsHeroCohensDPooSuperHulk <- jasptools::run("TTestIndependentSamples", datasetC13, options, view=FALSE, quiet=TRUE)
  resultsHeroCohensDPooSuperNinja <- jasptools::run("TTestIndependentSamples", datasetC14, options, view=FALSE, quiet=TRUE)
  resultsHeroCohensDPooSpiderHulk <- jasptools::run("TTestIndependentSamples", datasetC23, options, view=FALSE, quiet=TRUE)
  resultsHeroCohensDPooSpiderNinja <- jasptools::run("TTestIndependentSamples", datasetC24, options, view=FALSE, quiet=TRUE)
  resultsHeroCohensDPooHulkNinja <- jasptools::run("TTestIndependentSamples", datasetC34, options, view=FALSE, quiet=TRUE)
  resultsCohensDPoo <- list(resultsHeroCohensDPooSuperSpider[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]],
                            resultsHeroCohensDPooSuperHulk[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]],
                            resultsHeroCohensDPooSuperNinja[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]],
                            resultsHeroCohensDPooSpiderHulk[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]],
                            resultsHeroCohensDPooSpiderNinja[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]],
                            resultsHeroCohensDPooHulkNinja[["results"]][["ttest.tables"]][["ttestParametric"]][["data"]][[1]][["cohensd"]])
  expect_equal_tables(resultsCohensDPoo,
                      list(1.261983, 1.620304, 2.602089, 0.4878571, 1.480746, 0.8234014)
  )                    
})
  