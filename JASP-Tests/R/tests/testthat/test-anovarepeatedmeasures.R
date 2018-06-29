context("Repeated Measures ANOVA")

# Does not test:
#    - type I and type II sum of squares
#    - Simple effects
#    - Plots
#    - Contrasts apart from 'repeated'

initOpts <- function(){
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  
  options$repeatedMeasuresFactors <- list(
    list(name = "Drink", levels = c("Beer", "Wine", "Water")),
    list(name = "Imagery", levels = c("Positive", "Neutral", "Negative"))
  )
  
  options$repeatedMeasuresCells <- c("beerpos", "beerneut", "beerneg",
                                     "winepos", "wineneut", "wineneg",
                                     "waterpos", "waterneu", "waterneg")
  options$withinModelTerms <- list(
    list(components = "Drink"),
    list(components = "Imagery"), 
    list(components = c("Drink", "Imagery"))
  )
  
  options
}

test_that("Within subjects table results match", {
  options <- initOpts()
  options$sphericityCorrections <- TRUE

  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options, view = FALSE, quiet = TRUE)
  refTable <- list("Drink", "None", 2092.34444444444, 2, 1046.17222222222, 5.10598105687077,
                0.0108629307294978, "TRUE", "FALSE", 1, 1, 1, 1, 1, "Drink",
                "Greenhouse-Geisser", 2092.34444444444, 1.15422864073086, 1812.76427443316,
                5.10598105687077, 0.0297686804863521, "FALSE", "FALSE", 1, 1,
                1, 1, 1, "Drink", "Huynh-Feldt", 2092.34444444444, 1.18148845405137,
                1770.93939197604, 5.10598105687077, 0.028813909529067, "FALSE",
                "FALSE", 1, 1, 1, 1, 1, "Residual", "None", 7785.87777777778,
                38, 204.891520467836, "", "", "", "", "", "TRUE", "Residual",
                "Greenhouse-Geisser", 7785.87777777778, 21.9303441738863, 355.027614525488,
                "", "", "", "", "", "FALSE", "Residual", "Huynh-Feldt", 7785.87777777778,
                22.4482806269761, 346.836263638895, "", "", "", "", "", "FALSE",
                "Imagery", "None", 21628.6777777778, 2, 10814.3388888889, 122.564824909945,
                2.68019659683571e-17, "TRUE", "FALSE", 1, 1, 1, 1, 1, "Imagery",
                "Greenhouse-Geisser", 21628.6777777778, 1.49488144635967, 14468.4903478118,
                122.564824909945, 1.75728558571484e-13, "FALSE", "FALSE", 1,
                1, 1, 1, 1, "Imagery", "Huynh-Feldt", 21628.6777777778, 1.59368408969683,
                13571.4963320568, 122.564824909945, 3.14280380271786e-14, "FALSE",
                "FALSE", 1, 1, 1, 1, 1, "Residual", "None", 3352.87777777778,
                38, 88.2336257309941, "", "", "", "", "", "TRUE", "Residual",
                "Greenhouse-Geisser", 3352.87777777778, 28.4027474808338, 118.047656482539,
                "", "", "", "", "", "FALSE", "Residual", "Huynh-Feldt", 3352.87777777778,
                30.2799977042398, 110.729129193702, "", "", "", "", "", "FALSE",
                "Drink <unicode> Imagery", "None", 2624.42222222222, 4, 656.105555555556,
                17.1549223629789, 4.58904028152479e-10, "TRUE", "FALSE", "Drink <unicode> Imagery",
                "Greenhouse-Geisser", 2624.42222222222, 3.19359175963514, 821.777615847198,
                17.1549223629789, 1.90024850184092e-08, "FALSE", "FALSE", "Drink <unicode> Imagery",
                "Huynh-Feldt", 2624.42222222222, 3.91435133471376, 670.4615906467,
                17.1549223629789, 6.80963952075043e-10, "FALSE", "FALSE", "Residual",
                "None", 2906.68888888889, 76, 38.2459064327485, "", "", "",
                "", "", "TRUE", "Residual", "Greenhouse-Geisser", 2906.68888888889,
                60.6782434330676, 47.9033130234755, "", "", "", "", "", "FALSE",
                "Residual", "Huynh-Feldt", 2906.68888888889, 74.3726753595615,
                39.0827528367944, "", "", "", "", "", "FALSE")
  
  table <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("Sphericity Assumptions table match", {
  options <- initOpts()
  
  options$sphericityTests <- TRUE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options, view = FALSE, quiet = TRUE)
  
  refTable <- list("Drink", 0.267241056560857, 6.95230186958065e-06, 0.577114320365429,
                    0.590744227025686, 0.5, "TRUE", "Imagery", 0.662101262364057, 0.0244523015633462,
                    0.747440723179836, 0.796842044848417, 0.5, "FALSE", "Drink <unicode> Imagery",
                    0.595043993796251, 0.435658665786593, 0.798397939908785, 0.97858783367844, 0.25,
                    "FALSE")
  
  table <- results[["results"]][["assumptionsObj"]][["sphericity"]][["data"]]
  expect_equal_tables(table, refTable)
  
})

test_that("Post-hoc tests match", {
  options <- initOpts()
  
  options$postHocTestsVariables <- c("Drink", "Imagery")
  options$postHocTestEffectSize <- TRUE
  options$postHocTestsBonferroni <- TRUE
  options$postHocTestPooledError <- FALSE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options, view = FALSE, quiet = TRUE)
  
  refTable <- list("Beer", "Wine", 3.5, -3.045489, 10.04549, 2.84948954082566, 1.22829017262715, 0.274654032208925,
                   "", "", 0.703009687611414, "", "TRUE", "Beer", "Water", 8.31666666666667, 1.771177, 14.86216,
                   3.3351289023547, 2.49365674016224, 0.557598598355329, "", "",
                   0.0660988675936689, "", "FALSE", "Wine", "Water", 4.81666666666667, -1.728823, 11.36216,
                   1.1164571680934, 4.31424223366509, 0.964693890587566, "", "",
                   0.00112213065327869, "", "FALSE")
  
  table <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("Descriptives Match", {
  options <- initOpts()
  
  options$descriptives <- TRUE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options, view = FALSE, quiet = TRUE)
  
  refTable <- list("Beer", "Positive", 20, 21.05, 13.0079934938807, "TRUE", "Beer",
                   "Neutral", 20, 10, 10.295630140987, "FALSE", "Beer", "Negative",
                   20, 4.45, 17.3037111930543, "FALSE", "Wine", "Positive", 20,
                   25.35, 6.73775692801786, "TRUE", "Wine", "Neutral", 20, 11.65,
                   6.24310145596511, "FALSE", "Wine", "Negative", 20, -12, 6.18146635643918,
                   "FALSE", "Water", "Positive", 20, 17.4, 7.07404447704126, "TRUE",
                   "Water", "Neutral", 20, 2.35, 6.83855170878193, "FALSE", "Water",
                   "Negative", 20, -9.2, 6.8024763292882, "FALSE")
  
  table <- results[["results"]][["descriptivesObj"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table, refTable)
})


# Mixed Effects
initOpts <- function(){
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  
  options$repeatedMeasuresFactors <- list(
    list(name = "Looks", levels = c("Attractive", "Average", "Ugly")),
    list(name = "Charisma", levels = c("High", "Some", "None"))
  )
  
  options$repeatedMeasuresCells <- c("att_high", "att_some", "att_none",
                                     "av_high", "av_some", "av_none",
                                     "ug_high", "ug_some", "ug_none")
  options$withinModelTerms <- list(
    list(components = "Looks"),
    list(components = "Charisma"), 
    list(components = c("Looks", "Charisma"))
  )
  
  options$betweenSubjectFactors <- "gender"
  options$betweenModelTerms <- list(
    list(components = "gender")
  )
  options
}

test_that("Between Subjects table match", {
  options <- initOpts()
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list("gender", 0.200000000000001, 1, 0.200000000000001, 0.00473545746857648,
                0.945895847556855, "TRUE", "Residual", 760.222222222222, 18,
                42.2345679012346, "", "", "", "", "", "TRUE")
  
  table <- results[["results"]][["betweenSubjectsEffects"]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("Homogeneity tests correct", {
  options <- initOpts()

  options$homogeneityTests <- TRUE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list("att_high", 1.13105200239091, 1, 18, 0.301611198987337, "TRUE",
                "att_some", 0.598562976996908, 1, 18, 0.449169168742317, "FALSE",
                "att_none", 1.94893878806521, 1, 18, 0.179682774529315, "FALSE",
                "av_high", 0.101977401129945, 1, 18, 0.753145830077659, "FALSE",
                "av_some", 1.76314835904338, 1, 18, 0.200826123727507, "FALSE",
                "av_none", 0.00399511707912524, 1, 18, 0.950298338730636, "FALSE",
                "ug_high", 0.00491266375545877, 1, 18, 0.944894541517532, "FALSE",
                "ug_some", 0.123626373626372, 1, 18, 0.729216564281406, "FALSE",
                "ug_none", 0.0819838056680181, 1, 18, 0.777896246470082, "FALSE")
  
  table <- results[["results"]][["assumptionsObj"]][["levene"]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("(Repeated) Contrast table match", {
  options <- initOpts()
  
  options$contrasts <- list(list(contrast = "repeated", variable = "Looks"))
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list("Attractive - Average", 14.31667, 0.9040603, 15.83596,
                   8.431449e-18, "TRUE", "Average - Ugly", 11.96667, 0.9040603,
                   13.23658, 2.1268e-15, "FALSE")
  
  table <- results[["results"]][["contrasts"]][["collection"]][[1]][["data"]]
  expect_equal_tables(table, refTable)
})


test_that("Effect Size Calculation correct", {
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  
  options$repeatedMeasuresFactors <- list(
    list(name = "Animal", levels = c("Stick", "Kangaroo", "Fish", "Grub"))
    )
  options$repeatedMeasuresCells <- c("Stick Insect", "Kangaroo Testicle",
                                     "Fish Eye", "Witchetty Grub")
  
  options$withinModelTerms <- list(
    list(components = "Animal")
  )
  
  options$effectSizeEstimates <- TRUE
  options$effectSizeEtaSquared <- TRUE
  options$effectSizeOmegaSquared <- TRUE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaRepeatedMeasuresOneWay.csv",
                            options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list("Animal", 83.1249999999999, 3, 27.7083333333333, 3.79380603096984,
                0.0255702968630395, "TRUE", 1, 1, 1, 1, 1, 0.351479915433403,
                0.351479915433403, 0.238785176929506, "Residual", 153.375, 21,
                7.30357142857143, "", "", "", "", "", "TRUE")

  table <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("Simple Effects table match", {
  
  options <- initOpts()
  
  options$betweenSubjectFactors <- "gender"
  options$betweenModelTerms <- list(
    list(components = "gender")
  )
  
  options$withinModelTerms
  options$simpleFactor <- "Looks"
  options$moderatorFactorOne <- "gender"
  options$moderatorFactorTwo <- "Charisma"
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", #mydat,
                            options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list("Female", "High", 42.4666666666668, 2, 21.2333333333334, 0.639629588307488, 
                   0.539062933641058, "TRUE", "Female", "Some", 6444.46666666667, 2, 
                   3222.23333333334, 105.034770010866, 1.18808350406329e-10, "FALSE",               
                   "Female", "None", 187.8, 2, 93.8999999999999, 10.1696750902527, 
                   0.0011082808185639, "FALSE", "Male", "High", 5661.66666666667, 2, 
                   2830.83333333333, 82.5850891410049, 8.54593593608342e-10, "TRUE", 
                   "Male", "Some", 8157.26666666666, 2 ,4078.63333333333, 121.267591674926,
                   3.58637028279497e-11, "FALSE", "Male", "None", 10955, 2, 5477.5,
                   292.566765578635, 1.87815435905324e-14, "FALSE")
  
  table <- results[["results"]][["simpleEffects"]][["data"]]
  expect_equal_tables(table, refTable)
})

# Below are the unit tests for Andy Field's book

# Chapter 8
test_that("Fields Book - Chapter 8 results match", {
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("stick", "testicle", "eye", "witchetty"), "name" = "Animal"))
  options$repeatedMeasuresCells <- list("stick", "testicle", "eye", "witchetty")
  options$withinModelTerms <- list(list("components" = list("Animal")))
  options$contrasts <- list(list("contrast" = "repeated", "variable" = "Animal"))
  options$postHocTestsVariables <- c("Animal")
  options$descriptives <- TRUE
  options$sphericityTests <- options$sphericityCorrections <- TRUE
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Bushtucker.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput1 <- results[["results"]][["descriptivesObj"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(tableOutput1,
                      list("stick", 8, 8.125, 2.232071, "TRUE",
                           "testicle", 8, 4.25, 1.832251, "FALSE",
                           "eye", 8, 4.125, 2.748376, "FALSE",
                           "witchetty", 8, 5.75, 2.915476, "FALSE")
  )
  tableOutput2 <- results[["results"]][["assumptionsObj"]][["sphericity"]][["data"]]
  expect_equal_tables(tableOutput2,
                      list("Animal", 0.136248, 0.04684581, 0.5328456, 0.6657636, 0.3333333, "TRUE")
  )
  tableOutput3 <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(tableOutput3,
                      list("Animal", "None", 83.125, 3, 27.70833, 3.793806, 0.0255703, "TRUE", "FALSE", 1, 1, 1, 1, 1,
                           "Animal", "Greenhouse-Geisser", 83.125, 1.598537, 52.00068, 3.793806, 0.06258412, "FALSE", "FALSE", 1, 1, 1, 1, 1,
                           "Animal", "Huynh-Feldt", 83.125, 1.997291, 41.61888, 3.793806, 0.04833061, "FALSE", "FALSE", 1, 1, 1, 1, 1,
                           "Residual", "None", 153.375, 21, 7.303571, "", "", "", "", "", "TRUE",
                           "Residual", "Greenhouse-Geisser", 153.375, 11.18976, 13.70673, "", "", "", "", "", "FALSE",
                           "Residual", "Huynh-Feldt", 153.375, 13.98104, 10.97022, "", "", "", "", "", "FALSE")
  )
  tableOutput4 <- results[["results"]][["contrasts"]][["collection"]][[1]][["data"]]
  expect_equal_tables(tableOutput4,
                      list("stick - testicle", 3.875, 1.351256, 2.867702, 0.009213629, "TRUE",
                           "testicle - eye", 0.125, 1.351256, 0.09250653, 0.9271724, "FALSE",
                           "eye - witchetty", -1.625, 1.351256, -1.202585, 0.2425216, "FALSE")
  )
  tableOutput5 <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(tableOutput5,
                      list("stick", "testicle", 3.875, -0.05997322, 7.809973, 0.8114691, 4.77529, "", "", "", 0.0121397, "", "TRUE",
                           "stick", "eye", 4, 0.06502678, 7.934973, 0.7319251, 5.46504, "", "", "", 0.00564486, "", "FALSE",
                           "stick", "witchetty", 2.375, -1.559973, 6.309973, 1.79222, 1.325172, "", "", "", 1, "", "FALSE",
                           "testicle", "eye", 0.125, -3.809973, 4.059973, 1.201747, 0.1040152, "", "", "", 1, "", "FALSE",
                           "testicle", "witchetty", -1.5, -5.434973, 2.434973, 1.336306, -1.122497, "", "", "", 1, "", "FALSE",
                           "eye", "witchetty", -1.625, -5.559973, 2.309973, 1.821866, -0.8919426, "", "", "", 1, "", "FALSE")
  )

  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("beer", "wine", "water"), "name" = "drink"), list("levels" = list("positive", "negative", "neutral"), "name" = "imagery"))
  options$repeatedMeasuresCells <- list("beerpos", "beerneg", "beerneut", "winepos", "wineneg", "wineneut", "waterpos", "waterneg", "waterneut")
  options$withinModelTerms <- list(list("components" = list("drink")), list("components" = list("imagery")), list("components" = list("drink", "imagery")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "drink"), list("contrast" = "none", "variable" = "imagery"))
  options$descriptives <- TRUE
  options$sphericityTests <- options$sphericityCorrections <- TRUE
  options$postHocTestsVariables <- c("drink", "imagery")
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Attitude.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput6 <- results[["results"]][["descriptivesObj"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(tableOutput6,
                      list("beer", "positive", 20, 21.05, 13.00799, "TRUE",
                           "beer", "negative", 20, 4.45, 17.30371, "FALSE",
                           "beer", "neutral", 20, 10, 10.29563, "FALSE",
                           "wine", "positive", 20, 25.35, 6.737757, "TRUE",
                           "wine", "negative", 20, -12, 6.181466, "FALSE",
                           "wine", "neutral", 20, 11.65, 6.243101, "FALSE",
                           "water", "positive", 20, 17.4, 7.074044, "TRUE",
                           "water", "negative", 20, -9.2, 6.802476, "FALSE",
                           "water", "neutral", 20, 2.35, 6.838552, "FALSE")
  )
  tableOutput7 <- results[["results"]][["assumptionsObj"]][["sphericity"]][["data"]]
  expect_equal_tables(tableOutput7,
                      list("drink", 0.2672411, 6.952302e-06, 0.5771143, 0.5907442, 0.5, "TRUE",
                           "imagery", 0.6621013, 0.0244523, 0.7474407, 0.796842, 0.5, "FALSE",
                           "drink <unicode> imagery", 0.595044, 0.4356587, 0.7983979, 0.9785878, 0.25, "FALSE")
  )
  tableOutput8 <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(tableOutput8,
                      list("drink", "None", 2092.344, 2, 1046.172, 5.105981, 0.01086293, "TRUE", "FALSE", 1, 1, 1, 1, 1,
                           "drink", "Greenhouse-Geisser", 2092.344, 1.154229, 1812.764, 5.105981, 0.02976868, "FALSE", "FALSE", 1, 1, 1, 1, 1,
                           "drink", "Huynh-Feldt", 2092.344, 1.181488, 1770.939, 5.105981, 0.02881391, "FALSE", "FALSE", 1, 1, 1, 1, 1,
                           "Residual", "None", 7785.878, 38, 204.8915, "", "", "", "", "", "TRUE",
                           "Residual", "Greenhouse-Geisser", 7785.878, 21.93034, 355.0276, "", "", "", "", "", "FALSE",
                           "Residual", "Huynh-Feldt", 7785.878, 22.44828, 346.8363, "", "", "", "", "", "FALSE",
                           "imagery", "None", 21628.68, 2, 10814.34, 122.5648, 2.680197e-17, "TRUE", "FALSE", 1, 1, 1, 1, 1,
                           "imagery", "Greenhouse-Geisser", 21628.68, 1.494881, 14468.49, 122.5648, 1.757286e-13, "FALSE", "FALSE", 1, 1, 1, 1, 1,
                           "imagery", "Huynh-Feldt", 21628.68, 1.593684, 13571.5, 122.5648, 3.142804e-14, "FALSE", "FALSE", 1, 1, 1, 1, 1,
                           "Residual", "None", 3352.878, 38, 88.23363, "", "", "", "", "", "TRUE",
                           "Residual", "Greenhouse-Geisser", 3352.878, 28.40275, 118.0477, "", "", "", "", "", "FALSE",
                           "Residual", "Huynh-Feldt", 3352.878, 30.28, 110.7291, "", "", "", "", "", "FALSE",
                           "drink <unicode> imagery", "None", 2624.422, 4, 656.1056, 17.15492, 4.58904e-10, "TRUE", "FALSE",
                           "drink <unicode> imagery", "Greenhouse-Geisser", 2624.422, 3.193592, 821.7776, 17.15492, 1.900249e-08, "FALSE", "FALSE",
                           "drink <unicode> imagery", "Huynh-Feldt", 2624.422, 3.914351, 670.4616, 17.15492, 6.80964e-10, "FALSE", "FALSE",
                           "Residual", "None", 2906.689, 76, 38.24591, "", "", "", "", "", "TRUE",
                           "Residual", "Greenhouse-Geisser", 2906.689, 60.67824, 47.90331, "", "", "", "", "", "FALSE",
                           "Residual", "Huynh-Feldt", 2906.689, 74.37268, 39.08275, "", "", "", "", "", "FALSE")
  )
  tableOutput10 <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(tableOutput10,
                      list("beer", "wine", 3.5, -3.045489, 10.04549, 2.84949, 1.22829, "", "", "", 0.7030097, "", "TRUE",
                           "beer", "water", 8.316667, 1.771177, 14.86216, 3.335129, 2.493657, "", "", "", 0.06609887, "", "FALSE",
                           "wine", "water", 4.816667, -1.728823, 11.36216, 1.116457, 4.314242, "", "", "", 0.001122131, "", "FALSE")
  )
  tableOutput12 <- results[["results"]][["posthoc"]][["collection"]][[2]][["data"]]
  expect_equal_tables(tableOutput12,
                      list("positive", "negative", 26.85, 22.55466, 31.14534, 1.914621, 14.02366, "", "", "", 5.361804e-11, "", "TRUE",
                           "positive", "neutral", 13.26667, 8.97133, 17.562, 1.112555, 11.92451, "", "", "", 8.646278e-10, "", "FALSE",
                           "negative", "neutral", -13.58333, -17.87867, -9.287997, 1.979851, -6.860786, "", "", "", 4.54656e-06, "", "FALSE")
  )
})

# Chapter 9
test_that("Fields Book - Chapter 9 results match", {
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("attractive", "average", "unattractive"), "name" = "Looks"), list("levels" = list("high", "average", "low"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("att_high", "att_some", "att_none", "av_high", "av_some", "av_none", "ug_high", "ug_some", "ug_none")
  options$withinModelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Looks", "Charisma")))
  options$betweenSubjectFactors <- c("Strategy")
  options$betweenModelTerms <- list(list("components" = list("Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  options$sphericityTests <- options$sphericityCorrections <- TRUE
  options$sphericityGreenhouseGeisser <- TRUE
  options$sphericityHuynhFeldt <- options$sphericityNone <- FALSE
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput1 <- results[["results"]][["assumptionsObj"]][["sphericity"]][["data"]]
  expect_equal_tables(tableOutput1,
                      list("Looks", 0.9602054, 0.708101, 0.9617284, 1, 0.5, "TRUE",
                           "Charisma", 0.9293298, 0.5363446, 0.9339944, 1, 0.5, "FALSE",
                           "Looks <unicode> Charisma", 0.6133545, 0.5339382, 0.7993543, 0.9922411, 0.25, "FALSE")
  )
  tableOutput2 <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(tableOutput2,
                      list("Looks", "Greenhouse-Geisser", 20779.63, 1.923457, 10803.28, 423.7325, 7.624114e-25, "TRUE", "FALSE",
                           "Looks <unicode> Strategy", "Greenhouse-Geisser", 3944.1, 1.923457, 2050.527, 80.42699, 1.487026e-13, "FALSE", "FALSE",
                           "Residual", "Greenhouse-Geisser", 882.7111, 34.62222, 25.49551, "", "", "", "", "", "TRUE",
                           "Charisma", "Greenhouse-Geisser", 23233.6, 1.867989, 12437.76, 328.2498, 2.056621e-22, "TRUE", "FALSE",
                           "Charisma <unicode> Strategy", "Greenhouse-Geisser", 4420.133, 1.867989, 2366.252, 62.44868, 9.442426e-12, "FALSE", "FALSE",
                           "Residual", "Greenhouse-Geisser", 1274.044, 33.6238, 37.89115, "", "", "", "", "", "TRUE",
                           "Looks <unicode> Charisma", "Greenhouse-Geisser", 4055.267, 3.197417, 1268.295, 36.63253, 9.003598e-14, "TRUE", "FALSE",
                           "Looks <unicode> Charisma <unicode> Strategy", "Greenhouse-Geisser", 2669.667, 3.197417, 834.9448, 24.11596, 1.470422e-10, "FALSE", "FALSE",
                           "Residual", "Greenhouse-Geisser", 1992.622, 57.55351, 34.62208, "", "", "", "", "", "TRUE")
  )
  tableOutput3 <- results[["results"]][["betweenSubjectsEffects"]][["data"]]
  expect_equal_tables(tableOutput3,
                      list("Strategy", 0.2, 1, 0.2, 0.004735457, 0.9458958, "TRUE",
                           "Residual", 760.2222, 18, 42.23457, "", "", "", "", "", "TRUE")
  )
})
