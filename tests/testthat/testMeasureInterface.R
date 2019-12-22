context("Evaluation Measure Interface")

resamplingParams <- list(method = "cv", number = 5)
fittingParams <- list(preProc = c("center", "scale"), metric = "Accuracy", tuneGrid = expand.grid(k = seq(1, 10, by = 2))) 
wrapper <- wrapperGenerator("knn",resamplingParams, fittingParams)

measures <- list(chiSquared, cramer, binaryConsistency, IEConsistency, IEPConsistency, roughsetConsistency, mutualInformation, determinationCoefficient, gainRatio, symmetricalUncertain, fscore, giniIndex, Jd, relief, RFSM, wrapper)
for (measure in measures) {
  result <- measure(ToothGrowth, 'supp', c('len', 'dose'))
  test_that(paste(attr(measure,'name')," has maximize attribute"), {
    expect_true(isTRUE(attr(measure,'maximize')) || isFALSE(attr(measure,'maximize')))
  })
  test_that(paste(attr(measure,'name')," returns numeric value"), {
    expect_true(is.numeric(result))
  })
}

setMeasures <- list(binaryConsistency, IEConsistency, IEPConsistency, roughsetConsistency, mutualInformation, determinationCoefficient, gainRatio, symmetricalUncertain, giniIndex, Jd, RFSM, wrapper)
for (measure in setMeasures) {
  test_that(paste(attr(measure,'name')," has right kind"), {
    expect_equal(attr(measure,'kind'), "Set measure")
  })
}

individualMeasures <- list(chiSquared, cramer, fscore, relief)
for (measure in individualMeasures) {
  test_that(paste(attr(measure,'name')," has right kind"), {
    expect_equal(attr(measure,'kind'), "Individual measure")
  })
}
