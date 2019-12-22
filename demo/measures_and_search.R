resamplingParams <- list(method = "cv", number = 5)
fittingParams <- list(preProc = c("center", "scale"), metric = "Accuracy", tuneGrid = expand.grid(k = seq(1, 10, by = 2))) 
wrapper <- wrapperGenerator("knn",resamplingParams, fittingParams) # wrapper method
measures <- list(binaryConsistency, IEConsistency, IEPConsistency, roughsetConsistency, giniIndex, mutualInformation, gainRatio, symmetricalUncertain, determinationCoefficient, MDLC, RFSM, wrapper)

methods <- list(
  list(method = sfs,
       name = 'SFS'),
  list(method = sbs,
       name = 'SBS'),
  list(method = sffs,
       name = 'SFFS'),
  list(method = sfbs,
       name = 'SFBS'),
  list(method = lvw,
       name = 'LVW'),
  list(method = ga,
       name = 'GA'),
  list(method = sa,
       name = 'SA'),
  list(method = woa,
       name = 'WOA'),
  list(method = aco,
       name = 'ACO'),
  list(method = ts,
       name = 'TS'),
  list(method = hc,
       name = 'HC')
)
for (method in methods) {
  print("")
  print("###############################################")
  print(paste("Method ",method$name))
  print("###############################################")
  print("")
  print(paste("Method ",method$name))
  for (measure in measures) {
    print("")
    print("   @@@@@@@")
    print(paste("   @Measure ",attr(measure,'name')))
    print("   @@@@@@@")
    print("")
    value <- method$method(iris, 'Species',measure)
    print(value)
  }
}