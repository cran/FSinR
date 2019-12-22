context("Search + Wrapper")

data1 <- get(load('../data/dataClass.RData'))
data2 <- get(load("../data/dataReg.RData"))

test_that("Classification", {
  
  # Wrapper method
  resamplingParams <- list(method = "cv", number = 5)
  fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy", tuneGrid = expand.grid(k = seq(1,10,by=2))) 
  wrapper <- wrapperGenerator("knn",resamplingParams, fittingParams) # wrapper method
  
  # SFS
  res <- sfs(data1, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # SFFS
  res <- sffs(data1, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # SBS
  res <- sbs(data1, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # SFBS
  res <- sfbs(data1, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # BFS
  res <- breadthFirstSearch(data1, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # DFS
  res <- deepFirstSearch(data1, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # GA
  res <- ga(data1, 'y', wrapper, maxiter=15)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # WOA
  
  # ACO
  res <- aco(data1, 'y', wrapper, iter=15)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # SA
  res <- sa(data1, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # HC
  
  # TS
  res <- ts(data1, 'y', wrapper, iter=50, tamTabuList=3, intensification=1, diversification=1)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # LVW
  res <- lvw(data1, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
})

test_that("Regression", {
  
  # Wrapper method
  resamplingParams <- list(method = "cv", number = 3)
  fittingParams <- list(preProcess = c("center", "scale"), metric="RMSE")
  wrapper <- wrapperGenerator("lm",resamplingParams, fittingParams) # wrapper method
  
  # SFS
  res <- sfs(data2, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # SFFS
  res <- sffs(data2, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # SBS
  res <- sbs(data2, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # SFBS
  res <- sfbs(data2, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # BFS
  res <- breadthFirstSearch(data2, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # DFS
  res <- deepFirstSearch(data2, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # GA
  res <- ga(data2, 'y', wrapper, maxiter=15)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # WOA
  
  # ACO
  res <- aco(data2, 'y', wrapper, iter=15)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # SA
  res <- sa(data2, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # HC
  
  # TS
  res <- ts(data2, 'y', wrapper, iter=50, tamTabuList=3, intensification=1, diversification=1)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
  # LVW
  res <- lvw(data2, 'y', wrapper)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )
  
})