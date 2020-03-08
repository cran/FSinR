context("RFSM measure")

test_that("is discrete should return true", {
  expect_equal(is.discrete(BOD$Time), TRUE)
  expect_equal(is.discrete(ChickWeight$Chick), TRUE)
  expect_equal(is.discrete(Seatbelts[,7]), TRUE) #VanKilled
  
})

test_that("is discrete should return false", {
  expect_equal(is.discrete(BOD$demand), FALSE)
  expect_equal(is.discrete(ChickWeight$weight), FALSE)
  expect_equal(is.discrete(Seatbelts[,6]), FALSE) #PetrolPrice
})

test_that("RFSM_diff works with continuous features", {
  e1 <- iris[1,]
  e2 <- iris[51,]
  f_max = max(iris[,"Petal.Length"])
  f_min = min(iris[,"Petal.Length"])
  result = abs(e1$Petal.Length - e2$Petal.Length) / (f_max - f_min)
  expect_equal(RFSM_diff(iris, "Petal.Length", e1, e2),result)
})

test_that("RFSM_diff works with discrete features", {
  e1 <- mtcars[1,]
  e2 <- mtcars[2,]
  e3 <- mtcars[4,]
  expect_equal(RFSM_diff(mtcars, "gear", e1, e2),0)
  expect_equal(RFSM_diff(mtcars, "gear", e1, e3),1)
})

test_that("RFSM_diffS works without penalization", {
  e1 <- iris[1,]
  e2 <- iris[51,]
  pl_max = max(iris[,"Petal.Length"])
  pl_min = min(iris[,"Petal.Length"])
  sl_max = max(iris[,"Sepal.Length"])
  sl_min = min(iris[,"Sepal.Length"])
  result = max(abs(e1$Petal.Length - e2$Petal.Length) / (pl_max - pl_min), abs(e1$Sepal.Length - e2$Sepal.Length) / (sl_max - sl_min))
  expect_equal(RFSM_diffS(iris, c("Petal.Length","Sepal.Length"), e1, e2),result)
})

test_that("RFSM_diffS works with penalization", {
  e1 <- iris[1,]
  e2 <- iris[51,]
  pl_max = max(iris[,"Petal.Length"])
  pl_min = min(iris[,"Petal.Length"])
  sl_max = max(iris[,"Sepal.Length"])
  sl_min = min(iris[,"Sepal.Length"])
  result = min(abs(e1$Petal.Length - e2$Petal.Length) / (pl_max - pl_min), abs(e1$Sepal.Length - e2$Sepal.Length) / (sl_max - sl_min))
  expect_equal(RFSM_diffS(iris, c("Petal.Length","Sepal.Length"), e1, e2, TRUE),result)
})

test_that("selectkneighbours works", {
  e1 <- iris[1,]
  e3 <- iris[3,]
  e4 <- iris[4,]
  e3$rfsm_distance <- RFSM_diffS(iris, c("Petal.Length","Sepal.Length"), e1, e3)
  e4$rfsm_distance <- RFSM_diffS(iris, c("Petal.Length","Sepal.Length"), e1, e4)
  result <- selectKNeighbours(iris, c("Petal.Length","Sepal.Length"), iris[2:5,], iris[1,],2, penalization = FALSE) 
  expect_true(tail(duplicated(rbind(result,e3)),1))
  expect_true(tail(duplicated(rbind(result,e4)),1))
})

test_that("RFSM measure returns 0 on empty set", {
  expect_equal(RFSM(iris, 'Species', c()), 0)
})


test_that("RFSM works with 1 feature", {
  result <- RFSM(iris, 'Species', c('Petal.Length'))
  expect_gte(result, -1)
  expect_lte(result, 1)
})

test_that("RFSM works multiple features", {
  result <- RFSM(iris, 'Species', c('Sepal.Length','Petal.Length'))
  expect_gte(result, -1)
  expect_lte(result, 1)
})

test_that("RFSM works with all features", {
  result <- RFSM(iris, 'Species', c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'))
  expect_gte(result, -1)
  expect_lte(result, 1)
})

test_that("Name is set", {
  expect_equal(attr(RFSM,'name'),"RFSM");
  expect_equal(attr(RFSM,'shortName'),"RFSM");
})