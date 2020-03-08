RFSM_diffS <- function(data, S, e1, e2, penalization = FALSE) {
  result <- NULL
  for (feature in S) {
    if (is.null(feature))  {
      result <- RFSM_diff(data, feature, e1, e2)
    } else {
      f_column <- unlist(as.data.frame(data[, feature, drop = FALSE]))
      if (is.discrete(f_column)) {
        result <- (if (is.null(result)) 0 else result) || RFSM_diff(data, feature, e1, e2)
      } else {
        if (penalization) {
          result <- min(result, RFSM_diff(data, feature, e1, e2))
        } else {
          result <- max(result, RFSM_diff(data, feature, e1, e2))
        }
      }
    }
  }
  return(as.numeric(result))
}

RFSM_diff <- function(data, f, e1, e2) {
 f_column <- unlist(as.data.frame(data[, f, drop = FALSE]))
 if (is.discrete(f_column)) {
   if (e1[f] == e2[f]) {
     return(0)
   } else
     return(1)
 } else {
   f_min <- min(f_column)
   f_max <- max(f_column)
   return(as.numeric( (abs(e1[f] - e2[f]) / (f_max - f_min))))
 }
}

is.discrete <- function(data) {
  #Check if var is integer.
  enough_values <- length(data) > 30
  condition <- TRUE
  if (enough_values == TRUE) {
    condition <- length(unique(data)) / length(data) < 0.1
  }
  return((all(data == as.integer(data)) || all(typeof(data) == "integer")) && condition)
}

#' @importFrom rlang .data
selectKNeighbours <- function(data, S, examples, example, k, penalization = FALSE) {
  result <- examples
  result <- result %>%
    rowwise() %>%
    do(row = as_tibble(.data)) %>%
    mutate(rfsm_distance = RFSM_diffS(data, S, example, row, penalization)) %>%
    unnest(cols = c(row)) %>%
    arrange(desc(.data$rfsm_distance)) %>%
    top_n(k, .data$rfsm_distance)
  return(result)
}

#' @author Alfonso Jiménez-Vílchez
#' @title RFSM evaluation measure
#' @description Feature set measure based on relief. Described in \insertCite{Arauzo2004}{FSinR}
#' @param data - A data frame with the features and the class of the examples
#' @param class - The name of the dependent variable
#' @param features - The names of the selected features
#' @param m - Number of iterations
#' @param k - Number of neighbours
#'
#' @return - The value of the function for the selected features
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#' @import tidyr
#' @import prodlim
#'
#' @examples
#' RFSM(iris, 'Species', c('Sepal.Width','Sepal.Length'))
RFSM <- function(data, class, features, m = 5, k = 4) {
  
  if (!length(features)) {
    return(0);
  }
  
  # Group all proyected examples in lists by their class
  # Get values of the features
  features <- unlist(features)
  feature.data <- data[, features, drop = FALSE]
  feature.classes <- as.data.frame(data[, class, drop = FALSE])
  feature.list <- unique(feature.classes)
  
  hash.table <- list()
  for (i in 1:nrow(feature.list)) {
    hash.table[[i]] <- subset(feature.data, FALSE) # Get dataframe with columns names but no rows.
  }
  for (i in 1:nrow(feature.data)) {
    class_index <- which(feature.list == as.character(feature.classes[i, ]))
    hash.table[[class_index]][nrow(hash.table[[class_index]]) + 1, ] <- feature.data[i, ] # rbind does not work correctly
  }
  feature.prob <- list()
  for (i in 1:nrow(feature.list)) {
    feature.prob[[i]] <- nrow(hash.table[[i]]) / nrow(data)
  }
  
  w <- 0;
  
  for (i in range(m)) {
    e1.row <- sample(nrow(data), 1)
    e1 <- data[e1.row, ]
    e1.data <- e1[, features, drop = FALSE]
    for (j in 1:nrow(feature.list)) {
      examples <- hash.table[[j]][, features, drop = FALSE]
      if (e1[, class] == feature.list[j, ]) {
        position <- row.match(e1.data, examples)
        if (!is.na(position)) {
          examples <- examples[-c(position),, drop = FALSE]
        }
        w <- w - sum(selectKNeighbours(data, features, examples, e1.data, k)$rfsm_distance) / k
      } else {
        w <- w + feature.prob[[j]] * sum(selectKNeighbours(data, features, examples, e1.data, k, TRUE)$rfsm_distance) / k
      }
    }
  }
  
  return(w / m)
}
attr(RFSM,'shortName') <- "RFSM"
attr(RFSM,'name') <- "RFSM"
attr(RFSM,'maximize') <- TRUE
attr(RFSM,'kind') <- "Set measure"