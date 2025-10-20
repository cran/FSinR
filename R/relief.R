#' relief
#' classification and regression
#' continous and discrete data
#'
#' @author Alfonso Jiménez-Vílchez
#' @title Relief
#' @description Generates an evaluation function that calculates a measure of the set of features with relief (individual measure). The relief algorithm \insertCite{Kira1992}{FSinR} finds weights of continous and discrete attributes basing on a distance between instances. Adapted from Piotr Romanski's Fselector package \insertCite{FSelectorPkg}{FSinR}. This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @param neighbours.count - number of neighbours to find for every sampled instance
#' @param sample.size - number of instances to sample
#'
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @return Returns a function that is used to generate an individual evaluation measure using relief
#' 
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#'\donttest{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to individually evaluate a set of features
#' ## Classification problem
#' 
#' # Generate the evaluation function with Cramer
#' relief_evaluator <- relief()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' relief_evaluator(iris,'Species',c('Sepal.Length'))
#' }
relief <- function(neighbours.count = 5, sample.size = 10) {
  
  reliefEvaluator <- function(data, class, features) {
    
    # Helper functions
    instance_distance <- function(instance1, instance2) {
      len <- dim(instance1)[2]
      if(len != dim(instance2)[2]) stop("Instances of different lengths")
      if(len <= 1) stop("Too few attributes")
      result <- sapply(2:len, function(i) field_distance(i, instance1, instance2))
      res <- sum(result ^ 2)
      if(is.na(res)) stop("Internal error. Distance NA.")
      return(res)
    }
    
    field_distance <- function(col_idx, instance1, instance2) {
      value1 <- instance1[1, col_idx]
      value2 <- instance2[1, col_idx]
      attr_idx <- col_idx - 1
      if(is.factor(value1) && is.factor(value2)) {
        if(is.na(value1) && is.na(value2)) {
          if(classification)
            return(1 - sum(p_val_in_class[[attr_idx]][, instance1[1,1]] *
                             p_val_in_class[[attr_idx]][, instance2[1,1]]))
          else
            return(1 - p_same_val[[attr_idx]])
        } else if(is.na(value1) || is.na(value2)) {
          known_value <- ifelse(is.na(value1), value2, value1)
          unknown_class <- ifelse(is.na(value1), instance1[1,1], instance2[1,1])
          if(classification)
            return(1 - p_val_in_class[[attr_idx]][known_value, unknown_class])
          else
            return(1 - p_val[[attr_idx]][known_value])
        } else if(value1 == value2) return(0)
        else return(1)
      } else if(is.numeric(value1) && is.numeric(value2)) {
        if(is.na(value1) && is.na(value2)) return(1)
        else if(is.na(value1)) return(max(value2, 1 - value2))
        else if(is.na(value2)) return(max(value1, 1 - value1))
        else return(abs(value1 - value2))
      } else stop("Unsupported value type")
    }
    
    # Environment to store state
    state <- new.env(parent = emptyenv())
    
    # Prepare data
    formula <- as.simple.formula(features, class)
    new_data <- get.data.frame.from.formula(formula, data)
    new_data <- normalize.min.max(new_data)
    
    instances_count <- dim(new_data)[1]
    attributes_count <- dim(new_data)[2] - 1
    attr_names <- dimnames(new_data)[[2]][-1]
    
    classification <- is.factor(new_data[[1]])
    
    # Initialize state variables
    if(classification) {
      class_vector <- new_data[[1]]
      class_prob <- table(class_vector) / length(class_vector)
      classes <- names(class_prob)
      class_count <- length(classes)
      
      p_val_in_class <- lapply(new_data[-1], function(vec) {
        if(!is.factor(vec) || !any(is.na(vec))) return(NULL)
        tab <- table(vec, class_vector)
        apply(tab, 2, function(x) if(sum(x) == 0) x else x/sum(x))
      })
    } else {
      class_count <- 1
      ndc <- 0
      nda <- numeric(attributes_count)
      ndcda <- numeric(attributes_count)
      p_val <- lapply(new_data[-1], function(vec) {
        if(!is.factor(vec) || !any(is.na(vec))) return(NULL)
        tab <- table(vec)
        if(sum(tab) != 0) tab / sum(tab) else tab
      })
      p_same_val <- lapply(p_val, function(attr) if(is.null(attr)) NULL else sum(attr^2))
    }
    
    state$results <- rep(0, attributes_count)
    state$n_array <- array(0, c(class_count, neighbours.count, 2))
    state$nn_stored_count <- array(0, class_count)
    
    # Sample instances
    if(sample.size < 1) {
      sample.size <- 1
      sample_instances_idx <- sample(1:instances_count, 1)
    } else if(sample.size > instances_count) {
      sample.size <- instances_count
      sample_instances_idx <- 1:instances_count
    } else {
      sample_instances_idx <- sort(sample(1:instances_count, sample.size, replace = TRUE))
    }
    
    # Internal functions
    find_neighbours <- function(instance_idx) {
      instance <- new_data[instance_idx,, drop = FALSE]
      for(current_idx in 1:instances_count) {
        if(instance_idx == current_idx) next
        current_instance <- new_data[current_idx,, drop = FALSE]
        if(is.na(current_instance[1,1])) next
        dist <- instance_distance(instance, current_instance)
        class_no <- if(classification) which(classes == current_instance[[1]]) else 1
        if(state$nn_stored_count[class_no] < neighbours.count) {
          state$nn_stored_count[class_no] <- state$nn_stored_count[class_no] + 1
          state$n_array[class_no, state$nn_stored_count[class_no], ] <- c(dist, current_idx)
        } else {
          max_idx <- which.max(state$n_array[class_no,,1])
          if(dist < state$n_array[class_no,max_idx,1])
            state$n_array[class_no,max_idx,] <- c(dist, current_idx)
        }
      }
    }
    
    update_weights <- function(instance_idx) {
      instance <- new_data[instance_idx,, drop = FALSE]
      instance_class_no <- if(classification) which(classes == instance[1,1]) else 1
      
      if(classification) {
        for(attr_idx in 1:attributes_count) {
          col_idx <- attr_idx + 1
          # Nearest hits
          hits_sum <- 0
          if(state$nn_stored_count[instance_class_no] > 0) {
            hits_sum <- sum(sapply(1:state$nn_stored_count[instance_class_no], function(n_idx) {
              n_instance_idx <- state$n_array[instance_class_no,n_idx,2]
              n_instance <- new_data[n_instance_idx,, drop = FALSE]
              field_distance(col_idx, instance, n_instance)
            }))
            hits_sum <- hits_sum / state$nn_stored_count[instance_class_no]
          }
          # Nearest misses
          misses_sum <- 0
          if(class_count > 1) {
            misses_sum <- sum(sapply((1:class_count)[-instance_class_no], function(class_no) {
              class_misses_sum <- 0
              if(state$nn_stored_count[class_no] > 0) {
                class_misses_sum <- sum(sapply(1:state$nn_stored_count[class_no], function(n_idx) {
                  n_instance_idx <- state$n_array[class_no,n_idx,2]
                  n_instance <- new_data[n_instance_idx,, drop = FALSE]
                  field_distance(col_idx, instance, n_instance)
                }))
                class_misses_sum <- class_misses_sum * class_prob[class_no] / state$nn_stored_count[class_no]
              }
              return(class_misses_sum)
            }))
            misses_sum <- misses_sum / (1 - class_prob[instance_class_no])
          }
          state$results[attr_idx] <- state$results[attr_idx] - hits_sum + misses_sum
        }
      } else {
        if(state$nn_stored_count[1] > 0) {
          for(n_idx in 1:state$nn_stored_count[1]) {
            n_instance_idx <- state$n_array[1,n_idx,2]
            n_instance <- new_data[n_instance_idx,, drop = FALSE]
            class_diff <- field_distance(1, instance, n_instance)
            ndc <<- ndc + class_diff
            for(attr_idx in 1:attributes_count) {
              col_idx <- attr_idx + 1
              attr_diff_norm <- field_distance(col_idx, instance, n_instance) / state$nn_stored_count[1]
              nda[attr_idx] <- nda[attr_idx] + attr_diff_norm
              ndcda[attr_idx] <- ndcda[attr_idx] + class_diff * attr_diff_norm
            }
          }
        }
      }
    }
    
    # Main loop
    for(current_instance_idx in sample_instances_idx) {
      state$nn_stored_count[] <- 0
      state$n_array[] <- Inf
      find_neighbours(current_instance_idx)
      update_weights(current_instance_idx)
    }
    
    # Return results
    if(classification) {
      results <- state$results / sample.size
      names(results) <- features
      if(length(features) == 1) return(results[[1]])
      return(results)
    } else {
      results <- ndcda / ndc - ((nda - ndcda)/(sample.size - ndc))
      names(results) <- features
      if(length(features) == 1) return(results[[1]])
      return(results)
    }
  }
  
  attr(reliefEvaluator,'shortName') <- "relief"
  attr(reliefEvaluator,'name') <- "Relief"
  attr(reliefEvaluator,'target') <- "maximize"
  attr(reliefEvaluator,'kind') <- "Individual measure"
  attr(reliefEvaluator,'needsDataToBeDiscrete') <- FALSE
  attr(reliefEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(reliefEvaluator)
}



as.simple.formula <- function(attributes, class) {
  return(as.formula(paste(class, paste(attributes, sep = "", collapse = " + "), sep = " ~ ")))
}

#' @importFrom stats model.frame
get.data.frame.from.formula <- function(formula, data) {
  d = model.frame(formula, data, na.action = NULL)
  for(i in 1:dim(d)[2]) {
    if(is.factor(d[[i]]) || is.logical(d[[i]]) || is.character(d[[i]]))
      d[[i]] = factor(d[[i]])
  }
  return(d)
}


#' @importFrom stats complete.cases
normalize.min.max <- function(data) {
  attr_count = dim(data)[2]
  if(attr_count == 0)
    return(data)
  for(i in 1:attr_count) {
    if(!is.numeric(data[, i]))
      next()
    if(!any(complete.cases(data[, i])))
      next()
    mm = range(data[, i], na.rm = TRUE)
    minimum = mm[1]
    maximum = mm[2]
    if(minimum == maximum)
      data[, i] = data[, i] / minimum
    else
      data[, i] = (data[, i] - minimum) / (maximum - minimum)
  }
  
  return(data)
}

#' relief
#' classification and regression
#' continous and discrete data
#'
#' @author Alfonso Jiménez-Vílchez
#' @title Normalized Relief
#' @description Generates an evaluation function that calculates a measure of the set of features between 0 and 1 with relief (individual measure). The relief algorithm \insertCite{Kira1992}{FSinR} finds weights of continous and discrete attributes basing on a distance between instances. Adapted from Piotr Romanski's Fselector package \insertCite{FSelectorPkg}{FSinR}. This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @param neighbours.count - number of neighbours to find for every sampled instance
#' @param sample.size - number of instances to sample
#'
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @return Returns a function that is used to generate an individual evaluation measure using relief
#' 
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#'\donttest{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to individually evaluate a set of features
#' ## Classification problem
#' 
#' # Generate the evaluation function with Cramer
#' relief_evaluator <- normalizedRelief()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' relief_evaluator(iris,'Species',c('Sepal.Length'))
#' }
normalizedRelief <- function(neighbours.count = 5, sample.size = 10) {
  originalRelief <- relief(neighbours.count, sample.size)
  normalizedReliefEvaluator <- function(data, class, features) {
    originalValue <- originalRelief(data, class, features)
    return(originalValue / 2 + 0.5)
  }
  
  attr(normalizedReliefEvaluator,'shortName') <- "normalizedRelief"
  attr(normalizedReliefEvaluator,'name') <- "Normalized Relief"
  attr(normalizedReliefEvaluator,'target') <- "maximize"
  attr(normalizedReliefEvaluator,'kind') <- "Individual measure"
  attr(normalizedReliefEvaluator,'needsDataToBeDiscrete') <- FALSE
  attr(normalizedReliefEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(normalizedReliefEvaluator)
}
