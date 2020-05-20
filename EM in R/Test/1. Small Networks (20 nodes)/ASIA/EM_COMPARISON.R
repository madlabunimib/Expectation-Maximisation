
check_input_comparison <- function(initial_data, data_soft, data_hard ){
  if (is.null(initial_data)){
    stop("Some required parameters have not been defined: INITIAL_DATA")
  }
  if (is.null(data_soft)){
    stop("Some required parameters have not been defined: data_soft")
  }
  if (is.null(data_hard)){
    stop("Some required parameters have not been defined: data_hard")
  }
  
  if(ncol(initial_data) != ncol(data_soft)){
    initial_data <- data.frame(t(initial_data), stringsAsFactors = FALSE)
  }
}


compare_comparison <- function(initial_data, data_soft, data_hard ){
  equal_values = 0
  number_missing = 0
  
  for (i in 1:ncol(initial_data)){
    for (j in 1:nrow(initial_data)){
      if (is.na(initial_data[j,i])){
        if(data_soft[j,i] == data_hard[j,i]){
          equal_values = equal_values +1
        }
        number_missing = number_missing + 1
      }
    }
  }
  print(number_missing)
  print(equal_values)
  result = equal_values/number_missing
  return(result)
}



compare_em <- function(initial_data, data_soft, data_hard ){
  check_input_comparison(initial_data, data_soft, data_hard )
  percentile = compare_comparison(initial_data, data_soft, data_hard )
  return(percentile)
}




check_input_comparison_gt <- function(initial_data, ground_truth, data_predicted){
  if (is.null(initial_data)){
    stop("Some required parameters have not been defined: INITIAL_DATA")
  }
  if (is.null(ground_truth)){
    stop("Some required parameters have not been defined: data_soft")
  }
  if (is.null(data_predicted)){
    stop("Some required parameters have not been defined: data_hard")
  }
  
  if(ncol(initial_data) != ncol(data_predicted)){
    initial_data <- data.frame(t(initial_data), stringsAsFactors = FALSE)
  }
  if(ncol(ground_truth) != ncol(data_predicted)){
    ground_truth <- data.frame(t(ground_truth), stringsAsFactors = FALSE)
  }
}


compare_comparison_gt <- function(initial_data, ground_truth, data_predicted){
  equal_values = 0
  number_missing = 0
  
  for (i in 1:ncol(initial_data)){
    for (j in 1:nrow(initial_data)){
      if (is.na(initial_data[j,i])){
        if(data_predicted[j,i] == ground_truth[j,i]){
          equal_values = equal_values +1
        }
        number_missing = number_missing + 1
      }
    }
  }
  print(number_missing)
  print(equal_values)
  result = equal_values/number_missing
  return(result)
}





compare_em_with_ground_truth <- function(initial_data, ground_truth, data_predicted){
  check_input_comparison_gt(initial_data, ground_truth, data_predicted)
  percentile = compare_comparison_gt(initial_data, ground_truth, data_predicted)
  return(percentile)
}
