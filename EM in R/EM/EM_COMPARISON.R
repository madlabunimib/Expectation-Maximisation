
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
  #print(number_missing)
  #print(equal_values)
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
  #print(number_missing)
  #print(equal_values)
  result = equal_values/number_missing
  return(result)
}





compare_em_with_ground_truth <- function(initial_data, ground_truth, data_predicted){
  check_input_comparison_gt(initial_data, ground_truth, data_predicted)
  percentile = compare_comparison_gt(initial_data, ground_truth, data_predicted)
  return(percentile)
}













check_input_comparison_gt_DAV <- function(initial_bn, bn_predicted){
  if (is.null(initial_bn)){
    stop("Some required parameters have not been defined: initial_bn")
  }
  if (is.null(bn_predicted)){
    stop("Some required parameters have not been defined: bn_predicted")
  }
  
}


compare_comparison_gt_DAV <- function(initial_bn, bn_predicted){
  
  delta = 0
  cont = 0
  
  for (node in names(initial_bn)){

    
    for (value in 1:length(initial_bn[[node]]$prob)){
      differenza = abs(initial_bn[[node]]$prob[value] - bn_predicted[[node]]$prob[value])
      delta = delta + differenza

    }
    
  }
  

  return(delta)
}






compare_comparison_gt_nodes_DAV <- function(initial_bn, bn_predicted,n_row_node_matrix,node_matrix){
  
  delta = 0
  
  
  for (node in names(initial_bn)){
    
    delta = 0
    for (value in 1:length(initial_bn[[node]]$prob)){
      differenza = abs(initial_bn[[node]]$prob[value] - bn_predicted[[node]]$prob[value])
      delta = delta + differenza
      
    }
    node_matrix[n_row_node_matrix, node] = delta
    
  }
  
  
  write.csv(node_matrix,'results/NODE_RESULTS_MNAR_0.2_DAV.csv')
  return(node_matrix)
}





compare_em_with_ground_truth_nodes_DAV <- function(initial_bn, bn_predicted,n_row_node_matrix,node_matrix){
  check_input_comparison_gt_DAV(initial_bn, bn_predicted)
  matrix = compare_comparison_gt_nodes_DAV(initial_bn, bn_predicted,n_row_node_matrix,node_matrix)
  return(matrix)
}

compare_em_with_ground_truth_DAV <- function(initial_bn, bn_predicted){
  check_input_comparison_gt_DAV(initial_bn, bn_predicted)
  percentile = compare_comparison_gt_DAV(initial_bn, bn_predicted)
  return(percentile)
}









library(LaplacesDemon) 






check_input_comparison_gt_KL <- function(initial_bn, bn_predicted){
  if (is.null(initial_bn)){
    stop("Some required parameters have not been defined: initial_bn")
  }
  if (is.null(bn_predicted)){
    stop("Some required parameters have not been defined: bn_predicted")
  }
  
}




compare_comparison_gt_KL <- function(initial_bn, bn_predicted){
  
  vector_initial = c()
  vector_predicted = c()
  total_sum = 0
  norm = 0
  for (node in names(initial_bn)){

    for (value in 1:length(initial_bn[[node]]$prob)){
      if (! is.na(initial_bn[[node]]$prob[value])){
        vector_initial = c(vector_initial, initial_bn[[node]]$prob[value])
        vector_predicted = c(vector_predicted, bn_predicted[[node]]$prob[value])
        
      }

      
    }
    vector_initial = as.numeric(vector_initial)
    vector_predicted = as.numeric(vector_predicted)
    kld = KLD(vector_predicted, vector_initial)
    
    total_sum = total_sum + kld$sum.KLD.py.px
    vector_initial = c()
    vector_predicted = c()
    norm = norm + 1
    
  }
  
  total_sum = total_sum 
  return(total_sum)
}

compare_comparison_gt_nodes_KL <- function(initial_bn, bn_predicted,n_row_node_matrix,node_matrix){
  
  vector_initial = c()
  vector_predicted = c()
  
  for (node in names(initial_bn)){

    for (value in 1:length(initial_bn[[node]]$prob)){
      if (! is.na(initial_bn[[node]]$prob[value])){
        vector_initial = c(vector_initial, initial_bn[[node]]$prob[value])
        vector_predicted = c(vector_predicted, bn_predicted[[node]]$prob[value])
      }
      
    }
    vector_initial = as.numeric(vector_initial)
    vector_predicted = as.numeric(vector_predicted)
    kld = KLD(vector_predicted,vector_initial)
    node_matrix[n_row_node_matrix, node] = kld$sum.KLD.py.px
    vector_initial = c()
    vector_predicted = c()
    
  }
  
  write.csv(node_matrix,'results/NODE_RESULTS_MNAR_0.01_KL_random.csv')
  return(node_matrix)
}





compare_em_with_ground_truth_nodes_KL <- function(initial_bn, bn_predicted,n_row_node_matrix,node_matrix){
  check_input_comparison_gt_KL(initial_bn, bn_predicted)
  matrix = compare_comparison_gt_nodes_KL(initial_bn, bn_predicted,n_row_node_matrix,node_matrix)
  return(matrix)
}

compare_em_with_ground_truth_KL <- function(initial_bn, bn_predicted){
  check_input_comparison_gt_KL(initial_bn, bn_predicted)
  percentile = compare_comparison_gt_KL(initial_bn, bn_predicted)
  return(percentile)
}
