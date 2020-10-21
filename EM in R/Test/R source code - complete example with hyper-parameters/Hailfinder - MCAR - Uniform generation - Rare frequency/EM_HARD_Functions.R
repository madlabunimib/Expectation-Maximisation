library(bnlearn)
library(data.table)

#Limits:
# this script does not work on different BN structures


#return true if a record has at least a missing value
is_missing_hard <- function(data) {
  for (j in rownames(data)) {
    if (is.na(initial_data[j])) {
      return(TRUE)
    }
  }
  return(FALSE)
}


#return the list of the record with at least a missing value. It is important for the expectation step
list_missing_hard <- function(initial_data) {
  lista_missing = c()
  missing = FALSE
  for (i in colnames(initial_data)) {
    for (j in rownames(initial_data)) {
      if (is.na(initial_data[j,i])) {
        missing = TRUE
      }
    }
    if (missing == TRUE) {
      lista_missing <- c(lista_missing, i)
    }
    missing = FALSE
    
    
  }
  return(lista_missing)
}


#return the list of the record with at least a missing value. It is important for the expectation step
prob_missing_hard <- function(Data,initial_data) {
  M.prob = matrix(0,nrow = nrow(Data), ncol=ncol(Data))
  dimnames(M.prob) <- list(rownames(Data),colnames(Data))
  
  for (i in 1:ncol(initial_data)) {
    for (j in 1:nrow(initial_data)) {
      if (is.na(initial_data[j,i])) {
        M.prob[j,i] = 1
      }else {
        M.prob[j,i] = "observed"
      }
    }
    
  }
  return(M.prob)
}


get_not_observed_node_hard <- function(node_to_compute, lista_missing_variable, cpt) {
  
  lista_nodes = c()
  values_node = list()
  for (i in lista_missing_variable){
    if (i != node_to_compute){
      lista_nodes = c(lista_nodes, i)
      values_node[[i]] = as.integer(row.names(cpt[[i]]))+1
    }
  }
  
  
  
  #Discretizzazione: codice corretto e testato fino a questo punto
  
  matrix_node_not_observed = do.call(CJ, values_node, FALSE)
  matrix_node_not_observed = as.matrix(matrix_node_not_observed)
  colnames(matrix_node_not_observed) = lista_nodes
  
  #colnames(matrix_to_compute) = rev(rownames(list_genitori))
  
  #cicli = ncol(matrix_node_not_observed)-1
  #for (k in 1:cicli){
  # number = cicli - k + 1
  #matrix_node_not_observed = matrix_node_not_observed[order(matrix_node_not_observed[,number],decreasing=FALSE),]
  #}
  
  return(matrix_node_not_observed)
  
}


#it computes the expectation step. It works on simply data structures
expectation_step_hard <- function(missing_data,table_posterior_prob,M.prob,Data,bn,cpt) {
  
  #compute posteriori probabilities and update dataframe x
  for (i in missing_data) {

    
    
    #Trovo nodo con missing values
    lista_missing_variable = c()
    for (j in rownames(M.prob)){
      if (M.prob[j,i] != "observed"){
        node_to_compute = j
        lista_missing_variable = c(lista_missing_variable, node_to_compute)
        #posterior prob discreizzato
      } 
    }
    
    for (node_to_compute in lista_missing_variable){
      all_evidence = TRUE
      if (length(lista_missing_variable) > 1) {
        matrix_node_not_observed = get_not_observed_node_hard(node_to_compute, lista_missing_variable,cpt)
        all_evidence = FALSE
      }
      
      
      
      posterior_values= matrix(1, nrow = 1, ncol = length(row.names(cpt[[node_to_compute]])))
      colnames(posterior_values) = row.names(cpt[[node_to_compute]])
      lista_genitori = parents(bn, names(bn[node_to_compute]))
      
      
      if (all_evidence == TRUE) {
        #itero su tutti i genitori e sui nodi osservati
        for (factor in nodes(bn)){
          
          
          
          #prior observation discretize
          prior_observation= matrix(0, nrow = 1, ncol = length(row.names(cpt[[factor]])))
          colnames(prior_observation) = row.names(cpt[[factor]])
          prior_value = FALSE
          if (M.prob[factor,i] == "observed"){
            for (k in colnames(prior_observation)){
              prior_observation[1,k] = as.numeric(Data[factor,i])+1
              prior_value = TRUE
            }
            
          }else{
            names_col = colnames(prior_observation)
            for (k in 1:ncol(prior_observation)){
              prior_observation[1,k] = as.numeric(names_col[k]) + 1
            }
          }
          
          
          
          
          lista_genitori_factor = parents(bn, names(bn[factor]))
          
          
          #Ho osservazioni su tutti i nodi padri
          if (identical(lista_genitori_factor,character(0))){
            
            for (k in colnames(posterior_values)){
              if(prior_value){
                posterior_values[1,k] = posterior_values[1,k] * as.numeric(cpt[[factor]][prior_observation[,1]])
              }else{
                posterior_values[1,k] = posterior_values[1,k] * as.numeric(cpt[[factor]][prior_observation[,k]])
              }
              
            }
            #Discretizzazione: codice corretto e testato fino a questo punto
          }else{
            
            
            genitore_factor = names(dimnames(cpt[[factor]]))
            genitore_factor = genitore_factor[-1]
            genitore_factor = rev(genitore_factor)
            # genitore_factor = rev(genitore_factor)
            
            list_genitori = matrix(0, nrow = length(dimnames(cpt[[factor]])), ncol = ncol(posterior_values)) #Da rimuovere colonne in eccesso
            names_lg = colnames((posterior_values))
            for (k in 1:length(names_lg)){
              names_lg[k] = as.numeric(names_lg[k]) + 1
            }
            colnames(list_genitori) = names_lg
            rownames(list_genitori) = names(dimnames(cpt[[factor]]))
            
            
            if(prior_value == FALSE){
              list_genitori[factor,] = prior_observation[1,]
            }else{
              list_values_for_list_genitori = c()
              for (k in 1:ncol(list_genitori)){
                list_values_for_list_genitori = c(list_values_for_list_genitori, prior_observation[1,1])
              }
              list_genitori[factor,] = list_values_for_list_genitori
            }
            
            
            
            
            cpt_lenght = length(cpt[[factor]])
            genitore_observation= matrix(0, nrow = 1, ncol = length(row.names(cpt[[node_to_compute]])))
            
            
            
            
            
            colnames(genitore_observation) = row.names(cpt[[node_to_compute]])
            
            
            list_to_compute_matrix = list()
            for (x in genitore_factor){
              
              if (M.prob[x,i] == "observed"){
                
                for (k in colnames(genitore_observation)){
                  genitore_observation[1,k] = as.numeric(Data[x,i])+1
                }
                
              }else{
                names_col = colnames(genitore_observation)
                for (k in 1:ncol(genitore_observation)){
                  genitore_observation[1,k] = as.numeric(names_col[k]) + 1
                }
              }
              
              
              
              list_genitori[x,] = genitore_observation[1,]
              list_to_compute_matrix[[x]] = as.integer(row.names(cpt[[x]]))+1
            }
            list_to_compute_matrix[[factor]] = as.integer(row.names(cpt[[factor]])) + 1
            
            
            
            #Discretizzazione: codice corretto e testato fino a questo punto
            
            matrix_to_compute = do.call(CJ, list_to_compute_matrix, FALSE)
            matrix_to_compute = as.matrix(matrix_to_compute)
            
            
            #colnames(matrix_to_compute) = rev(rownames(list_genitori))
            
            cicli = ncol(matrix_to_compute)-1
            for (k in 1:cicli){
              number = cicli - k + 1
              matrix_to_compute = matrix_to_compute[order(matrix_to_compute[,number],decreasing=FALSE),]
            }
            
            
            
            
            #Discretizzazione: codice corretto e testato fino a questo punto
            n_index = c()
            for (k in 1:ncol(list_genitori))
              n_index = c(n_index, 1)
            
            row_genitore = 1
            for (number_values in 1:length(n_index)){
              for (index_cpt in 1:nrow(matrix_to_compute)){
                lock_p0 = FALSE
                for (col in colnames(matrix_to_compute)){
                  if (as.integer(matrix_to_compute[index_cpt,col]) == list_genitori[col,row_genitore]){
                    lock_p0 = TRUE
                  }else{
                    lock_p0 = FALSE
                    break
                  }
                }
                if (lock_p0 == TRUE){
                  break
                }
                n_index[number_values] = n_index[number_values] + 1
                
                
              }
              row_genitore = row_genitore + 1
            }
            
            for (k in 1:ncol(posterior_values)){
              posterior_values[1,k] = posterior_values[1,k]*as.numeric(cpt[[factor]][n_index[k]])
            }
            
          }
          
          
        }
        
      }else{
        
        
        
        
        sum_table= matrix(0, nrow = 1, ncol = length(row.names(cpt[[node_to_compute]])))
        colnames(sum_table) = row.names(cpt[[node_to_compute]])
        
        
        
        
        
        
        for (combination in 1:nrow(matrix_node_not_observed)){
          posterior_values= matrix(1, nrow = 1, ncol = length(row.names(cpt[[node_to_compute]])))
          colnames(posterior_values) = row.names(cpt[[node_to_compute]])
          
          fit_data = Data
          fit_Mprob = M.prob
          for (column in colnames(matrix_node_not_observed)){
            fit_data[column, i] = as.integer(matrix_node_not_observed[combination, column]) - 1
            fit_Mprob[column, i] = "observed"
          }
          
          #itero su tutti i genitori e sui nodi osservati
          for (factor in nodes(bn)){
            
            
            
            #prior observation discretize
            prior_observation= matrix(0, nrow = 1, ncol = length(row.names(cpt[[factor]])))
            colnames(prior_observation) = row.names(cpt[[factor]])
            prior_value = FALSE
            if (fit_Mprob[factor,i] == "observed"){
              for (k in colnames(prior_observation)){
                prior_observation[1,k] = as.numeric(fit_data[factor,i])+1
                prior_value = TRUE
              }
              
            }else{
              names_col = colnames(prior_observation)
              for (k in 1:ncol(prior_observation)){
                prior_observation[1,k] = as.numeric(names_col[k]) + 1
              }
            }
            
            
            
            
            lista_genitori_factor = parents(bn, names(bn[factor]))
            
            
            #Ho osservazioni su tutti i nodi padri
            if (identical(lista_genitori_factor,character(0))){
              
              for (k in colnames(posterior_values)){
                if(prior_value){
                  posterior_values[1,k] = posterior_values[1,k] * as.numeric(cpt[[factor]][prior_observation[,1]])
                }else{
                  posterior_values[1,k] = posterior_values[1,k] * as.numeric(cpt[[factor]][prior_observation[,k]])
                }
                
              }
              #RAMO CASO IN CUI CI SONO PIU' MISSING VALUE PER DATO
            }else{
              
              
              genitore_factor = names(dimnames(cpt[[factor]]))
              genitore_factor = genitore_factor[-1]
              genitore_factor = rev(genitore_factor)
              # genitore_factor = rev(genitore_factor)
              
              list_genitori = matrix(0, nrow = length(dimnames(cpt[[factor]])), ncol = ncol(posterior_values)) #Da rimuovere colonne in eccesso
              names_lg = colnames((posterior_values))
              for (k in 1:length(names_lg)){
                names_lg[k] = as.numeric(names_lg[k]) + 1
              }
              colnames(list_genitori) = names_lg
              rownames(list_genitori) = names(dimnames(cpt[[factor]]))
              
              
              if(prior_value == FALSE){
                list_genitori[factor,] = prior_observation[1,]
              }else{
                list_values_for_list_genitori = c()
                for (k in 1:ncol(list_genitori)){
                  list_values_for_list_genitori = c(list_values_for_list_genitori, prior_observation[1,1])
                }
                list_genitori[factor,] = list_values_for_list_genitori
              }
              
              
              
              
              cpt_lenght = length(cpt[[factor]])
              genitore_observation= matrix(0, nrow = 1, ncol = length(row.names(cpt[[node_to_compute]])))
              
              
              
              
              
              colnames(genitore_observation) = row.names(cpt[[node_to_compute]])
              
              
              list_to_compute_matrix = list()
              for (x in genitore_factor){
                
                if (fit_Mprob[x,i] == "observed"){
                  
                  for (k in colnames(genitore_observation)){
                    genitore_observation[1,k] = as.numeric(fit_data[x,i])+1
                  }
                  
                }else{
                  names_col = colnames(genitore_observation)
                  for (k in 1:ncol(genitore_observation)){
                    genitore_observation[1,k] = as.numeric(names_col[k]) + 1
                  }
                }
                
                
                
                list_genitori[x,] = genitore_observation[1,]
                list_to_compute_matrix[[x]] = as.integer(row.names(cpt[[x]]))+1
              }
              list_to_compute_matrix[[factor]] = as.integer(row.names(cpt[[factor]])) + 1
              
              
              
              #Discretizzazione: codice corretto e testato fino a questo punto
              
              matrix_to_compute = do.call(CJ, list_to_compute_matrix, FALSE)
              matrix_to_compute = as.matrix(matrix_to_compute)
              
              
              #colnames(matrix_to_compute) = rev(rownames(list_genitori))
              
              cicli = ncol(matrix_to_compute)-1
              for (k in 1:cicli){
                number = cicli - k + 1
                matrix_to_compute = matrix_to_compute[order(matrix_to_compute[,number],decreasing=FALSE),]
              }
              
              
              
              
              #Discretizzazione: codice corretto e testato fino a questo punto
              n_index = c()
              for (k in 1:ncol(list_genitori))
                n_index = c(n_index, 1)
              
              row_genitore = 1
              for (number_values in 1:length(n_index)){
                for (index_cpt in 1:nrow(matrix_to_compute)){
                  lock_p0 = FALSE
                  for (col in colnames(matrix_to_compute)){
                    if (as.integer(matrix_to_compute[index_cpt,col]) == list_genitori[col,row_genitore]){
                      lock_p0 = TRUE
                    }else{
                      lock_p0 = FALSE
                      break
                    }
                  }
                  if (lock_p0 == TRUE){
                    break
                  }
                  n_index[number_values] = n_index[number_values] + 1
                  
                  
                }
                row_genitore = row_genitore + 1
              }
              
              for (k in 1:ncol(posterior_values)){
                posterior_values[1,k] = posterior_values[1,k]*as.numeric(cpt[[factor]][n_index[k]])
              }
              
            }
            
            
            
            
          }
          
          for (index in colnames(sum_table)){
            sum_table[1,index] = sum_table[1,index] + as.numeric(posterior_values[1,index])
            
          }
          
          
          
          
        }
        
        
        
        for (index in colnames(posterior_values)){
          posterior_values[1,index] = as.numeric(sum_table[1,index])
        }
      }
      
      
      somma_posterior = 0
      for (k in 1:ncol(posterior_values)){
        somma_posterior = somma_posterior + posterior_values[1,k] 
      }
      
      
      start = 0
      for (row_table in 1:nrow(table_posterior_prob)){
        if (table_posterior_prob[row_table,"Node"] == node_to_compute){
          start = start + 1
          break
        }
        start = start + 1
      }
      
      
      for (k in 1:ncol(posterior_values)){
        table_posterior_prob[start,i] = posterior_values[1,k]/somma_posterior
        start = start + 1
      }
      
    }
    
    
  }
  
  
  return(table_posterior_prob)
}






#Update missing values. This function works on all type of structures.
update_data_hard <- function(missing_data,table_posterior_prob,Data) {
  
  for (miss in missing_data){
    max = 0
    value = 0
    node = ""
    for (row in 1:nrow(table_posterior_prob)){
      if (table_posterior_prob[row,"Node"] == node || node == ""){
        if (table_posterior_prob[row,miss] > max ){
          max = as.numeric(table_posterior_prob[row,miss])
          value = as.integer(table_posterior_prob[row,"Value"])
          node = table_posterior_prob[row,"Node"]
          
        }
      }else{
        Data[node, miss] = value
        max = as.numeric(table_posterior_prob[row,miss])
        value = as.integer(table_posterior_prob[row,"Value"])
        node = table_posterior_prob[row,"Node"]
      }
      
    }
    Data[node, miss] = value
  }
  
  return(Data)
  
}




#Works very well on every BN structures
stopping_criteria_hard <- function(alpha,cpt_last, cpt, bn) {
  delta = 0
  for (node in names(bn)){
    for (value in 1:length(cpt[[node]])){
      differenza = abs(cpt[[node]][value] - cpt_last[[node]][value])
      delta = delta + differenza
    }
  }
  if (delta > alpha){
    return(FALSE)
    
  }else{
    return(TRUE)
  }
  
}







maximisation_step_hard <- function(node,bn,table_posterior_prob,Data,cpt) {
  parents = parents(bn, names(bn[node]))
  
  
  #Indirizzamento corretto alle CPT
  column_cpt = c()
  val = 1
  for (index in rownames(cpt[[node]])){
    column_cpt = c(column_cpt,val)
    val = val + 1
  }
  
  #controllo se nodo è condizionato da altri nodi
  if (identical(parents,character(0))) {
    
    
    numeratore = matrix(0, nrow = 1, ncol = length(column_cpt))
    colnames(numeratore) = rownames(cpt[[node]])
    
    denominatore= 0
    
    
    
    
    for (i in colnames(Data)) {
      for (np in colnames(numeratore)){
        if (Data[node,i] == as.numeric(np)) {
          numeratore[1,np] = numeratore[1,np] + 1
        }
      }
      denominatore = denominatore + 1
    }
    
    
    for (i in 1:length(numeratore)){
      cpt[[node]][i] = numeratore[1,i]/denominatore
    }
  }else{
    
    list_to_compute_matrix = list()
    for (x in parents){
      list_to_compute_matrix[[x]] = as.integer(row.names(cpt[[x]]))
    }
    
    matrix_to_compute = do.call(CJ, list_to_compute_matrix, FALSE)
    matrix_to_compute = as.matrix(matrix_to_compute)
    colnames(matrix_to_compute) = parents
    
    row.names.matrix = c()
    for (index in 1:nrow(matrix_to_compute)){
      row.names.matrix[index] = index
    }
    rownames(matrix_to_compute) = row.names.matrix
    
    values_node = nrow(cpt[[node]])
    values_list = c()
    
    for (vn in 1:values_node){
      values_list = c(values_list, (vn-1))
    }
    
    
    number_parents = ncol(matrix_to_compute)
    stopifnot(number_parents == length(parents))
    compute_probabilities = matrix(0, nrow = nrow(matrix_to_compute), ncol = (ncol(matrix_to_compute)+values_node + 1)) #Da rimuovere colonne in eccesso
    colnames(compute_probabilities) = c(colnames(matrix_to_compute), values_list, "denominator")
    rownames(compute_probabilities) = rownames(matrix_to_compute)
    
    for(index in colnames(matrix_to_compute)){
      compute_probabilities[,index] = matrix_to_compute[,index]
    }
    
    
    for(data.index in 1:ncol(Data)){
      for(row.index in 1:nrow(compute_probabilities)){
        is_correct_row = TRUE
        for(parent.variable in parents){
          if(compute_probabilities[row.index,parent.variable] != Data[parent.variable,data.index]){
            is_correct_row = FALSE
          }
          
          
        }
        if(is_correct_row){
          value = as.numeric(Data[node,data.index])
          compute_probabilities[row.index,as.character(value)] = compute_probabilities[row.index,as.character(value)] + 1
          compute_probabilities[row.index,"denominator"] = compute_probabilities[row.index,"denominator"] + 1
        }
        
        
      }
    }
    
    for(j in rownames(compute_probabilities)){
      
      
      if(compute_probabilities[j,"denominator"] != 0){
        
        
        list_final_probabilities = c()
        for (value_to_update in values_list){
          
          list_final_probabilities = c(list_final_probabilities, as.numeric(compute_probabilities[j,as.character(value_to_update)] / compute_probabilities[j,"denominator"]))
          
        }
        
        cpt[[node]][column_cpt] = c(list_final_probabilities)
        
      }
      
      for (cc in 1:length(column_cpt)){
        column_cpt[cc] = column_cpt[cc] + nrow(cpt[[node]])
      }
      
    }
  }
  
  return(cpt)
  
}





#Update missing values. This function works on all type of structures. only binary data
Create_matrix_posterior_prob_hard <- function(Data, missing_data, M.prob,bn) {
  
  number_missing_data = length(missing_data)
  
  row_number = 0
  row_names_table = c()
  row_values_table = c()
  for (node in names(bn)){
    add = length(row.names(cpt[[node]]))
    row_number = row_number + add
    
    for (i in 1:add){
      row_names_table = c(row_names_table, node)
    }
    for (i in row.names(cpt[[node]])){
      row_values_table = c(row_values_table, i)
    }
    
  }
  
  
  tables_posterior_probabilities = matrix(0, nrow = row_number, ncol = number_missing_data + 2) #Da rimuovere colonne in eccesso
  colnames(tables_posterior_probabilities) = c("Node", "Value", missing_data)
  tables_posterior_probabilities[,"Node"] = row_names_table
  tables_posterior_probabilities[,"Value"] = row_values_table
  
  return(tables_posterior_probabilities)
}



initialize_matrix_posterior_prob_hard <- function(Data, missing_data, M.prob,table_posterior_prob,bn){
  for (i in missing_data){
    for (j in row.names(M.prob)){
      if (M.prob[j,i] == "observed"){
        for (k in 1:nrow(table_posterior_prob)){
          if (table_posterior_prob[k,1] == j){
            table_posterior_prob[k,i] = 0
          }
        }
        
      } else{
        for (k in 1:nrow(table_posterior_prob)){
          if (table_posterior_prob[k,1] == j){
            table_posterior_prob[k,i] = 1
          }
        }
        
      }
      
    }
    
  }
  
  return(table_posterior_prob)
}



check_input_hard <- function(initial_data,cpt,ALPHA,NI){
  if (is.null(initial_data)){
    stop("Some required parameters have not been defined: DATA")
  }
  
  if (is.null(cpt)){
    stop("Some required parameters have not been defined: CPT")
  }
  
  if (typeof(initial_data) != "list"){
    stop("Initial_data param must be a list (data.frame)")
  }
  
  if(ALPHA <= 0){
    stop("ALPHA must be a positive value")
  }
  if(NI <= 0 || NI > 1000){
    stop("Number interaction must be a integer value ranges from 1 to 1000")
  }
  
  
}

standardize_data_hard <- function(initial_data,cpt){
  if(length(cpt) == ncol(initial_data) && names(cpt[1]) %in% colnames(initial_data)){
    initial_data <- data.frame(t(initial_data), stringsAsFactors = FALSE)
  }else if(length(cpt) == nrow(initial_data) && names(cpt[1]) %in% rownames(initial_data)){
  }else{
    stop("Input Data non compatible with cpt structure. Please check that all cpt names matches with header of the Data")
  }
  
  
  if (typeof(initial_data[1,1]) == "character"){
    names_row = rownames(initial_data)
    initial_data <- as.data.frame(sapply(initial_data, as.numeric))
    Data <- data.frame(initial_data)
    rownames(Data) = names_row
    Data[is.na(Data)] <-as.numeric(0.5)
    Data[Data=="0"]<-as.numeric(0)
    Data[Data=="1"]<-as.numeric(1)
  }
  else{
    Data <- data.frame(initial_data)
    Data[is.na(Data)] <-as.numeric(0.5)
    Data[Data=="0"]<-as.numeric(0)
    Data[Data=="1"]<-as.numeric(1)
  }
  
  return(Data)
}

em_hard <- function(initial_data, structure, cpt, ALPHA=0.05, NUMBER_ITERACTION=20){
  NUMBER_ITERACTION = as.integer(NUMBER_ITERACTION)
  check_input_hard(initial_data,cpt,ALPHA, NUMBER_ITERACTION)
  Data = standardize_data_hard(initial_data,cpt)
  
  
  cpt_last = cpt
  bn = custom.fit(structure,cpt)
  
  #variable that contains all the missing variables (important to expectation step)
  missing_data = list_missing_hard(initial_data)
  M.prob = prob_missing_hard(Data,initial_data)
  
  #Inizio programma
  table_posterior_prob = Create_matrix_posterior_prob_hard(Data,missing_data, M.prob,bn)
  table_posterior_prob = initialize_matrix_posterior_prob_hard(Data,missing_data, M.prob,table_posterior_prob,bn)
  
  
  for (i in 1:NUMBER_ITERACTION) {
    

    table_posterior_prob = expectation_step_hard(missing_data,table_posterior_prob,M.prob,Data,bn,cpt)
    Data = update_data_hard(missing_data,table_posterior_prob,Data)

    for (node in names(bn)){
      cpt = maximisation_step_hard(node,bn,table_posterior_prob,Data,cpt)
    }

    stop_criteria = stopping_criteria_hard(ALPHA, cpt_last, cpt,bn)
    
    if (stop_criteria == TRUE){
      bn = custom.fit(structure,cpt)

      break
    } else{
      cpt_last = cpt
      bn = custom.fit(structure,cpt)
    }
    
    
    
  }
  return(bn)
}



