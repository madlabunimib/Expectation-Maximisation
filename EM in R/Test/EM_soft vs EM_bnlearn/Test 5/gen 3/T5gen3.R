library(bnlearn)
library(data.table)

NUMBER_ITERACTION = 1000
ALPHA = 0.001
#Limits:
# this script does not work on different BN structures


#return true if a record has at least a missing value
is_missing<- function(data) {
  for (j in rownames(data)) {
    if (data[j] == "NA") {
      return(TRUE)
    }
  }
  return(FALSE)
}


#return the list of the record with at least a missing value. It is important for the expectation step
list_missing<- function(initial_data) {
  lista_missing = c()
  missing = FALSE
  for (i in colnames(initial_data)) {
    for (j in rownames(initial_data)) {
      if (initial_data[j,i] == "NA") {
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
prob_missing <- function() {
  M.prob = matrix(0,nrow = nrow(Data), ncol=ncol(Data))
  dimnames(M.prob) <- list(rownames(Data),colnames(Data))
  
  for (i in colnames(initial_data)) {
    for (j in rownames(initial_data)) {
      if (initial_data[j,i] == "NA") {
        M.prob[j,i] = 1
      }else {
        M.prob[j,i] = "observed"
      }
    }
    
  }
  return(M.prob)
}




#it computes the expectation step. It works on simply data structures
expectation_step <- function() {
  
  #compute posteriori probabilities and update dataframe x
  for (i in missing_data) {
    
    #Trovo nodo con missing values
    for (j in rownames(M.prob)){
      if (M.prob[j,i] != "observed"){
        node_to_compute = j
        
        #posterior prob discreizzato
        posterior_values= matrix(1, nrow = 1, ncol = length(row.names(cpt[[node_to_compute]])))
        colnames(posterior_values) = row.names(cpt[[node_to_compute]])
        lista_genitori = parents(bn, names(bn[node_to_compute]))
        break
      } 
    }
    
    
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
  return(table_posterior_prob)
}






#Update missing values. This function works on all type of structures.
update_data <- function() {
  
  for (miss in missing_data){
    max = 0
    value = 0
    node = ""
    for (row in 1:nrow(table_posterior_prob)){
      if (as.numeric(table_posterior_prob[row,miss]) > max){
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
stopping_criteria <- function(alpha) {
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









maximisation_step <- function() {
  #ciclo su tutti i nodi della rete
  for (k in 1:length(bn)) {
    
    
    
    
    lista_genitori = parents(bn, names(bn[k]))
    node = names(bn[k])
    
    if (node == "E"){
      ok= 2
    }
    
    column_cpt = c()
    val = 1
    for (index in rownames(cpt[[node]])){
      column_cpt = c(column_cpt,val)
      val = val + 1
    }
    
    
    
    
    #controllo se nodo è condizionato da altri nodi
    if (identical(lista_genitori,character(0))) {
      j = node
      
      
      
      numerator_prior= matrix(0, nrow = 1, ncol = length(column_cpt))
      colnames(numerator_prior) = rownames(cpt[[node]])
      
      denominator = 0
      
      
      
      
      for (i in colnames(Data)) {
        for (np in colnames(numerator_prior)){
          if (Data[j,i] == np && M.prob[j,i] == "observed") {
            numerator_prior[1,np] = numerator_prior[1,np] + 1
          } else if (M.prob[j,i] != "observed"){
            
            
            for (tpp in 1:nrow(table_posterior_prob)){
              if (table_posterior_prob[tpp, "Node"] == j && table_posterior_prob[tpp, "Value"] == np)
                numerator_prior[1,np] = numerator_prior[1,np] + as.numeric(table_posterior_prob[tpp, i])
              
            }
            
            
            
            
          }
          
        }
        denominator = denominator + 1
      }
      
      
      for (i in 1:length(numerator_prior)){
        cpt[[node]][i] = numerator_prior[1,i]/denominator
      }
      
      
      
      
      
      #TESTATO FIN QUI. TUTTO FUNZIONANTE e corretto 
      
      #se node è condizionato  
    }else{
      genitore = names(dimnames(cpt[[node]]))
      genitore = genitore[-1]
      genitore = rev(genitore)
      
      
      list_to_compute_matrix = list()
      for (x in genitore){
        list_to_compute_matrix[[x]] = as.integer(row.names(cpt[[x]]))
      }
      #list_to_compute_matrix[[node]] = as.integer(row.names(cpt[[node]])) + 1
      
      
      #Discretizzazione: codice corretto e testato fino a questo punto
      
      matrix_to_compute = do.call(CJ, list_to_compute_matrix, FALSE)
      matrix_to_compute = as.matrix(matrix_to_compute)
      colnames(matrix_to_compute) = genitore
      
      
      
      namesmc = c()
      for (indexmc in 1:nrow(matrix_to_compute)){
        namesmc[indexmc] = indexmc
      }
      rownames(matrix_to_compute) = namesmc
      
      
      
      values_node = nrow(cpt[[node]])
      values_list = c()
      
      for (vn in 1:values_node){
        values_list = c(values_list, (vn-1))
      }
      
      compute_probabilities = matrix(0, nrow = nrow(matrix_to_compute), ncol = (values_node + 1)) #Da rimuovere colonne in eccesso
      colnames(compute_probabilities) = c(values_list, "denominator")
      rownames(compute_probabilities) = rownames(matrix_to_compute)
      
      
      
      
      #ciclo su tutte le combinazioni
      for (j in rownames(matrix_to_compute)) { 
        
        
        for (i in colnames(Data)) {
          
          
          out = FALSE
          missing = FALSE
          
          for (parent in genitore){
            if (Data[parent,i] == matrix_to_compute[j,parent] && M.prob[parent,i] == "observed"){
              out = TRUE
            } else if (!M.prob[parent,i] == "observed"){
              missing = TRUE
              genitore_missing = parent
            } else {
              out = FALSE
              missing = FALSE
              break
            }
          }
          #dato nodo missing - Da generalizzare da qui
          if (out == TRUE && missing == FALSE ) {
            
            
            
            for(vn in 1:values_node){
              if (Data[node,i] == vn-1 && M.prob[node,i] == "observed") {
                compute_probabilities[j,vn] = as.numeric(compute_probabilities[j,vn]) + 1
                compute_probabilities[j,"denominator"] = as.numeric(compute_probabilities[j,"denominator"]) + 1
              }
              
            }
            
            
            
            if (M.prob[node,i] != "observed") {
              
              for(vn in 1:values_node){
                for(tpp in 1:nrow(table_posterior_prob)){
                  if (table_posterior_prob[tpp, "Node"] == node && table_posterior_prob[tpp, "Value"] == vn-1){
                    index_comp = tpp
                    break
                  }
                }
                
                
                compute_probabilities[j,vn] = as.numeric(compute_probabilities[j,vn])  + as.numeric(table_posterior_prob[index_comp, i])
              }
              
              compute_probabilities[j,"denominator"] = as.numeric(compute_probabilities[j,"denominator"]) + 1
            }
            
            
            
          }
          
          
          
          #Se altri nodi sono missing - Si entra in questo if quando un nodo è missing
          if (missing == TRUE) {
            
            
            
            for (vn in 1:values_node){
              
              
              if (Data[node,i] == vn-1) {
                value_to_search = matrix_to_compute[j,genitore_missing] + 1
                for(tpp in 1:nrow(table_posterior_prob)){
                  if (table_posterior_prob[tpp, "Node"] == genitore_missing && (as.integer(table_posterior_prob[tpp, "Value"])+1) == value_to_search){
                    index_comp = tpp
                    break
                  }
                }
                
                compute_probabilities[j,vn] = compute_probabilities[j,vn] + as.numeric(table_posterior_prob[index_comp,i])
                compute_probabilities[j,"denominator"]  = compute_probabilities[j,"denominator"]  + as.numeric(table_posterior_prob[index_comp,i])
                
              }
              
              
              
            }
            
            
            
            
            
          }
          
        }
        
        
        
        
        if(compute_probabilities[j,"denominator"] != 0){
          
          list_final_probabilities = c()
          for (vn in 1:values_node){
            list_final_probabilities = c(list_final_probabilities, 0)
          }
          
          for (lfp in 1:length(list_final_probabilities)){
            
            list_final_probabilities[lfp] = as.numeric(compute_probabilities[j,lfp] / compute_probabilities[j,"denominator"])
            
            
          }
          
          cpt[[node]][column_cpt] = c(list_final_probabilities)
          
          
          
          
        }
        
        
        for (cc in 1:length(column_cpt)){
          column_cpt[cc] = column_cpt[cc] + nrow(cpt[[node]])
        }
        
        
      }
      
      
      
    }
  }
  
  
  return(cpt)
  
  
}












#Update missing values. This function works on all type of structures. only binary data
Create_matrix_posterior_prob <- function() {
  
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


initialize_matrix_posterior_prob <- function(){
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



#Initial data
initial_data = data.frame("Data_1" = c(0,"NA",1), "Data_2" = c(1,"NA",0), "Data_3" = c(0,1,0), "Data_4" = c(0,"NA",0), "Data_5" = c(1,"NA",0), "Data_6" = c(1,"NA",0), "Data_7" = c(0,"NA",1), row.names = c("A", "B", "C"))
#, "Data_5" = c("NA",0,0), "Data_6" = c("NA",0,1), "Data_7" = c(1,0,1), "Data_8" = c(1,0,1), "Data_9" = c(1,1,"NA"), "Data_10" = c(1,"NA",0),

#Updates data
Data = data.frame("Data_1" = c(0,0.5,1), "Data_2" = c(1,0.5,0), "Data_3" = c(0,1,0), "Data_4" = c(0,0.5,0), "Data_5" = c(1,0.5,0), "Data_6" = c(1,0.5,0), "Data_7" = c(0,0.5,1), row.names = c("A", "B", "C"), stringsAsFactors = TRUE)
#"Data_5" = c(0.5,0,0), "Data_6" = c(0.5,0,1), "Data_7" = c(1,0,1), "Data_8" = c(1,0,1), "Data_9" = c(1,1,0.5), "Data_10" = c(1,0.5,0), 

bn_data = model2network("[A][B|A][C|B]")

#variable that contains all the missing variables (important to expectation step)
lv = c("0", "1")
missing_data = list_missing(initial_data)
M.prob = prob_missing()



#STEP 1 - INIT - only on initial data
A.prob = matrix(c(0.5, 0.5), dimnames = list(A = lv))
B.prob = matrix(c(1/3, 2/3, 2/3, 1/3 ), ncol=2,
                dimnames = list(B = lv, A = lv))
C.prob = matrix(c(0.3, 0.7, 0.6, 0.4 ), ncol=2,
                dimnames = list(C = lv, B = lv))

cpt = list(A = A.prob, B = B.prob, C = C.prob)
cpt_last = cpt
bn = custom.fit(bn_data,cpt)




#Inizio programma
table_posterior_prob = Create_matrix_posterior_prob()
table_posterior_prob = initialize_matrix_posterior_prob()


for (i in 1:NUMBER_ITERACTION) {
  table_posterior_prob = expectation_step()
  Data = update_data()
  cpt = maximisation_step()
  stop_criteria = stopping_criteria(ALPHA)
  
  if (stop_criteria == TRUE){
    cat(sprintf("Uscita allo step: %s\n", i ))
    cat(sprintf("Convergenza dell'algoritmo allo step: %s\n", i-1 ))
    print(bn)
    break
  }
  else{
    cpt_last = cpt
    bn = custom.fit(bn_data,cpt)
  }
  
  
  
}

