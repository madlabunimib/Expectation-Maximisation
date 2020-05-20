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
    posterior_0 = 1
    posterior_1 = 1
    #Trovo nodo con missing values
    for (j in rownames(M.prob)){
      if (M.prob[j,i] != "observed"){
        node_to_compute = j
        lista_genitori = parents(bn, names(bn[node_to_compute]))
        
        break
      } 
    }
    
    
    #itero su tutti i genitori e sui nodi osservati
    for (factor in nodes(bn)){
      if (M.prob[factor,i] == "observed"){
        prior_observation = c(as.numeric(Data[factor,i])+1,as.numeric(Data[factor,i])+1)
      }else{
        prior_observation = c(1,2)
      }
      
      lista_genitori_factor = parents(bn, names(bn[factor]))
      
      
      #Ho osservazioni su tutti i nodi padri
      if (identical(lista_genitori_factor,character(0))){
        posterior_0 = posterior_0 * as.numeric(cpt[[factor]][prior_observation[1]])
        posterior_1 = posterior_1 * as.numeric(cpt[[factor]][prior_observation[2]])
      }else{
        
        genitore_factor = parents(bn, factor)
        
        
        list_genitori = matrix(0, nrow = length(dimnames(cpt[[factor]])), ncol = 2) #Da rimuovere colonne in eccesso
        colnames(list_genitori) = c(1,2)
        rownames(list_genitori) = names(dimnames(cpt[[factor]]))
        list_genitori[factor,] = prior_observation
        
        
        cpt_lenght = length(cpt[[factor]])
        for (x in genitore_factor){
          if (M.prob[x,i] == "observed"){
            genitore_observation = c(as.numeric(Data[x,i])+1,as.numeric(Data[x,i])+1)
          }else{
            genitore_observation = c(1,2)
          }
          
          list_genitori[x,] = genitore_observation
          
        }
        
        matrix_to_compute = do.call(CJ, replicate(nrow(list_genitori), 1:2, FALSE))
        matrix_to_compute = as.matrix(matrix_to_compute)
        colnames(matrix_to_compute) = rev(rownames(list_genitori))
        n_0 = 1
        n_1 = 1
        for (index_cpt in 1:nrow(matrix_to_compute)){
          lock_p0 = FALSE
          for (col in colnames(matrix_to_compute)){
            if (as.integer(matrix_to_compute[index_cpt,col]) == list_genitori[col,1]){
              lock_p0 = TRUE
            }else{
              lock_p0 = FALSE
              break
            }
          }
          if (lock_p0 == TRUE){
            break
          }
          n_0 = n_0 + 1 
          
          
        }
        
        for (index_cpt in 1:nrow(matrix_to_compute)){
          lock_p1 = FALSE
          for (col in colnames(matrix_to_compute)){
            if (as.integer(matrix_to_compute[index_cpt,col]) == list_genitori[col,2]){
              lock_p1 = TRUE
            }else{
              lock_p1 = FALSE
              break
            }
          }
          if (lock_p1 == TRUE){
            break
          }
          n_1 = n_1 + 1 
          
          
        }
        
        
        posterior_0 = posterior_0 * as.numeric(cpt[[factor]][n_0])
        posterior_1 = posterior_1 * as.numeric(cpt[[factor]][n_1])      
        
        
      }
      
      
    }
    
    M.prob[node_to_compute,i] = posterior_0/ (posterior_0 + posterior_1)
    
    
  }
  return(M.prob)
}






#Update missing values. This function works on all type of structures. only binary data
update_data <- function() {
  for (i in rownames(M.prob)) {
    for (j in colnames(M.prob)) {
      if (M.prob[i,j] != "observed") {
        if (as.numeric(M.prob[i,j]) >= 0.5) {
          Data[i,j] = 0
        }else{
          Data[i,j] = 1
        }
        
      }
    }
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
    
    
    column_cpt = c(1,2)
    lista_genitori = parents(bn, names(bn[k]))
    node = names(bn[k])
    
    if (node == "C"){
      ok = 1
    }
    #controllo se nodo è condizionato da altri nodi
    if (identical(lista_genitori,character(0))) {
      j = node
      #update prior probabilities
      numerator_a_0 = 0
      numerator_a_1 = 0
      n = 0
      
      for (i in colnames(Data)) {
        if (Data[j,i] == 0 && M.prob[j,i] == "observed") {
          numerator_a_0 = numerator_a_0 + 1
        } else if (Data[j,i] == 1 && M.prob[j,i] == "observed") {
          numerator_a_1 = numerator_a_1 + 1
        } else  {
          numerator_a_0 = numerator_a_0 + as.numeric(M.prob[j,i])
          numerator_a_1 = numerator_a_1 + (1 - as.numeric(M.prob[j,i]))
        }
        n = n + 1
      }
      
      
      P_0 = numerator_a_0 / n
      P_1 = numerator_a_1 / n
      
      cpt[[node]][1] = P_0
      cpt[[node]][2] = P_1
      #se non è condizionato  
    }else{
      genitore = parents(bn, node)
      matrix_to_compute = do.call(CJ, replicate(length(parents(bn,node)), 0:1, FALSE))
      matrix_to_compute = as.matrix(matrix_to_compute)
      colnames(matrix_to_compute) = genitore
      
      namesmc = c()
      for (indexmc in 1:nrow(matrix_to_compute)){
        namesmc[indexmc] = indexmc
      }
      rownames(matrix_to_compute) = namesmc
      
      
      
      
      compute_probabilities = matrix(0, nrow = nrow(matrix_to_compute), ncol = 4) #Da rimuovere colonne in eccesso
      colnames(compute_probabilities) = c("numerator", "denominator", "0_prob", "1_prob")
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
            if (Data[node,i] == 0 && M.prob[node,i] == "observed") {
              compute_probabilities[j,"numerator"] = as.numeric(compute_probabilities[j,"numerator"]) + 1
              compute_probabilities[j,"denominator"] = as.numeric(compute_probabilities[j,"denominator"]) + 1
            }
            if (Data[node,i] == 1 && M.prob[node,i] == "observed") {
              compute_probabilities[j,"denominator"] = as.numeric(compute_probabilities[j,"denominator"]) + 1
            }
            if (M.prob[node,i] != "observed") {
              compute_probabilities[j,"numerator"] = as.numeric(compute_probabilities[j,"numerator"])  + as.numeric(M.prob[node,i])
              compute_probabilities[j,"denominator"] = as.numeric(compute_probabilities[j,"denominator"]) + 1
            }
          }
          
          
          
          
          
          #Se altri nodi sono missing - Si entra in questo if quando un nodo è missing
          if (missing == TRUE) {
            
            
            
            if (Data[node,i] == 0) {
              
              compute_probabilities[j,"numerator"] = compute_probabilities[j,"numerator"] + abs(matrix_to_compute[j,genitore_missing] - as.numeric(M.prob[genitore_missing,i]))
              
              compute_probabilities[j,"denominator"]  = compute_probabilities[j,"denominator"]  + abs(matrix_to_compute[j,genitore_missing] - as.numeric(M.prob[genitore_missing,i]))
              
            }
            if (Data[node,i] == 1) {
              compute_probabilities[j,"denominator"]  = compute_probabilities[j,"denominator"]  + abs(matrix_to_compute[j,genitore_missing] - as.numeric(M.prob[genitore_missing,i]))
            }
            
          }
          
        }
        
        
        
        if(compute_probabilities[j,"denominator"] != 0){
          compute_probabilities[j,"0_prob"] = compute_probabilities[j,"numerator"] / compute_probabilities[j,"denominator"]
          
          compute_probabilities[j,"1_prob"] = 1 - compute_probabilities[j,"0_prob"]
          
          
          cpt[[node]][column_cpt] = c(as.numeric(compute_probabilities[j,"0_prob"]), as.numeric(compute_probabilities[j,"1_prob"]))
          
        }
        
        
        
        column_cpt[1] = column_cpt[1] + 2
        column_cpt[2] = column_cpt[2] + 2
        
      }
      
      
      
    }
  }
  
  
  return(cpt)
  
  
}






#Initial data
initial_data = data.frame("Data_1" = c(0,1,0), "Data_2" = c(0,0,"NA"), "Data_3" = c("NA",0,1), "Data_4" = c(0,"NA",0), "Data_5" = c(1,0,0), "Data_6" = c(0,1,0), "Data_7" = c("NA",1,1), "Data_8" = c(1,0,"NA"), "Data_9" = c(1,1,"NA"), "Data_10" = c(0, "NA", 1), "Data_11" = c(1,1,1), "Data_12" = c(0,1,0), "Data_13" = c(0,0,1), "Data_14" = c(1,0,1), "Data_15" = c(1,"NA",1), "Data_16" = c(1,1,"NA"), "Data_17" = c(0,"NA",1), "Data_18" = c("NA", 1, 0), "Data_19" = c(0,1,1), "Data_20" = c(0,1,1), "Data_21" = c(0,0,0),  row.names = c("A", "B", "C"))
#, "Data_5" = c("NA",0,0), "Data_6" = c("NA",0,1), "Data_7" = c(1,0,1), "Data_8" = c(1,0,1), "Data_9" = c(1,1,"NA"), "Data_10" = c(1,"NA",0),

#Updates data
Data = data.frame("Data_1" = c(0,1,0), "Data_2" = c(0,0,0.5), "Data_3" = c(0.5,0,1), "Data_4" = c(0,0.5,0), "Data_5" = c(1,0,0), "Data_6" = c(0,1,0), "Data_7" = c(0.5,1,1), "Data_8" = c(1,0,0.5), "Data_9" = c(1,1,0.5), "Data_10" = c(0, 0.5, 1), "Data_11" = c(1,1,1), "Data_12" = c(0,1,0), "Data_13" = c(0,0,1), "Data_14" = c(1,0,1), "Data_15" = c(1,0.5,1), "Data_16" = c(1,1,0.5),  "Data_17" = c(0,0.5,1), "Data_18" = c(0.5, 1, 0), "Data_19" = c(0,1,1), "Data_20" = c(0,1,1), "Data_21" = c(0,0,0), row.names = c("A", "B", "C"), stringsAsFactors = TRUE)
#"Data_5" = c(0.5,0,0), "Data_6" = c(0.5,0,1), "Data_7" = c(1,0,1), "Data_8" = c(1,0,1), "Data_9" = c(1,1,0.5), "Data_10" = c(1,0.5,0), 

bn_data = model2network("[A][B|A][C|B:A]")

#variable that contains all the missing variables (important to expectation step)
lv = c("0", "1")
missing_data = list_missing(initial_data)
M.prob = prob_missing()



#STEP 1 - INIT - only on initial data
A.prob = matrix(c(0.5, 0.5), dimnames = list(A = lv))
B.prob = matrix(c(1/3, 2/3, 2/3, 1/3 ), ncol=2,
               dimnames = list(B = lv, A = lv))
C.prob = c(0.5, 0.5, 0.4, 0.6, 0.3, 0.7, 0.2, 0.8)
dim(C.prob) = c(2, 2, 2)
dimnames(C.prob) = list(C = lv, B = lv, A = lv)

cpt = list(A = A.prob, B = B.prob, C = C.prob)
cpt_last = cpt
bn = custom.fit(bn_data,cpt)





for (i in 1:NUMBER_ITERACTION) {
  M.prob = expectation_step()
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

