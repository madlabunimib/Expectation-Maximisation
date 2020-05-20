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
        if (M.prob[genitore_factor,i] == "observed"){
          genitore_observation = c(as.numeric(Data[genitore_factor,i])+1,as.numeric(Data[genitore_factor,i])+1)
        }else{
          genitore_observation = c(1,2)
        }
        
        posterior_0 = posterior_0 * as.numeric(cpt[[factor]][prior_observation[1],genitore_observation[1]])
        posterior_1 = posterior_1 * as.numeric(cpt[[factor]][prior_observation[2],genitore_observation[2]])
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
  for (i in names(cpt)) {
    #nodo prior
    if (length(cpt[[i]]) == 2){
      delta = delta + abs(as.numeric(cpt_last[[i]][1]) - as.numeric(cpt[[i]][1]))
    }else{
      cond = length(cpt[[i]])/2
      num = 1
      #determino numero di genitori di quel nodo. In futuro è migliorabile
      while (cond != 2) {
        cond = cond/2
        num = num + 1
        
      }
      
      for (j in 1:num) {
        delta = delta + abs(as.numeric(cpt_last[[i]][1,j]) - as.numeric(cpt[[i]][1,j]))
      }
    }
    

  }
  if (delta <= alpha){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
  
}




maximisation_step <- function() {
  #ciclo su tutti i nodi della rete
  for (k in 1:length(bn)) {
    lista_genitori = parents(bn, names(bn[k]))
    node = names(bn[k])
    #controllo se nodo è condizionato da altri nodi
    if (identical(lista_genitori,character(0))) {
      j = node
      #update each CPT_A probabilities
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
      for (j in 0:1) { #valori binari. Calcolo prima P(B=0|A=0) e P(B=1|A=0) e solo successivamente P(B=0|A=1) e P(B=1|A=1)
        numerator = 0
        denominator = 0
        for (i in colnames(Data)) {
          
          #dato B missing
          if (Data[genitore,i] == j && M.prob[genitore,i] == "observed") {
            if (Data[node,i] == 0 && M.prob[node,i] == "observed") {
              numerator = numerator + 1
              denominator = denominator + 1
            }
            if (Data[node,i] == 1 && M.prob[node,i] == "observed") {
              denominator = denominator + 1
            }
            if (M.prob[node,i] != "observed") {
              numerator = numerator + as.numeric(M.prob[node,i])
              denominator = denominator + 1
            }
          }
          
          
          #Se il dato A è missing
          if (M.prob[genitore,i] != "observed") {
            if (Data[node,i] == 0) {
              numerator = numerator + abs(j - as.numeric(M.prob[genitore,i]))
              denominator = denominator + abs(j - as.numeric(M.prob[genitore,i]))
            }
            if (Data[node,i] == 1) {
              denominator = denominator + abs(j - as.numeric(M.prob[genitore,i]))
            }
            
          }
        }
        cpt[[node]][1,j+1] = numerator / denominator
        cpt[[node]][2,j+1] = 1 - cpt[[node]][1,j+1]
      }
    }
  }
  
  
  return(cpt)
  
  
}








#Initial data
initial_data = data.frame("Data_1" = c(0,0), "Data_2" = c(0,1), "Data_3" = c(0,1), "Data_4" = c("NA",1), "Data_5" = c("NA",0), "Data_6" = c("NA",0), "Data_7" = c(1,0), "Data_8" = c(1,0), "Data_9" = c(1,1), "Data_10" = c(1,"NA"), row.names = c("A", "B"))

#Updates data
Data = data.frame("Data_1" = c(0,0), "Data_2" = c(0,1), "Data_3" = c(0,1), "Data_4" = c(0.5,1), "Data_5" = c(0.5,0), "Data_6" = c(0.5,0), "Data_7" = c(1,0), "Data_8" = c(1,0), "Data_9" = c(1,1), "Data_10" = c(1,0.5), stringsAsFactors = TRUE, row.names = c("A", "B"))
library(bnlearn)
bn_data = model2network("[A][B|A]")

#variable that contains all the missing variables (important to expectation step)
lv = c("0", "1")
missing_data = list_missing(initial_data)
M.prob = prob_missing()



#STEP 1 - INIT - only on initial data
A.prob = array(c(0.5, 0.5), dim = 2, dimnames = list(A = lv))
B.prob = array(c(1/3, 2/3, 2/3, 1/3 ), dim = c(2, 2),
               dimnames = list(B = lv, A = lv))


cpt = list(A = A.prob, B = B.prob)
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













"'
#Compute conditional probability P(B|A) given the observations 
#Count A = 0
count_A_0 = 0
numerator_B_0 = 0
numerator_B_1 = 0

for (i in colnames(initial_data)) {
  if (!is_missing(initial_data[,i])){
    
    if (initial_data['A', i] == 0) {
      count_A_0 = count_A_0 + 1
      if (initial_data[B', i] == 0) {
        numerator_B_0 = numerator_B_0 + 1
      }
      if (initial_data['B', i] == 1) {
        numerator_B_1 = numerator_B_1 + 1
      }
    }
    
  }
  
}

P_B_0_A_0 = numerator_B_0 / count_A_0
P_B_1_A_0 = numerator_B_1 / count_A_0


B.prob = array(c(P_B_0_A_0, P_B_1_A_0, 1 - P_B_0_A_0, 1 - P_B_1_A_0), dim = c(2, 2),
               dimnames = list(B = lv, A = lv))
'"


#Risultati uguali alle slide ma diversa da bnlearn
#versione becera vs versione non becera

