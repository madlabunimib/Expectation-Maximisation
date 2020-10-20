library(bnlearn)
library(mice)
source("EM_HARD_Functions.R")
source("EM_SOFT_Functions.R")
source("EM_COMPARISON.R")
memory.limit(35000)
# data = read.csv('trainingData_PATHFINDER_N_1000k.csv')
# data = na.omit(data)
# print(sum(is.na(data)))
# write.csv(data,'PATHFINDER_without_na.csv')

data <- as.data.frame(hailfinder)
data_gt <- as.data.frame(hailfinder)
e = empty.graph(names(data_gt))
arc.set = matrix(c("N07muVerMo","CombVerMo",
                   "SatContMoist","CombMoisture",
                   "VISCloudCov","CombClouds",
                   "Date","Scenario",
                   "LatestCIN","CurPropConv",
                   "CombVerMo","AreaMesoALS",
                   "Scenario","ScenRelAMCIN",
                   "Scenario","ScenRelAMIns",
                   "Scenario","ScenRel34",
                   "Scenario","ScnRelPlFcst",
                   "Scenario","Dewpoints",
                   "Scenario","LowLLapse",
                   "Scenario","MeanRH",
                   "Scenario","MidLLapse",
                   "Scenario","MvmtFeatures",
                   "Scenario","RHRatio",
                   "Scenario","SfcWndShfDis",
                   "Scenario","SynForcng",
                   "Scenario","TempDis",
                   "Scenario","WindAloft",
                   "Scenario","WindFieldMt",
                   "Scenario","WindFieldPln",
                   "AreaMesoALS","AreaMoDryAir",
                   "ScenRelAMCIN","AMCINInScen",
                   "ScenRelAMIns","AMInsWliScen",
                   "AreaMesoALS","CldShadeOth",
                   "CldShadeOth","InsInMt",
                   "InsInMt","OutflowFrMt",
                   "InsInMt","CldShadeConv",
                   "InsInMt","MountainFcst",
                   "WndHodograph","Boundaries",
                   "ScenRel34","N34StarFcst",
                   "AreaMesoALS","CompPlFcst",
                   "CompPlFcst","CapChange",
                   "CompPlFcst","InsChange",
                   "CapChange","CapInScen",
                   "InsChange","InsSclInScen",
                   "MountainFcst","R5Fcst",
                   "CapInScen","PlainsFcst",
                   "SubjVertMo","CombVerMo",
                   "RaoContMoist","CombMoisture",
                   "IRCloudCover","CombClouds",
                   "LLIW","CurPropConv",
                   "CombMoisture","AreaMoDryAir",
                   "MorningCIN","AMCINInScen",
                   "LIfr12ZDENSd","AMInsWliScen",
                   "AreaMoDryAir","CldShadeOth",
                   "AMInstabMt","InsInMt",
                   "WndHodograph","OutflowFrMt",
                   "WndHodograph","CldShadeConv",
                   "OutflowFrMt","Boundaries",
                   "PlainsFcst","N34StarFcst",
                   "CldShadeOth","CompPlFcst",
                   "LoLevMoistAd","InsChange",
                   "AMCINInScen","CapInScen",
                   "AMInsWliScen","InsSclInScen",
                   "N34StarFcst","R5Fcst",
                   "InsSclInScen","PlainsFcst",
                   "CurPropConv","PlainsFcst",
                   "Boundaries","CompPlFcst",
                   "MorningBound","Boundaries",
                   "AMDewptCalPl","AMInsWliScen",
                   "CombClouds","CldShadeOth",
                   "CldShadeConv","CompPlFcst",
                   "ScnRelPlFcst","PlainsFcst",
                   "QGVertMotion","CombVerMo"
),ncol = 2, byrow = TRUE,dimnames = list(NULL, c("from", "to")))
arcs(e) = arc.set

net = bn.fit(e, data_gt) #Ground truth


c_h_s = c()
c_gt_s = c()
c_gt_h = c()
c_dati = c()


num_dati = c(300,600,900,1200)
iterazioni = length(num_dati)
prop = 0.003 #0.005, 0.01
error_prop = prop*0.15

risultati <- data.frame("id" = 1:iterazioni, "Dati" = num_dati, 
                        "Result_1_HARD" = 0, "Result_2_HARD" = 0, "Result_3_HARD" = 0, "Result_4_HARD" = 0, "Result_5_HARD" = 0, "Result_6_HARD" = 0, "Result_7_HARD" = 0,"Result_8_HARD" = 0,
                        "Result_1_SOFT" = 0, "Result_2_SOFT" = 0, "Result_3_SOFT" = 0, "Result_4_SOFT" = 0, "Result_5_SOFT" = 0, "Result_6_SOFT" = 0, "Result_7_SOFT" = 0,"Result_8_SOFT" = 0,
                        "Result_1_FORCED" = 0, "Result_2_FORCED" = 0, "Result_3_FORCED" = 0, "Result_4_FORCED" = 0, "Result_5_FORCED" = 0, "Result_6_FORCED" = 0, "Result_7_FORCED" = 0, "Result_8_FORCED" = 0)

risultati_KL <- data.frame("id" = 1:iterazioni, "Dati" = num_dati, 
                           "Result_1_HARD" = 0, "Result_2_HARD" = 0, "Result_3_HARD" = 0, "Result_4_HARD" = 0, "Result_5_HARD" = 0, "Result_6_HARD" = 0, "Result_7_HARD" = 0,"Result_8_HARD" = 0,
                           "Result_1_SOFT" = 0, "Result_2_SOFT" = 0, "Result_3_SOFT" = 0, "Result_4_SOFT" = 0, "Result_5_SOFT" = 0, "Result_6_SOFT" = 0, "Result_7_SOFT" = 0,"Result_8_SOFT" = 0,
                           "Result_1_FORCED" = 0, "Result_2_FORCED" = 0, "Result_3_FORCED" = 0, "Result_4_FORCED" = 0, "Result_5_FORCED" = 0, "Result_6_FORCED" = 0, "Result_7_FORCED" = 0, "Result_8_FORCED" = 0)


#3 * nrep * length(num_dati)
node_matrix_KL <- data.frame("id" = 1:96)
for (j in colnames(data)){
  node_matrix_KL[[j]] = 0
}


n_row_node_matrix = 1
inc = 1


for (it in 1: nrow(risultati)){
  index_hard = 3
  index_soft = 11
  index_forced = 19
  for (col in 1:8){
    set.seed(2650) #This is the designed seed for this experiment
    random_values = sample(1:6330,risultati[["Dati"]][it])
    
    
    
    #testdata <- as.data.frame(data)
    testdata <- data
    testdata <- as.data.frame(sapply(testdata, as.numeric))
    
    
    result1 <- ampute(data = data)
    patterns <- result1$patterns
    random_values_patterns = sample(1:7744,7744*0.00085)
    cont = 1
    for (i in 1:ncol(patterns)){
      for(j in 1:nrow(patterns)){
        if (cont %in% random_values_patterns){
          patterns[j,i] = 0
        }else{
          patterns[j,i] = 1
        }
        cont = cont + 1
        
      }
    }
    
    
    for (i in 1:ncol(patterns)){
      for(j in 1:nrow(patterns)){
        if (i == j){
          patterns[j,i] = 0
        }
      }
    }
    
    testdata = testdata[c(random_values),]
    
    
    result <- ampute(testdata, mech= "MCAR", prop= prop, bycases=FALSE, patterns=patterns,weights = NULL, cont = TRUE, type = NULL, odds = NULL)
    
    
    a = result$amp
    a = a - 1
    
    total_values = ncol(a) * nrow(a)
    total_NA = sum(is.na(a))
    
    percentile = total_NA/total_values
    
    
    #Controllo che la percentuale di missing values è vicino alla percentuale desiderata
    while (!(percentile > (prop - error_prop) && percentile < (prop + error_prop))){
      patterns <- result1$patterns
      random_values_patterns = sample(1:7744,7744*0.00085)
      cont = 1
      for (i in 1:ncol(patterns)){
        for(j in 1:nrow(patterns)){
          if (cont %in% random_values_patterns){
            patterns[j,i] = 0
          }else{
            patterns[j,i] = 1
          }
          cont = cont + 1
          
        }
      }
      
      
      for (i in 1:ncol(patterns)){
        for(j in 1:nrow(patterns)){
          if (i == j){
            patterns[j,i] = 0
          }
        }
      }
      
      result <- ampute(testdata, mech= "MCAR", prop= prop, bycases=FALSE, patterns=patterns,weights = NULL, cont = TRUE, type = NULL, odds = NULL)
      
      
      a = result$amp
      a = a - 1
      
      total_values = ncol(a) * nrow(a)
      total_NA = sum(is.na(a))
      
      percentile = total_NA/total_values
    }
    
    initial_data <- data.frame(t(a), stringsAsFactors = FALSE)
    
    row.names(initial_data) = names(data)
    
    # colnames(initial_data) = names(data)
    # initial_data[["Violence"]] = initial_data[["Violence"]] +1
    # initial_data[["ViolentConvictions"]] = initial_data[["ViolentConvictions"]] +1

    
    modelstring = modelstring(net)
    structure = model2network(modelstring)
    
    
    cpt = list()
    
    for (name in colnames(data)){
      values_variable = length(unique(data[[name]]))
      
      values_parents = c()
      
      if (!identical(net[[name]]$parents,character(0))){
        for (parents in net[[name]]$parents){
          values_parents = c(values_parents, length(unique(data[[parents]])))
        }
      }
      
      if(!is.null(values_parents)){
        stimated_value = prod(values_parents) * values_variable
        
      }else{
        stimated_value = values_variable
      }
      
      #popolare c
      
      values_matrix =c()
      
      for (i in 1:stimated_value){
        values_matrix = c(values_matrix, 1/values_variable)
      }
      
      #popolare dim
      dimension_matrix=c()
      if(!identical(net[[name]]$parents,character(0))){
        dimension_matrix = c(values_variable,values_parents)
      }
      
      
      list_dimnames = list()
      variables_to_fit = c(name,net[[name]]$parents)
      for (ins in variables_to_fit){
        #poplare dimnames
        
        vector_names = c()
        for (ins_unique in 1:length(unique(data[[ins]]))){
          nome = ins_unique - 1
          nome = as.character(nome)
          vector_names = c(vector_names, nome)
        }
        list_dimnames[[ins]] = vector_names
      }
      
      dimnames_matrix = list_dimnames
      
      if(identical(net[[name]]$parents,character(0))){
        CPT.prob = matrix(values_matrix, dimnames = dimnames_matrix)
        
      }else{
        CPT.prob = c(values_matrix)
        dim(CPT.prob) =  dimension_matrix
        dimnames(CPT.prob)  = dimnames_matrix
      }
      
      cpt[[name]] = CPT.prob
      
      
    }
    
    

    
    print(paste("dataset found with: ", sum(is.na(initial_data))))

    library("future")
    plan(multiprocess, workers = 2, gc = TRUE)
    #Inizializzazione personalizzata custom
    #Inizializzazione personalizzata custom
    #print("INIZIO EM_HARD ASSIGNMENT")
    Data_hard %<-% em_hard(initial_data,structure,cpt,NUMBER_ITERACTION = 3)
    results_soft %<-% em_soft(initial_data,structure,cpt,NUMBER_ITERACTION = 6)
    
    #print("INIZIO EM_SOFT_FORCED ASSIGNMENT")
    Data_soft_forced = results_soft$forced
    Data_soft = results_soft$soft
    
    
    results_gt_hard = compare_em_with_ground_truth_KL(net,Data_hard)
    results_gt_soft = compare_em_with_ground_truth_KL(net,Data_soft)
    results_gt_soft_forced = compare_em_with_ground_truth_KL(net, Data_soft_forced)

    plan(sequential)
    
    print("em end")
    
    risultati_KL[[index_hard]][it] = results_gt_hard
    risultati_KL[[index_soft]][it] = results_gt_soft
    risultati_KL[[index_forced]][it] = results_gt_soft_forced
    
    n_row_node_matrix = inc
    node_matrix_KL = compare_em_with_ground_truth_nodes_KL(net,Data_hard,n_row_node_matrix,node_matrix_KL)
    n_row_node_matrix = n_row_node_matrix + 32
    
    node_matrix_KL = compare_em_with_ground_truth_nodes_KL(net,Data_soft, n_row_node_matrix,node_matrix_KL)
    n_row_node_matrix = n_row_node_matrix + 32
    node_matrix_KL = compare_em_with_ground_truth_nodes_KL(net, Data_soft_forced, n_row_node_matrix,node_matrix_KL)
    
    
    inc = inc + 1
    
    index_hard = index_hard+1
    index_soft = index_soft+1
    index_forced = index_forced+1
    
  }
  
  
}


write.csv(risultati_KL,'results/MNAR_0.005_KL_random.csv')

