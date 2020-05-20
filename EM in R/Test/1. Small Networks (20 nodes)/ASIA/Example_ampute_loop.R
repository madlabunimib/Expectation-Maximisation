library(bnlearn)
library(mice)
source("EM_HARD_Functions.R")
source("EM_SOFT_Functions.R")
source("EM_COMPARISON.R")

c_h_s = c()
c_gt_s = c()
c_gt_h = c()
c_dati = c()

numero_dati = 20 
while (numero_dati <= 2720){
  set.seed(2016)
  testdata <- as.data.frame(asia)
  summary(testdata)
  result <- ampute(testdata, mech= "MCAR", prop = 0.5)
  
  a = result$amp
  initial_data <- data.frame(t(a), stringsAsFactors = FALSE)
  initial_data[initial_data=="no"]<-as.numeric(0)
  initial_data[initial_data=="yes"]<-as.numeric(1)
  initial_data = initial_data[,c(1:numero_dati)]
  
  #create structure
  structure = model2network("[A][S][T|A][L|S][B|S][E|T:L][X|E][D|E:B]")
  
  A.prob = matrix(c(0.5,0.5), dimnames = list(A = c("0", "1")))
  S.prob = matrix(c(0.5,0.5), dimnames = list(S = c("0", "1")))
  T.prob = matrix(c(0.5,0.5,0.5,0.5), ncol=2, dimnames = list(T = c("0", "1"), A=c("0", "1")))
  L.prob = matrix(c(0.5,0.5,0.5,0.5), ncol=2, dimnames = list(L = c("0", "1"), S=c("0", "1")))
  B.prob = matrix(c(0.5,0.5,0.5,0.5), ncol=2, dimnames = list(B = c("0", "1"), S=c("0", "1")))
  E.prob = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
  dim(E.prob) = c(2, 2, 2)
  dimnames(E.prob) = list(E = c("0", "1"), T = c("0", "1"), L=c("0","1"))
  X.prob = matrix(c(0.5,0.5,0.5,0.5), ncol=2, dimnames = list(X = c("0", "1"), E=c("0", "1")))
  D.prob = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
  dim(D.prob) = c(2, 2, 2)
  dimnames(D.prob) = list(D = c("0", "1"), E = c("0", "1"), B=c("0","1"))
  
  cpt = list(A = A.prob, S = S.prob, T = T.prob, L = L.prob, B = B.prob, E = E.prob, X = X.prob, D = D.prob)
  
  #Inizializzazione personalizzata custom
  print("INIZIO EM_HARD ASSIGNMENT")
  Data_hard = em_hard(initial_data,structure,cpt, NUMBER_ITERACTION = 3)
  
  print("INIZIO EM_SOFT ASSIGNMENT")
  Data_soft = em_soft(initial_data,structure,cpt,NUMBER_ITERACTION = 3)
  
  print("INIZIO VALUTAZIONE")
  results = compare_em(initial_data,Data_soft,Data_hard)

  
  print("INIZIO VALUTAZIONE GROUND TRUTH")
  gt = t(asia)
  gt[gt=="no"]<-as.numeric(0)
  gt[gt=="yes"]<-as.numeric(1)
  results_gt_soft = compare_em_with_ground_truth(initial_data,gt,Data_soft)

  results_gt_hard = compare_em_with_ground_truth(initial_data,gt,Data_hard)

  
  c_h_s = c(c_h_s, results)
  c_gt_s = c(c_gt_s, results_gt_soft)
  c_gt_h = c(c_gt_h, results_gt_hard)
  c_dati = c(c_dati,numero_dati)
  numero_dati = numero_dati+50
  
  print(numero_dati)
  
  
}


library(plotly)

x <- c_dati
data <- data.frame(x, c_h_s)

fig <- plot_ly(data, x = ~x, y = ~c_h_s, type = 'scatter', mode = 'lines')
fig <- fig %>% layout(title = "ASIA MAR 0.5 FORCED - Comparison EM HARD EM SOFT",
                      xaxis = list(title = "Number of Data"),
                      yaxis = list (title = "% equal values"))

fig




data_gt <- data.frame(x, c_gt_s, c_gt_h)

fig_gt <- plot_ly(data_gt, x = ~x, y = ~c_gt_s, name = 'Comparison EM SOFT with ground truth', type = 'scatter', mode = 'lines',
                  line = list(color = 'rgb(205, 12, 24)', width = 4))
fig_gt <- fig_gt %>% add_trace(y = ~c_gt_h, name = 'Comparison EM HARD with ground truth', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
fig_gt <- fig_gt %>% layout(title = "ASIA MAR 0.5 FORCED - Comparison EM with ground truth",
                            xaxis = list(title = "Number of Data"),
                            yaxis = list (title = "% equal values"))

fig_gt

