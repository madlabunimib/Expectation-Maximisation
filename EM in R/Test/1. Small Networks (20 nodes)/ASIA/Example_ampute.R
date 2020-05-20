library(bnlearn)
library(mice)
source("EM_HARD_Functions.R")
source("EM_SOFT_Functions.R")
source("EM_COMPARISON.R")

set.seed(2016)
testdata <- as.data.frame(asia)
summary(testdata)
result <- ampute(testdata, mech= "MNAR", prop = 0.5)

a = result$amp
initial_data <- data.frame(t(a), stringsAsFactors = FALSE)
initial_data[initial_data=="1"]<-as.numeric(0)
initial_data[initial_data=="2"]<-as.numeric(1)
initial_data = initial_data[,c(1:1750)]

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
print(results)

print("INIZIO VALUTAZIONE GROUND TRUTH")
gt = t(asia)
gt[gt=="no"]<-as.numeric(0)
gt[gt=="yes"]<-as.numeric(1)
results_gt_soft = compare_em_with_ground_truth(initial_data,gt,Data_soft)
print(results_gt_soft)
results_gt_hard = compare_em_with_ground_truth(initial_data,gt,Data_hard)
print(results_gt_hard)

load("asia.rda")

