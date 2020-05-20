library(bnlearn)
library(mice)
source("EM_HARD_Functions.R")
source("EM_SOFT_Functions.R")
source("EM_COMPARISON.R")

set.seed(2016)
testdata <- as.data.frame(alarm)
summary(testdata)
result <- ampute(testdata, mech= "MNAR", prop = 0.3)

a = result$amp
a = a - 1
initial_data <- data.frame(t(a), stringsAsFactors = FALSE)
initial_data = initial_data[,c(1:2500)]

modelstring = paste0("[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF][LVF]",
                     "[STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA][HRSA|ERCA:HR][ANES]",
                     "[APL][TPR|APL][ECO2|ACO2:VLNG][KINK][MINV|INT:VLNG][FIO2][PVS|FIO2:VALV]",
                     "[SAO2|PVS:SHNT][PAP|PMB][PMB][SHNT|INT:PMB][INT][PRSS|INT:KINK:VTUB][DISC]",
                     "[MVS][VMCH|MVS][VTUB|DISC:VMCH][VLNG|INT:KINK:VTUB][VALV|INT:VLNG]",
                     "[ACO2|VALV][CCHL|ACO2:ANES:SAO2:TPR][HR|CCHL][CO|HR:STKV][BP|CO:TPR]")
structure = model2network(modelstring)






HIST.prob = matrix(c(0.5,0.5,0.5,0.5), ncol=2, dimnames = list(HIST = c("0", "1"), LVF=c("0", "1")))
CVP.prob = matrix(c(1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3), ncol=3, dimnames = list(CVP = c("0", "1","2"), LVV=c("0", "1","2")))
PCWP.prob = matrix(c(1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3), ncol=3, dimnames = list(PCWP = c("0", "1","2"), LVV=c("0", "1","2")))
HYP.prob = matrix(c(0.5,0.5), dimnames = list(HYP = c("0", "1")))
LVV.prob = c(1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3)
dim(LVV.prob) = c(3, 2, 2)
dimnames(LVV.prob) = list(LVV = c("0", "1","2"), HYP = c("0", "1"), LVF=c("0","1"))
LVF.prob = matrix(c(0.5,0.5), dimnames = list(LVF = c("0", "1")))
STKV.prob = c(1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3)
dim(STKV.prob) = c(3, 2, 2)
dimnames(STKV.prob) = list(STKV = c("0", "1","2"), HYP = c("0", "1"), LVF=c("0","1"))
ERLO.prob = matrix(c(0.5,0.5), dimnames = list(ERLO = c("0", "1")))
HRBP.prob = c(1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3)
dim(HRBP.prob) = c(3, 2, 3)
dimnames(HRBP.prob) = list(HRBP = c("0", "1","2"), ERLO = c("0", "1"), HR=c("0","1","2"))
HREK.prob = c(1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3)
dim(HREK.prob) = c(3, 2, 3)
dimnames(HREK.prob) = list(HREK = c("0", "1","2"), ERCA = c("0", "1"), HR=c("0","1","2"))
ERCA.prob = matrix(c(0.5,0.5), dimnames = list(ERCA = c("0", "1")))
HRSA.prob = c(1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3)
dim(HRSA.prob) = c(3, 2, 3)
dimnames(HRSA.prob) = list(HRSA = c("0", "1","2"), ERCA = c("0", "1"), HR=c("0","1","2"))
ANES.prob = matrix(c(0.5,0.5), dimnames = list(ANES = c("0", "1")))
APL.prob = matrix(c(0.5,0.5), dimnames = list(APL = c("0", "1")))
TPR.prob = matrix(c(1/3,1/3,1/3,1/3,1/3,1/3), ncol=2, dimnames = list(TPR = c("0", "1","2"), APL=c("0", "1")))
ECO2.prob = c(1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4)
dim(ECO2.prob) = c(4, 3, 4)
dimnames(ECO2.prob) = list(ECO2 = c("0", "1","2","3"), ACO2 = c("0", "1","2"), VLNG=c("0","1","2","3"))
KINK.prob = matrix(c(0.5,0.5), dimnames = list(KINK = c("0", "1")))
MINV.prob = c(1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4)
dim(MINV.prob) = c(4, 3, 4)
dimnames(MINV.prob) = list(MINV = c("0", "1","2","3"), INT = c("0", "1","2"), VLNG=c("0","1","2","3"))
FIO2.prob = matrix(c(0.5,0.5), dimnames = list(FIO2 = c("0", "1")))

PVS.prob = c(1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3)
dim(PVS.prob) = c(3, 2, 4)
dimnames(PVS.prob) = list(PVS = c("0", "1","2"), FIO2 = c("0", "1"), VALV=c("0","1","2","3"))

SAO2.prob = c(1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3)
dim(SAO2.prob) = c(3, 3, 2)
dimnames(SAO2.prob) = list(SAO2 = c("0", "1","2"), PVS = c("0", "1","2"), SHNT=c("0","1"))
PAP.prob = matrix(c(1/3,1/3,1/3,1/3,1/3,1/3), ncol=2, dimnames = list(PAP = c("0", "1","2"), PMB=c("0", "1")))
PMB.prob = matrix(c(0.5,0.5), dimnames = list(PMB = c("0", "1")))
SHNT.prob = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
dim(SHNT.prob) = c(2, 3, 2)
dimnames(SHNT.prob) = list(SHNT = c("0", "1"), INT = c("0", "1","2"), PMB=c("0","1"))
INT.prob = matrix(c(1/3,1/3,1/3), dimnames = list(INT = c("0", "1","2")))

PRSS.prob = c(1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4)
dim(PRSS.prob) = c(4, 3,2,4)
dimnames(PRSS.prob) = list(PRSS = c("0", "1","2","3"), INT = c("0", "1","2"), KINK=c("0","1"), VTUB=c("0","1","2","3"))






DISC.prob = matrix(c(0.5,0.5), dimnames = list(DISC = c("0", "1")))
MVS.prob = matrix(c(1/3,1/3,1/3), dimnames = list(MVS = c("0", "1","2")))
VMCH.prob = matrix(c(1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4), ncol=3, dimnames = list(VMCH = c("0", "1","2","3"), MVS=c("0", "1","2")))
VTUB.prob = c(1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4)
dim(VTUB.prob) = c(4, 2, 4)
dimnames(VTUB.prob) = list(VTUB = c("0", "1","2","3"), DISC = c("0", "1"), VMCH=c("0","1","2","3"))
VLNG.prob = c(1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4)
dim(VLNG.prob) = c(4,3,2,4)
dimnames(VLNG.prob) = list(VLNG = c("0", "1","2","3"), INT = c("0", "1","2"), KINK=c("0","1"), VTUB=c("0","1","2","3"))
VALV.prob = c(1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4,1/4)
dim(VALV.prob) = c(4, 3, 4)
dimnames(VALV.prob) = list(VALV = c("0", "1","2","3"), INT = c("0", "1","2"), VLNG=c("0","1","2","3"))
ACO2.prob = matrix(c(1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3), ncol=4, dimnames = list(ACO2 = c("0", "1","2"), VALV=c("0", "1","2","3")))

CCHL.prob = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
dim(CCHL.prob) = c(2,3,2,3,3)
dimnames(CCHL.prob) = list(CCHL = c("0", "1"), ACO2 = c("0", "1","2"), ANES=c("0","1"), SAO2=c("0","1","2"), TPR=c("0","1","2"))


HR.prob = matrix(c(1/3,1/3,1/3,1/3,1/3,1/3), ncol=2, dimnames = list(HR = c("0", "1","2"), CCHL=c("0", "1")))

CO.prob = c(1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3)
dim(CO.prob) = c(3, 3, 3)
dimnames(CO.prob) = list(CO= c("0", "1","2"), HR = c("0", "1","2"), STKV=c("0","1","2"))

BP.prob = c(1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3)
dim(BP.prob) = c(3, 3, 3)
dimnames(BP.prob) = list(BP = c("0", "1","2"), CO = c("0", "1","2"), TPR=c("0","1","2"))


cpt = list(HIST=HIST.prob, CVP=CVP.prob, PCWP=PCWP.prob, HYP=HYP.prob, LVV=LVV.prob, LVF=LVF.prob, STKV=STKV.prob, ERLO=ERLO.prob, HRBP=HRBP.prob, HREK=HREK.prob, ERCA=ERCA.prob, HRSA=HRSA.prob, ANES=ANES.prob, APL=APL.prob, TPR=TPR.prob, ECO2=ECO2.prob, KINK =KINK.prob, MINV =MINV.prob, FIO2 =FIO2.prob, PVS =PVS.prob, SAO2=SAO2.prob, PAP =PAP.prob, PMB =PMB.prob, SHNT =SHNT.prob, INT = INT.prob, PRSS = PRSS.prob, DISC=DISC.prob, MVS=MVS.prob, VMCH=VMCH.prob, VTUB=VTUB.prob, VLNG=VLNG.prob, VALV=VALV.prob, ACO2=ACO2.prob, CCHL=CCHL.prob, HR = HR.prob, CO=CO.prob, BP = BP.prob)


#Inizializzazione personalizzata custom
print("INIZIO EM_HARD ASSIGNMENT")
Data_hard = em_hard(initial_data,structure,cpt)


print("INIZIO EM_SOFT ASSIGNMENT")
Data_soft = em_soft(initial_data,structure,cpt)




print("INIZIO VALUTAZIONE")
results = compare_em(initial_data,Data_soft,Data_hard)
print(results)

print("INIZIO VALUTAZIONE GROUND TRUTH")
dataset_gt <- data.frame(testdata)
gt <- as.data.frame(sapply(dataset_gt, as.numeric))
dataset_gt <- data.frame(t(gt))
dataset_gt = dataset_gt[,c(1:2500)]

dataset_gt = dataset_gt - 1
results_gt_soft = compare_em_with_ground_truth(initial_data,dataset_gt,Data_soft)
print(results_gt_soft)
results_gt_hard = compare_em_with_ground_truth(initial_data,dataset_gt,Data_hard)
print(results_gt_hard)


