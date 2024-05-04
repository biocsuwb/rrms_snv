library(randomForest)
library(Boruta)
library(caret)

source('modelRFcross.R')
source('indexcross.R')
source('varBorutacross.R')

set.seed(123)
### Load data sets
# load SNVs and clinical (age, sex) markers of rrms and control groups (positive class: rrms)
df.rrms.control = read.csv2('data_rrms_control.csv')
# load genotype (SNVs) and phenotype markers of patients with RRMS
df.rrms = read.csv2('data_rrms_snv_clinical.csv')

### 1a. Feature selection: RRMS vs HC
# feature selection stability

data = data.frame(df.rrms.control[, -1])
list.index.cross = indexcross(
  y = data$class,
  folds = 3,
  iterations = 30,
  stratified = TRUE
)
var.imp = varimpBoruta(
  data = data,
  folds = 3,
  niter = 30,
  list.index.cross,
  maxRuns = 100
)

# marker frequency in 90 relevant feature subsets
marker.count = sort(table(unlist(var.imp)), decreasing = T)
print(marker.count)

# age  rs187238 rs1946518       sex
# 90        90        90        85

# Marker importance
result.boruta = Boruta(data[, names(marker.count)], data$class, doTrace =
                         2)
print(result.boruta)

# Boruta performed 9 iterations in 0.4208739 secs.
# 4 attributes confirmed important: age, rs187238, rs1946518, sex;
# No attributes deemed unimportant.

plot(
  result.boruta ,
  sort = TRUE,
  las = 2,
  cex.lab = 1,
  main = 'RRMS vs HC',
  cex = 1,
  xlab = "",
  cex.axis = 0.9,
  ylab = "Importance"
)

### Predictive models RRMS vs HC (male and female)

rank.markers = data.frame(
  var.imp = names(result.boruta$finalDecision),
  importance = result.boruta$finalDecision
)
Ntop.markers = rank.markers[rank.markers$importance != 'Rejected', ]
result = modelRFcross(
  data,
  var.imp = Ntop.markers,
  niter = 30,
  folds = 3,
  list.index.cross
)


print(result)

# OOB.mean 0.2112
# OOB.sd   0.0027
# mcc.mean 0.5863
# mcc.sd   0.0074
# auc.mean 0.7927
# auc.sd   0.0038
# acc.mean 0.7925
# acc.sd   0.0038
# f1.mean  0.8089
# f1.sd    0.0038

#########################################################
### 1b. Feature selection: RRMS vs HC (female)
# feature selection stability

data = data.frame(df.rrms.control[df.rrms.control$sex == 0, -c(1, 3)])
list.index.cross = indexcross(
  y = data$class,
  folds = 3,
  iterations = 30,
  stratified = TRUE
)
var.imp = varimpBoruta(
  data = data,
  folds = 3,
  niter = 30,
  list.index.cross,
  maxRuns = 100
)

# marker frequency in 90 relevant feature subsets
marker.count = sort(table(unlist(var.imp)), decreasing = T)
print(marker.count)

# age  rs187238 rs1946518
# 90        90        90

# Marker importance
result.boruta = Boruta(data[, names(marker.count)], data$class, doTrace =
                         2)
print(result.boruta)

# Boruta performed 9 iterations in 0.3171542 secs.
# 3 attributes confirmed important: age, rs187238, rs1946518;
# No attributes deemed unimportant.

plot(
  result.boruta ,
  sort = TRUE,
  las = 2,
  cex.lab = 1,
  main = 'RRMS vs HC, female',
  cex = 1,
  xlab = "",
  cex.axis = 0.9,
  ylab = "Importance"
)

### Predictive models RRMS vs HC (male and female)

rank.markers = data.frame(
  var.imp = names(result.boruta$finalDecision),
  importance = result.boruta$finalDecision
)
Ntop.markers = rank.markers[rank.markers$importance != 'Rejected', ]
result = modelRFcross(
  data,
  var.imp = Ntop.markers,
  niter = 30,
  folds = 3,
  list.index.cross
)


print(result)

# OOB.mean 0.1920
# OOB.sd   0.0032
# mcc.mean 0.6196
# mcc.sd   0.0081
# auc.mean 0.8059
# auc.sd   0.0043
# acc.mean 0.8166
# acc.sd   0.0039
# f1.mean  0.8487
# f1.sd    0.0032

#################################
### 1c. Feature selection: RRMS vs HC (male)
# feature selection stability

data = data.frame(df.rrms[, -1])
list.index.cross = indexcross(
  y = data$sex,
  folds = 3,
  iterations = 30,
  stratified = TRUE
)
var.imp = varimpBoruta(
  data = data,
  folds = 3,
  niter = 30,
  list.index.cross,
  maxRuns = 100
)

# marker frequency in 90 relevant feature subsets
marker.count = sort(table(unlist(var.imp)), decreasing = T)
print(marker.count)

# rs187238 rs1946518       age
# 90        83        45

# Marker importance
result.boruta = Boruta(data[, names(marker.count)], data$class, doTrace =
                         2)
print(result.boruta)

# Boruta performed 38 iterations in 0.7051079 secs.
# 3 attributes confirmed important: age, rs187238, rs1946518;
# No attributes deemed unimportant.

plot(
  result.boruta ,
  sort = TRUE,
  las = 2,
  cex.lab = 1,
  main = 'RRMS vs HC, male',
  cex = 1,
  xlab = "",
  cex.axis = 0.9,
  ylab = "Importance"
)

### Predictive models RRMS vs HC (male and female)

rank.markers = data.frame(
  var.imp = names(result.boruta$finalDecision),
  importance = result.boruta$finalDecision
)
Ntop.markers = rank.markers[rank.markers$importance != 'Rejected', ]
result = modelRFcross(
  data,
  var.imp = Ntop.markers,
  niter = 30,
  folds = 3,
  list.index.cross
)


print(result)

# OOB.mean 0.3008
# OOB.sd   0.0078
# mcc.mean 0.4564
# mcc.sd   0.0140
# auc.mean 0.7150
# auc.sd   0.0065
# acc.mean 0.7310
# acc.sd   0.0070
# f1.mean  0.6449
# f1.sd    0.0085

#################################
set.seed(123)
### 1c. Feature selection: female vs male, RRMS
# feature selection stability
data = data.frame(class = df.rrms$sex, df.rrms[, -c(1, 2)])
list.index.cross = indexcross(
  y = data$class,
  folds = 3,
  iterations = 30,
  stratified = TRUE
)
var.imp = varimpBoruta(
  data = data,
  folds = 3,
  niter = 30,
  list.index.cross,
  maxRuns = 100
)

# marker frequency in 90 relevant feature subsets
marker.count = sort(table(unlist(var.imp)), decreasing = T)
print(marker.count)

# age                 rs187238              smoking_bin
# 90                       48                       36
# EDSS_currently                  family3            drug_earlier2
# 27                       23                       18
# concomitant_diseases1           infections_bin         treatment_months
# 13                       13                       12
# EDSS_beginning              infections1     reason_changing_drug
# 11                       10                        9
# drug_currently4          drug_currently1 concomitant_diseases_bin
# 5                        3                        2
# concomitant_diseases5          drug_currently3                rs1946518
# 2                        2                        2
# city_country               family_bin                  family2
# 1                        1                        1

# marker importance (10 most frequently occurring markers in 90 feature subsets)
result.boruta = Boruta(data[, names(marker.count)[1:10]], data$class, doTrace =
                         2)
print(result.boruta)

# Boruta performed 99 iterations in 2.61002 secs.
# 4 attributes confirmed important: age, EDSS_currently, rs187238, smoking_bin;
# 5 attributes confirmed unimportant: concomitant_diseases1, drug_earlier2, family3, infections_bin,
# treatment_months;
# 1 tentative attributes left: EDSS_beginning;


plot(
  result.boruta ,
  sort = TRUE,
  las = 2,
  cex.lab = 1,
  main = 'female vs male, RRMS',
  cex = 1,
  xlab = "",
  cex.axis = 0.9,
  ylab = "Importance"
)

### Predictive models RRMS vs HC (male and female)


rank.markers = data.frame(
  var.imp = names(result.boruta$finalDecision),
  importance = result.boruta$finalDecision
)
Ntop.markers = rank.markers[rank.markers$importance != 'Rejected', ]
result = modelRFcross(
  data,
  var.imp = Ntop.markers,
  niter = 30,
  folds = 3,
  list.index.cross
)


print(result)
# OOB.mean 0.3384
# OOB.sd   0.0044
# mcc.mean 0.2946
# mcc.sd   0.0110
# auc.mean 0.6563
# auc.sd   0.0058
# f1.mean  0.7507
# f1.sd    0.0053