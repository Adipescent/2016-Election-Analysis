#1 
#Voter prediction could be difficult since data can be bias. The data used to predict the election will most likely come from surveys, which 
#depending on how the survey is adminstered could contain a lot of people on one side than the other. If the survey was conducted online at a college
#it will most likely contain democrats. Voters may also answer that they will vote for one candidate, but may actually vote for the other candidate for
#whatever reason. People that are on side may not be willing to answer to surveys either.

#2 
#Silver seemed to have used machine learning techniques such as hierarchical modelling while also generating a time series. The time series
#graph takes into account measurable variables as well as nonmeasurable variables which are included as extra random terms. Then the data is
#simulated forward in time showing possible percentages for who wins in each state. The hierarchical modelling is used for state polls to calculate 
#data for states that might not have gotten polled which may help with nonresponse bias. All of the math is calculated using the Bayes' Theorem.
-
#3
#In 2016, all the polls seem to be answered mainly by supporters of Clinton while leaving out the supporters of Trump. They all seem to contain
#errors that led to seeming like Clinton would win by a large margin. People might not be willing to say that they were voting for Trump and
#and said something else. Women voters might have been especially affected by this phenomenon. Trump had also gotten support from other Republican 
#voters who supported other candidates or were undecided during the polls. 


library(dplyr)
library(readr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(tidyr)

## set the working directory as the file location
setwd(getwd())
## put the data folder and this handout file together.
## read data and convert candidate from string to factor
election.raw <- read_delim("data/election/election.csv", delim = ",") %>% mutate(candidate=as.factor(candidate))

census_meta <- read_delim("D:/pstat 131/Final project/data/census/metadata.csv", delim = ";", col_names = FALSE) 
census <- read_delim("data/census/census.csv", delim = ",") 

#4==============================================================================
#kable(election.raw %>% filter(county == "Los Angeles County"))  %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width=FALSE)
election.raw <- filter(election.raw, fips != 2000)

dimension = matrix(NA, nrow=1, ncol=2)

colnames(dimension) = c("Observations","Variables")

dimension[1,1]<-dim(election.raw)[1]
dimension[1,2]<-dim(election.raw)[2]

kable(dimension) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width=FALSE)

#When the fips is 2000 it does not have a county so it should be excluede. 
#5==============================================================================
election_federal <- filter(election.raw, fips == "US")
election_state <- filter(election.raw, fips != "US" & is.na(county))
election <- filter(election.raw, !is.na(county))
#6==============================================================================
group <- as.factor(election_federal$candidate)
ggplot(election_federal, aes(x = reorder(candidate, votes), y= log(votes), fill = candidate)) +geom_bar(stat = "identity") + geom_text(aes(label = votes))+xlab("Candidates")+theme_minimal()+theme(legend.position = "none") +coord_flip()

dim(election_federal)
#there are 32 candidates 
#7==============================================================================
ss <- group_by(election, fips)
total <- summarize(ss, total = sum(votes))
newdata <-left_join(ss, total, by = "fips")
cpct <- mutate(newdata , pct = votes/total)
county_winner <- top_n(cpct, n =1)

ss <- group_by(election_state, state)
total <- summarize(ss, total = sum(votes))
newdata <-left_join(ss, total, by = "state")
cpct <- mutate(newdata , pct = votes/total)
state_winner <- top_n(cpct, n =1)


states <- map_data("state")

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # color legend is unnecessary and takes too long
#8==============================================================================
counties = map_data("county")
ggplot(data = counties) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # color legend is unnecessary and takes too long
#9 =============================================================================
fips = state.abb[match(states$region, tolower(state.name))]
states=states%>%mutate(fips=state.abb[match(states$region,tolower(state.name) )])
winner_data1 <- left_join(states, state_winner, by = "fips")

ggplot(data = winner_data1) + 
  geom_polygon(aes(x = long, y = lat, fill = winner_data1$candidate, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # color legend is unnecessary and takes too long
#10=============================================================================
countyMap<- maps::county.fips
countySplit=separate(countyMap,polyname,c("region", "subregion"),sep="," )
countyMap<- left_join(counties, countySplit, by = c("subregion", "region") )
class(countyMap$fips)="character"
countyMap<- left_join(countyMap, county_winner, by = "fips")

ggplot(data = countyMap) + 
  geom_polygon(aes(x = long, y = lat, fill = candidate, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # color legend is unnecessary and takes too long
#11=============================================================================
library(hexbin)
ggplot(census, aes(x=Asian, y = Income))+geom_hex(bins = 70)+scale_fill_continuous(type = "viridis") +theme_bw()
#This visualization of Poverty and Income shows that the higher 
ggplot(census, aes(x=Income, y = State))+geom_hex(bins = 70)+scale_fill_continuous(type = "viridis") +theme_bw()
ggplot(census, aes(x=White, y = Income))+geom_hex(bins = 70)+scale_fill_continuous(type = "viridis") +theme_bw()

#12=============================================================================
census.del <- na.omit(census) 
census.del$Men = 100*census.del$Men/census.del$TotalPop
census.del$Employed = 100*census.del$Employed/census.del$TotalPop
census.del$Citizen = 100*census.del$Citizen/census.del$TotalPop
#makes minority column
census.del$Minority = census.del$Hispanic + census.del$Black +census.del$Native+census.del$Asian + census.del$Pacific
#gets rid of columns
census.del <- subset(census.del, select = -c(Hispanic,Black,Native, Asian, Pacific))
census.del <- subset(census.del, select = -c(Walk, PublicWork, Construction))
#get rid of columns that are sets who add up to 100%
census.del <- subset(census.del, select = -c(Women))

census.subct <- census.del%>%group_by(State,County)%>%add_tally(name = "CountyTotal")
census.subct$CountyWeight = census.subct$TotalPop/census.subct$CountyTotal
#census.subct <- mutate(census.subct, CountyWeight = TotalPop/CountyTotal)

CountySum <-census.subct%>%summarize_at(vars(CountyWeight),sum)
census.ct <- left_join(census.subct, CountySum, by =c("State", "County"))

names(census.ct)[names(census.ct)=="CountyWeight.y"] <- "CountySum"
names(census.ct)[names(census.ct)=="CountyWeight.x"] <- "CountyWeight"

census.ct <- mutate(census.ct, CountyWeight = CountyWeight/CountySum)
census.ct[4:29] <- census.ct[4:29]*census.ct$CountyWeight
census.ct <- census.ct %>% summarise_at(vars(Men:Minority), funs(sum))

kable(head(census.ct)) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width=FALSE)
#13=============================================================================
numcensus.ct <- census.ct[3:27]
numcensus.subct <- census.subct[3:28]
ct.pc <- prcomp(numcensus.ct,scale = TRUE, center = TRUE)
pc1<-sort(abs(ct.pc$rotation[,1]),decreasing = TRUE)

colnames(pc1) = c("thing1", "thing2")
kable(head(pc1))%>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width=FALSE)



rotation.ct <- ct.pc$rotation[,c(1,2)]
#rotation.ct[c(6,9,8),]
kable(rotation.ct[c("IncomePerCap","ChildPoverty","Poverty"),]) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width=FALSE)

#The sign for PC1 is positive for IncomePerCap which means that if IncomePerCap increases
#PC1 will also increase. The sign for ChildPoverty and Poverty is negative for PC1
#which means that an increase in ChildPoverty or Poverty will mean a decrease for PC1.

#------------------------------------------------------------------------------

subct.pc <- prcomp(numcensus.subct,scale = TRUE, center = TRUE)
rotation.subct<-subct.pc$rotation[,c(1,2)]

pc1<-sort(abs(subct.pc$rotation[,1]),decreasing = TRUE)
head(pc1)

rotation.subct[c(7,11,9),]
#The sign for IncomePerCap and Professional are negative meaning that they are negatively correlated in the subcounty data.
#An increase in IncomePerCap or Professional will decrease PC1.
#Poverty has a positive PC1 which means that an increase in Poverty will be an increase in PC1. 

#14=============================================================================
#for county
ct.var <- (ct.pc$sdev)^2
ct.pve = ct.var/sum(ct.var)
ct.cumulative <- cumsum(ct.pve)
which(ct.cumulative>=0.90)[1]

#for subcounty
subct.var <- (subct.pc$sdev)^2
subct.pve = subct.var/sum(subct.var)
subct.cumulative <- cumsum(subct.pve)
which(subct.cumulative>=0.90)[1]

#plot for county
plot(ct.pve, type="l", lwd=3)
plot(ct.cumulative, type="l", lwd=3)

#plot for subcounty
plot(subct.pve, type="l",lwd=3)
plot(subct.cumulative, type="l", lwd=3)
#15=============================================================================
library(dendextend)

scensus = scale(census.ct[, -c(1,2)], center=TRUE, scale=TRUE)
census.dist = dist(scensus)
census.hclust = hclust(census.dist)
census.dend = as.dendrogram(census.hclust)
census.dend = color_branches(census.dend, k=10)
census.dend = color_labels(census.dend, k=10)
census.dend = set(census.dend,"labels_cex", 0.3)
census.dend = set_labels(census.dend, labels=census.ct$County)
plot(census.dend, horiz=F, main="Dendrogram colored by 10 groups")


#-------------------------------------------------------------------------------

census.pc5= ct.pc$x[,1:5]
censuspc5.dist = dist(census.pc5)
censuspc5.hclust = hclust(censuspc5.dist)
censuspc5.dend = as.dendrogram(censuspc5.hclust)
censuspc5.dend = color_branches(censuspc5.dend, k=10)
censuspc5.dend = color_labels(censuspc5.dend, k=10)
censuspc5.dend = set(censuspc5.dend,"labels_cex", 0.3)
censuspc5.dend = set_labels(censuspc5.dend, labels=census.ct$County)
plot(censuspc5.dend, horiz=F, main="PC5 colored by 10 groups")
abline(h=4, col="black", lty=2) 

SanMateo.Pos <- which(census.ct$County == "San Mateo")
cutpc5 = cutree(censuspc5.hclust, k=10)
cutcen = cutree(census.hclust, k=10)
cutpc5[SanMateo.Pos]
cutcen[SanMateo.Pos]

census.clus7 <- which(cutcen == "7")
census.pc5.clus4 <- which(cutpc5 == "4")

x7<- scale(census.clus7, center=TRUE)
x4<-scale(census.pc5.clus4, center=TRUE)

x7.dist <- dist(x7)
x4.dist<-dist(x4)

mean(x7.dist)
mean(x4.dist)

sd(x7.dist)
sd(x4.dist)
# We belive that the PC5 hierarchical clustering is the superior approach as it has a smaller standard deviation, which means the data varies less
#which means that it will be more likely to place San Mateo in the correct cluster more consistently. 

#16=============================================================================
#Classification
set.seed(1)
tmpwinner <- county_winner %>% ungroup %>%
  mutate(state = state.name[match(state, state.abb)]) %>%               ## state abbreviations
  mutate_at(vars(state, county), tolower) %>%                           ## to all lowercase
  mutate(county = gsub(" county| columbia| city| parish", "", county))  ## remove suffixes
tmpcensus <- census.ct %>% mutate_at(vars(State, County), tolower)

election.cl <- tmpwinner %>%
  left_join(tmpcensus, by = c("state"="State", "county"="County")) %>% 
  na.omit

## save meta information
election.meta <- election.cl %>% select(c(county, fips, state, votes, pct, total))

## save predictors and class labels
election.cl = election.cl %>% select(-c(county, fips, state, votes, pct, total))

#Using the following code, partition data into 80% training and 20% testing:----
set.seed(10) 
n <- nrow(election.cl)
in.trn <- sample.int(n, 0.8*n) 
trn.cl <- election.cl[ in.trn,]
tst.cl <- election.cl[-in.trn,]
#Using the following code, define 10 cross-validation folds:--------------------
set.seed(20) 
nfold <- 10
folds <- sample(cut(1:nrow(trn.cl), breaks=nfold, labels=FALSE))
#Using the following error rate function:---------------------------------------
calc_error_rate = function(predicted.value, true.value){
  return(mean(true.value!=predicted.value))
}
records = matrix(NA, nrow=3, ncol=2)
colnames(records) = c("train.error","test.error")
rownames(records) = c("tree","logistic","lasso")

#16=============================================================================
library(ISLR)
library(tree)
library(maptree)

tree <- tree(candidate~., data = trn.cl)

cv = cv.tree(tree,rand = folds, FUN=prune.misclass, K=nfold)
draw.tree(tree, nodeinfo=FALSE) #tree before pruning

best.cv = cv$size[which.min(cv$dev)]
tree.pruned = prune.tree(tree, best=best.cv)
draw.tree(tree.pruned, nodeinfo=TRUE)
title("Pruned Tree")
#If the transit is less than 1.05249, then the tree will check the percentage of white people in the county. If the amount of white people
#in the county is less than 48.3773 percent then it will check the unemployment rate. Then if unemployment is larger than 10.4482 percent, it is 
#60.6% likely that Hillary Clinton wins or if unemployment is smaller then, Donald Trump is likely to win. 

pred.test.cv = predict(tree.pruned, tst.cl, type="class")
#training error prediction
pred.train.cv = predict(tree.pruned ,trn.cl, type="class")

training_error =calc_error_rate(pred.train.cv,trn.cl$candidate)
#test error
test_error = calc_error_rate(pred.test.cv,tst.cl$candidate)
records[1,1] = training_error
records[1,2] = test_error
records
#17=============================================================================
glm.fit = glm(candidate ~.,data= trn.cl,family=binomial)

response = predict(glm.fit, trn.cl, type="response")
predict.train <- trn.cl %>% mutate(res=(ifelse(response>.50, "Hillary Clinton", "Donald Trump")))
training_error<-calc_error_rate(predict.train$res,trn.cl$candidate)

response = predict(glm.fit, tst.cl, type="response")
predict.train <- tst.cl %>% mutate(res=(ifelse(response>.50, "Hillary Clinton", "Donald Trump")))
test_error<-calc_error_rate(predict.train$res,tst.cl$candidate)

records[2,1]<-training_error
records[2,2]<-test_error

kable(residuals(summary(glm.fit))) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width=FALSE)
#Some of the significant variables include Citizen, IncomePerCap, and Professional. These variables are impactful
#when deciding which candidate will win the county. These variables also seem to coincide with the different views of 
#Democrats and Republicans. 
#Transit is not a significant variable in the logistic regression, but it is the first variable that is split in the decision tree. Otherwise
#the other variables split are significant variables, so it coincides a little. 
#18=============================================================================
library(glmnet)
x = model.matrix(candidate~., trn.cl)[,-1]
y <- droplevels(as.factor(trn.cl$candidate))
ridge.mod=cv.glmnet(x,y,alpha=1,family="binomial",lambda = c(1, 5, 10, 50) * 1e-4)
best<-ridge.mod$lambda.min
best
lasso_mod = glmnet(x, y, alpha = 1, family="binomial", lambda  = best)
lasso.coef <- coef(lasso_mod)
lasso.coef[which(lasso.coef != 0)]
which(lasso.coef != 0)
coef<- predict(lasso_mod,type="coefficients",s=best)[1:20,]
coef
options(digits = 10)
kable(coef)  %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width=FALSE)
x.tst <- model.matrix(candidate ~ . , tst.cl)[,-1]
train.pred = predict(lasso_mod, newx= x,type="response")
test.pred = predict(lasso_mod, newx= x.tst, type = "response")

lasso.train <- trn.cl %>% mutate(predict.train=(ifelse(train.pred>.50, "Hillary Clinton", "Donald Trump")))
lasso.test <- tst.cl %>% mutate(predict.test=(ifelse(test.pred>.50, "Hillary Clinton", "Donald Trump")))

training_error <- calc_error_rate(lasso.train$predict.train, trn.cl$candidate)
test_error <- calc_error_rate(lasso.test$predict.test, tst.cl$candidate)

records[3,1] <-training_error
records[3,2]<-test_error
records
#19=============================================================================
library(ROCR)
pred.tree <- predict(tree.pruned, tst.cl, type = "vector")
perf.tree <- performance(prediction(pred.tree[,13], as.numeric(tst.cl$candidate)), "tpr", "fpr")

pred.log <- predict(glm.fit, tst.cl, type = "response")
perf.log <- performance(prediction(pred.log, as.numeric(tst.cl$candidate)), "tpr", "fpr")

pred.lasso <- predict(lasso_mod, newx =x.tst, s = best, type="response")
perf.lasso <- performance(prediction(pred.lasso, as.numeric(tst.cl$candidate)), "tpr", "fpr")

tree.lm = plot(perf.tree, col=2, lwd=3, text=T, main="ROC Curve")
log.lm = plot(perf.log, add=TRUE, col=3, lwd=3)
lasso.lm = plot(perf.lasso, add=TRUE, col=4, lwd=3)

legend("right", legend=c("decision tree", "logistic","lasso logistic"), col = c(2,3,4),lty=1:1)
abline(0,1)

auc = performance(prediction(pred.tree[,13], as.numeric(tst.cl$candidate)), "auc")@y.values
auc

auc1 = performance(prediction(pred.log, as.numeric(tst.cl$candidate)), "auc")@y.values
auc1

auc2 = performance(prediction(pred.lasso, as.numeric(tst.cl$candidate)), "auc")@y.values
auc2
records
matrixAUC = matrix(NA, nrow=1, ncol=3)
auc1[[1]]
auc[[1]]
matrixAUC[1,1]=auc[1][1,1]
auc[1]
matrixAUC[1,1]=auc2
#20=============================================================================
#random forest------------------------------------------------------------------
library(randomForest)

trn.cl$candidate <- factor(trn.cl$candidate)
rf = randomForest(candidate ~ ., data=trn.cl, importance=TRUE)
invisible(rf)
rf
importance(rf)
#The random forest uses 500 trees and tried 5 variables at each split. The out-of-bounds error rate was 6.23%. The two most important variables
#were White and Transit. Which were also very important in decision trees. This is to be expected because
#random forests is similar to decision tree, but it uses several smaller decision trees by applying bootstrap method to it. 
varImpPlot(rf, cex=.7)
#The plot also indicates that Transit is the most important variable. 


#boosting-----------------------------------------------------------------------
library(gbm)
set.seed(1)
boosting <- gbm(ifelse(candidate=="Hillary Clinton",1,0)~.,distribution="bernoulli",data=trn.cl, n.trees=1000, shrinkage=0.01)
kable(head(summary(boosting)))%>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width=FALSE)
invisible(head(summary(boosting)))
#The boosting model also shows a similar trend of Transit and White being the most important variables. 
#This is because the method is similar to random forests, except that it makes each new tree with information from previous trees.
#SVM----------------------------------------------------------------------------
library(e1071)
#trn.cl<- trn.cl %>% mutate(candidate=as.factor(ifelse(candidate=="Hil")))
svm_fit=svm(ifelse(candidate=="Hillary Clinton",1,0)~., data=trn.cl, kernel="radial", cost=1,scale=TRUE)


pred <- predict(svm_fit, newdata=tst.cl)

tst.cl <- tst.cl %>% mutate(pred.svm=pred)
confusionmatrix =table(pred = pred, truth =tst.cl$candidate) #do not include looks weird
confusionmatrix
#class(trn.cl$candidate)="character"
set.seed(1) 
tune.out=tune(svm,candidate~.,data=trn.cl,kernel="radial",ranges=list(cost=c(0.001, 0.01, 0.1,1,10,100)))

summary(tune.out)
#The best cost was 10 and its error was 0.06147337. 
best<- tune.out$best.model
tune.pred<- predict(best,tst.cl)
tune.pred
levels(tst.cl$candidate)
tru <- droplevels(tst.cl$candidate)
confmat=table(predict = tune.pred, truth = tru)
kable(confmat)%>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width=FALSE)
sum(diag(confmat)/sum(confmat))

1 - sum(diag(confmat)/sum(confmat))


svmfitbestcost=svm(ifelse(candidate=="Hillary Clinton",1,0)~., data=trn.cl, kernel="radial", cost=10,scale=TRUE)
#svmfitbestcost

pred.svmbestcost <- predict(svmfitbestcost, newdata=tst.cl)

confusionmatrixbestcost= table(pred=pred.svmbestcost, truth= tru)
#confusionmatrixbestcost
#KNN----------------------------------------------------------------------------
library(class)
set.seed(1)
YTrain = trn.cl$candidate
XTrain = trn.cl %>% select(-candidate)
# YTest is the true labels for High on the test set, Xtest is the design matrix
YTest = tst.cl$candidate
XTest = tst.cl %>% select(-candidate)

validation.error = NULL
allK = 1:50

set.seed(1)
for (i in allK) {  # Loop through different number of neighbors
  pred.Yval = knn.cv(train=XTrain, cl=YTrain, k=i) # Predict on the left-out validation set
  validation.error = c(validation.error, mean(pred.Yval!=YTrain)) # Combine all validation errors
}



#validation.error

numneighbor = max(allK[validation.error == min(validation.error)])
numneighbor

set.seed(1)
pred.YTest = knn(train=XTrain, test=XTest[,-26], cl=YTrain, k=numneighbor)

conf.matrix = table(predicted=pred.YTest, true=ifelse(YTest=="Hillary Clinton",1,0)) 
conf.matrix

sum(diag(conf.matrix)/sum(conf.matrix))

1 - sum(diag(conf.matrix)/sum(conf.matrix))
