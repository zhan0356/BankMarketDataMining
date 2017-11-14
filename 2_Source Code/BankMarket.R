#### Read csv directly from github------------------------------------------------------------------
df <- read.csv('https://raw.githubusercontent.com/zhan0356/BankMarket/master/bank-full.csv', header=T, sep=';')
# look at the summary
summary(df)
# pdays: number of days that passed by after the client was last contacted from a previous campaign
# most data = -1 (not contacted)
# recode this as very large number
# to differentiate the '-1' with '1', '2'
df$pdays[df$pdays==-1] <- .Machine$integer.max





#### Pre-processing--------------------------------------------------------------------------------
### Test for independence: test independence of the output column and other columns------
xindep <- function(df, class) {
  # df: source dataframe
  #class: class attribute name (string)
  for (i in 1:ncol(df)) {
    cname <- colnames(df[i])
    if (cname != class) {
      print(cname)
      print(summary(table(df[[class]], df[[i]])))
      # the summary of the table will give out the chi-square test for the table
      # the chi-square test for he last row and each other row will be conducted
    }
  }
}
xindep(df, 'y')



### Cauculate the percentage of unknown value--------------------------------------------
percNA <- function(df, NAval=NA) {
  # check percentage of unknown or missing values
  # df: source dataframe
  # NAval: value taken to be NA (missing)
  for (i in 1:ncol(df)) {
    print(colnames(df[i]))
    if (is.na(NAval)) {
      # if NA
      print(mean(is.na(df[[i]])))
    } else {
      # if other unknown values
      print(mean(df[[i]]==NAval))
    }
  }
}
# cms = columnId of columns with unknown values
cms <- c(2, 4, 9, 16)
percNA(df[cms], 'unknown')
# questionable to impute for contact and poutcome



### Convert the unknown value to NA------------------------------------------------------
convertNA <- function(df, columnIds, NAval='unknown') {
  # df: source dataframe
  # columnIds: columnIds to convert
  # NAval: original value to convert to NA
  for (i in columnIds) {
    df[[i]][df[[i]]==NAval] <- NA
    # drop unused factor
    if (is.factor(df[[i]])) df[[i]] <- droplevels(df[[i]])
  }
  return(df)
}
# filter columnIds accordingly
# percentage of column contact 29% and poutcome 82%, imputation will increase bias if there are too many unknown
cms <- c(2, 4)
# convert the 'unknown' to 'NA' accordingly
df <- convertNA(df, cms)



### Creation of new features-------------------------------------------------------------
# the data is ordered in date from May 2008 to November 2010
# create date, year and weekofday columns
# rows 1: 27729 year 2008
# rows 27730:42591 year 2009
# rows 42593:45211 year 2010

## create new column year------------------------------------------------------
i <- 'year'
df[i] <- 2008
df[[i]][27730:42591] <- 2009
df[[i]][42592:45211] <- 2010

## create new column date------------------------------------------------------
Sys.setlocale("LC_TIME","C")
df['date_c'] <- as.Date(paste(df$day, df$month, df$year), '%d %b %Y')

## create new column day of week-----------------------------------------------
# strftime() takes a date-time object and makes an character vector. 
# the %u will display the output as the day of week
df['dayofweek'] <- as.integer(strftime(df$date_c,'%u'))

## convert the month into integer and create new features in complex format----
# reorder the months numerically
month2num <- function(df) {
  i <- 'month'
  # the level of the column month is in the following order
  # levels(df$month)
  # "apr" "aug" "dec" "feb" "jan" "jul" "jun" "mar" "may" "nov" "oct" "sep"
  # replace the str with unteger
  nummth <- c(4, 8, 12, 2, 1, 7, 6, 3, 5, 11, 10, 9)
  names(nummth) <- levels(df[[i]])
  df[[i]] <- nummth[df[[i]]]
  # convert the month into complex number format
  z = data.frame()
  for (x in 1:12) {
    z <- rbind(z, data.frame(mthx=cos(2*pi*x/12),mthy=sin(2*pi*x/12)))
  }
  m_x <- 'mthx'; m_y <- 'mthy'
  # two new columns (dateframe) looks up the z dataframe at month row
  df[m_x] <- z[[m_x]][df[[i]]]
  df[m_y] <- z[[m_y]][df[[i]]]
  return(df)
}
df <- month2num(df)

## read Euribor-interbank interest rates and merge it to dataframe-------------
# https://www.bportugal.pt/EstatisticasWeb/
# fetch the data from bank of Portugal
getEuribor <- function(df) {
  require(readxl)
  i <- 'date_c'
  ebdf <- readxl::read_xlsx('Euribor1wk.xlsx')
  ebdf[[i]] <- as.Date(ebdf[[i]])
  # create list of unique dates from source
  dftmp <- as.data.frame(unique(df[[i]]))
  colnames(dftmp) <- i
  # perform the Vlookup equivalent between dftmp and Euribor dataframe
  dftmp <- merge(dftmp, ebdf, all.x=T)
  # interpolate all missing values
  # replace NA with interpolated values
  require(zoo)
  dftmp$Euribor1wk <- na.approx(dftmp$Euribor1wk)
  # get running difference in interest rates
  # the latter one - the prior one
  x <- as.data.frame(apply(dftmp[2], 2, diff))
  # start with 0
  x <- rbind(0, x)
  colnames(x) <- 'Euridiff'
  dftmp <- cbind(dftmp, x)
  # return merged dataframe
  return(merge(df, dftmp))
}
df <- getEuribor(df)

### impute with Hmisc----------------------------------------------------------
# install.packages('Hmisc')
library(Hmisc)
imputefunc <- function(df, columnIds, n=1) {
  # calling aregImpute from library(Hmisc)
  # Multiple Imputation using Additive Regression, Bootstrapping, and Predictive Mean Matching
  # inputs
  # df: source dataframe
  # columnIds: column indices to impute
  # n: number of output arrays
  impute_arg <- aregImpute(
    as.formula(
      paste('~', paste( colnames(df[columnIds]), collapse=' + '))
    ), data=df, n.impute=n)
  # merging imputed data with source dataframe
  imputed <- impute.transcan(impute_arg, data=df, imputation=1, list.out=T, pr=F, check=F)
  imputed.data <- as.data.frame(do.call(cbind, imputed))
  
  # convert to factors those previously were factors
  for (i in columnIds) {
    vec <- df[[i]]
    # col indices of the imputed.data may be different from indices of source dataframe
    # therefore, use column names instead
    cname <- colnames(df[i])
    if (is.factor(vec)) {
      imputed.data[[cname]] <- factor(imputed.data[[cname]], labels=levels(vec))
    }
    # copy the imputed values to source dataframe
    df[[i]] <- imputed.data[[cname]]
  }
  return(df)
}
# impute dataframe
df <- imputefunc(df, c(2:14, 17))



### write processed dataframe to csv the if necessary--------------------------
writefile <- function(df, filename='bkmktg.csv') {
  write.csv(df, file=filename, fileEncoding='UTF-16LE')
}
#writefile(df)




#### Simple visulization---------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)

### create time series trend---------------------------------------------------
t <- table(df$date_c, df$y)
plot(x=as.Date(names(t[,2])), y=t[,2], type='l', xlab='date', ylab='yes_count')



### Education level vs Call duration with respect to different outcome---------
dataSubsetBalance <- df[,c(5,13, 18)]
# clients with tertiary education need less time to make up their minds
ggplot(dataSubsetBalance, aes(education, fill = y)) + geom_density(alpha = 0.3) +
  xlab("Education Level") +
  ylab("Call Duration")+
  ggtitle("Call duration - Education")+
  theme(plot.title = element_text(hjust = 0.5))



### Density graph duration for different outcome-------------------------------
# Negative density
ndensity<-ggplot(df[df$y %in% ("no"),], aes(duration))+
  geom_histogram(aes(y=..density..),colour="black", fill="white",binwidth=50)+
  geom_density(alpha=.2, fill="cyan3")+
  geom_vline(aes(xintercept=mean(df[df$y %in% ("no"),]$duration)),color="blue", linetype="dashed", size=1)+
  xlim(-10, 1000)+
  ggtitle("Duration of contact with outcome = no")+
  theme(plot.title = element_text(hjust = 0.5))

# Positive density
ydensity<-ggplot(df[df$y %in% ("yes"),], aes(duration))+
  geom_histogram(aes(y=..density..),colour="black", fill="white",binwidth=50)+
  geom_density(alpha=.2, fill="cyan3")+
  geom_vline(aes(xintercept=mean(df[df$y %in% ("yes"),]$duration)),color="blue", linetype="dashed", size=1)+
  xlim(-10, 1000)+
  ggtitle("Duration of contact with outcome = yes")+
  theme(plot.title = element_text(hjust = 0.5))

# When a client talks for more duration, it is more likely that he may subscribe a term deposit
grid.arrange(ndensity, ydensity)





#### Cross validation training---------------------------------------------------------------------
#http://topepo.github.io/caret/index.html
#m: method
#g: tuning parameters
#prp: caret can handle proprocessing for each fold validation
#prp=NULL means to skip any preprocessing
#some ML algorithms like knn, neural net and SVM will perform better if data has been centered and scaled
#prp=c('center','scale') means do center and scaling as pre-pressing
#some perform better with propressing
#kfold: number of folds
#drawgraph: draw MCC graph or not
#prn:print mean value of auc and mcc
cv5train <- function(df, m, g, prp=c('center','scale'), kfold=5, drawgraph=F, prn=T) {
  # function to perform kfold validations and return AUC and MCC scores
  require(caret); require(ROCR); if (m=='svmRadial') require(kernlab)
  #use same train control for all the method
  
  #classProbs: 
  #a logical; class probabilities be computed for classification models (along with predicted values) in each resample
  #summaryFunction: 
  #a function to compute performance metrics across resamples
  ctrl <- trainControl(method='none', classProbs=T, summaryFunction=twoClassSummary)
  
  #set seed to ensure the sampe partition result
  #create kfold partitions, 1/kfold data will be trained
  set.seed(8)
  split <- createDataPartition (df$y, p=(1/kfold), list=F, times=kfold)
  # mccdf: dataframe to collect the mcc scores for the kfolds
  mccdf <- data.frame()
  for (i in 1:kfold) {
    #define training set and testing set
    trn <- df[-split[,i],]
    tst <- df[split[,i],]
    #train on training set
    #metric: A string that specifies what summary metric will be used to select the optimal model
    #ROC is applied here to quantified the model
    if (m=='nnet') {
      #For neural network, liout = F, 
      #otherwise a sigmoidal activation function is used and all of the predictions will be constrained to be on [0, 1]
      #trace = F
      #The nnet function returns training errors during iteration
      #trace = T
      #The nnet function returns training errors but only after each iteration
      fit <- train(y ~ ., data=trn, method=m, metric='ROC'
                   , trControl=ctrl, tuneGrid=g, preProc=prp, linout=F, trace=F)
    } else if (m=='gbm') {
      #For Generalized Boosted Regression Models
      #verbose=F
      fit <- train(y ~ ., data=trn, method=m, metric='ROC'
                   , trControl=ctrl, tuneGrid=g, preProc=prp, verbose=F)
    } else {
      fit <- train(y ~ ., data=trn, method=m, metric='ROC'
                   , trControl=ctrl, tuneGrid=g, preProc=prp)
    }
    p <- predict(fit, newdata=tst, type='prob')
    pred <- ROCR::prediction(p[,2], tst$y)
    #class(pred)
    #slotNames(pred)
    #sn = slotNames(pred)
    #sapply(sn, function(x) length(slot(pred, x)))
    #sapply(sn, function(x) class(slot(pred, x)))
    #We see the the returned result of prediction is an object of class prediction, which an S4 object with a series of slots. 
    #each slot has length 1 and is a list
    
    
    #Evaluate the performance with auc and mcc
    auc.perf <- ROCR::performance(pred, measure='auc')
    #slotNames(auc.perf)
    mcc.perf <- ROCR::performance(pred, measure='mat')
    #slotNames(mcc.perf)
    
    if (drawgraph) plot(mcc.perf)
    # extract maximum matthews coefficient
    yv <- 'y.values'; xv <- 'x.values'
    ind <- which.max( slot(mcc.perf, yv)[[1]] )
    score <- slot(mcc.perf, yv)[[1]][ind]
    cutoff <- slot(mcc.perf, xv)[[1]][ind]
    sc <- c(auc=auc.perf@y.values[[1]], mcc=score, cutoff=cutoff)
    if (prn) print(sc)
    mccdf <- rbind(mccdf, sc)
  }
  # rename the column names
  colnames(mccdf) <- c('auc', 'mcc','cutoff')
  # show mean mcc for the kfold
  if (prn) {
    print(paste('mean auc', round(mean(mccdf[[1]]), 5)
                , 'mean mcc', round(mean(mccdf[[2]]), 5)
    )
    )
  }
  return(mccdf)
}



### Examples of applying cv5train----------------------------------------------
# eXtreme Gradient Boosting
m <- 'xgbTree'
g <- expand.grid(nrounds=1000,eta=.02, max_depth=5, gamma=.75, colsample_bytree=.5, subsample=.75, min_child_weight=7)
mcd <- cv5train(df, m, g)

# C5.0
m <- 'C5.0'
g <- expand.grid(trials=100,model='tree',winnow=F)
mcd <- cv5train(df, m, g,prp=NULL)

# Stochastic Gradient Boosting
m <- 'gbm'
g <- expand.grid(interaction.depth=3, n.trees=1500, shrinkage=.1, n.minobsinnode=11)
mcd <- cv5train(df, m, g)

# Random Forest
m <- 'rf'
g <- expand.grid(mtry=10)
mcd <- cv5train(df, m, g, prp=NULL)

# Bagged AdaBoost
m <- 'AdaBag'
g <- expand.grid(mfinal=80, maxdepth=13)
mcd <- cv5train(df, m, g,prp=NULL)

# Oblique Random Forest
m <- 'ORFlog'
g <- expand.grid(mtry=3)
mcd <- cv5train(df, m, g,prp=NULL)

# neural network
m <- 'nnet'
g <- expand.grid(size=20 , decay= 0.5)
mcd <- cv5train(df, m, g)

# Logistic Model Trees
m <- 'LMT'
g <- NULL
mcd <- cv5train(df, m, g)

# Support Vector Machines with Radial Basis Function Kernel
m <- 'svmRadial'
g <- expand.grid(C=1,sigma=0)
mcd <- cv5train(df, m, g)

# CART
m <- 'rpart'
g <- expand.grid(cp=.0005)
mcd <- cv5train(df, m, g, prp=NULL)

# Multi-Layer Perceptron
m <- 'mlp'
g <- expand.grid(size=8)
mcd <- cv5train(df, m, g)


# Naive Bayes
m <- 'nb'
g <- expand.grid(fL=0, usekernel=T, adjust=.01)
mcd <- cv5train(df, m, g,prp=NULL)

# KNN
m <- 'knn'
g <- expand.grid(k=24)
mcd <- cv5train(df, m, g)

# Partial Least Squares 
m <- 'pls'
g <- expand.grid(ncomp=c(20))
mcd <- cv5train(df, m, g, prp=NULL)

# Boosted Logistic Regression 
m <- 'LogitBoost'
g <- expand.grid(nIter=4)
mcd <- cv5train(df, m, g, prp=NULL)

# Bayesian Generalized Linear Model
m <- 'bayesglm'
g <- NULL
mcd <- cv5train(df, m, g,prp=NULL)

# Generalized Linear Model
m <- 'glm'
g <- NULL
mcd <- cv5train(df, m, g)





#### Exhaustive Parameter Tuning with cross validation --------------------------------------------
require(lattice)
require(caret)



### xgbTree--------------------------------------------------------------------
## Build parameter combinations
xgb_grid = expand.grid(
  nrounds = 100,
  eta = c(0.01, 0.02, 0.03),
  max_depth = c( 4, 5, 6),
  min_child_weight = c(6, 7, 8),
  subsample = c(0.74, 0.75, 0.76),
  colsample_bytree = c(0.4, 0.5, 0.6),
  gamma = c(0.5, 0.75, 1)
)

## Input parameters to measure the performance of grid search
xgb_trcontrol = trainControl(
  method = "cv",                                          #The resampling method: cv---cross validation
  number = 5,                                             #number of folds or number of resampling iterations
  verboseIter = TRUE,                                     #A logical for printing a training log.
  returnData = FALSE,                                     #A logical for saving the data
  returnResamp = "all",                                   #save losses across all models
  classProbs = TRUE,                                      #set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,                      #A function to compute performance metrics across resamples.
  allowParallel = TRUE                                    #A logical for parallel backend
)

## Train for xgbtree algorithm
# The training process will last for hours depending on your setting
# Once the training is finished, the suggested parameters will be output on console window
# reordered need to converted to factor and give names to levels
set.seed(8)
split <- createDataPartition (df$y, p=(1/5), list=F, times=5)
trn <- df[-split[,1],]
tst <- df[split[,1],]
trn$y = as.factor(trn$y)
levels(trn$y) = make.names(levels(trn$y))
xgb_train = train(
  x = as.matrix(subset(trn,select=-c(y))),
  y = trn$y,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree"                                      # specify the method to be trained     
)





#### Column deletion: delete column one by one and run CV5-training--------------------------------
colDel <- function(df, m, g, col=(1:length(df)), prp=c('center','scale')) {
  # function to trim features
  # features are trimmed once-at-a-time and AUC/MCC scores are computed
  # rslt = result dataframe of trimmed feature scores
  rslt <- data.frame()
  # first round initalisation
  init <- F
  refactordf <- function(dfx) {
    # correct the class types of the columns in rslt dataframe
    # the rslt dataframe consists of four columns: number of clumn in df, column string, auc value, mcc value
    for (j in c(1,3,4)) {
      k <- dfx[[j]]
      dfx[[j]] <- as.numeric(as.character(dfx[[j]]))
    }
    dfx[[2]] <- as.character(dfx[[2]])
    return(dfx)
  }
  
  # for each column in dataframe, deselect it and do cv5train
  for (i in col) {
    cname <- colnames(df[i])
    if (cname != 'y'  && cname != 'duration') {
      x <- cv5train(df[-i], m, g, prp)
      k <- c(i, cname, mean(x[[1]]), mean(x[[2]]))
      print(k)
      rslt <- rbind(rslt, k)
      
      if (!init) {
        # initialise result dataframe with appropriate column attributes
        colnames(rslt) <- c('col', 'name', 'auc', 'mcc')
        rslt <- refactordf(rslt)
        init <- T
      }
    }
  }
  # final correction to output dataframe
  rslt <- refactordf(rslt)
  print(rslt[order(rslt$mcc, decreasing=T),])
  return(rslt)
}



### Examples of applying colDel------------------------------------------------
# Partial Least Squares 
m <- 'glm'
g <- NULL
colDel(df, m, g)





#### CV-5 ensemble stack with backwards feature selection-------------------------------------------
# cross validation ensemble model
cv5ensemble <- function(df, kfold=5) {
  require(caret); require(ROCR)
  ctrl <- trainControl(method='none', classProbs=T, summaryFunction=twoClassSummary)
  set.seed(8)
  split <- createDataPartition (df$y, p=(1/kfold), list=F, times=kfold)
  cs <- c('center','scale')
  # build the list of base classifiers
  # i = index of the list of base classifiers
  lbc <- list(); i <- 0
  
  i <- i + 1
  lbc[[i]] <- list(m='LogitBoost', g=data.frame(nIter=4), cfil=c(1,13,17,18), prp=NULL)
  i <- i + 1
  lbc[[i]] <- list(m='rpart', g=data.frame(cp=.0005), cfil=c(-16,-7), prp=NULL)
  i <- i + 1
  lbc[[i]] <- list(m='knn', g=data.frame(k=24), cfil=c(-11,-3,-4,-5,-15,-7,-6,-24,-9,-16), prp=cs)
  i <- i + 1
  lbc[[i]] <- list(m='nb', g=data.frame(fL=0, usekernel=T, adjust=.01), cfil=c(1,13,18), prp=NULL)
  #i <- i + 1
  #lbc[[i]] <- list(m='svmRadial', g=data.frame(C=1,sigma=0)
  #                 , cfil=c(-3,-5,-14,-7,-9,-4,-6,-16,-22,-1,-15,-17,-24,-10,-2,-20,-8), prp=cs)
  #i <- i + 1
  #lbc[[i]] <- list(m='mlp', g=data.frame(size=8), cfil=c(-3,-10), prp=cs)
  # i <- i + 1
  # lbc[[i]] <- list(m='nnet', g=data.frame(.size=20, .decay=.5)
  #                 , cfil=c(-21,-3,-19,-14,-16,-15,-7), prp=cs)
  # i <- i + 1
  # lbc[[i]] <- list(m='pls', g=data.frame(ncomp=11), cfil=c(-19,-23,-22,-24,-5), prp=NULL)
  # i <- i + 1
  # lbc[[i]] <- list(m='glm', g=NULL, cfil=c(-14,-21), prp=cs)
  # i <- i + 1
  # lbc[[i]] <- list(m='rf', g=data.frame(.mtry=10), cfil=c(-11,-19,-21,-22), prp=NULL)
  # i <- i + 1
  # lbc[[i]] <- list(m='gbm'
  #                  , g=data.frame(interaction.depth=3, n.trees=1500, shrinkage=.1, n.minobsinnode=11)
  #                  , cfil=c(-3,-2), prp=cs)
  # i <- i + 1
  # lbc[[i]] <- list(m='xgbTree'
  #                  , g=data.frame(nrounds=1000
  #                                 , eta=.02
  #                                 , max_depth=5
  #                                 , gamma=.75
  #                                 , colsample_bytree=.5
  #                                 , subsample=.75
  #                                 , min_child_weight=7)
  #                  , cfil=c(-5,-2,-19,-15,-9,-12,-3), prp=cs)
  # i <- i + 1
  # lbc[[i]] <- list(m='C5.0', g=data.frame(trials=100,model='tree',winnow=F)
  #                  , cfil=c(-3,-16,-2,-5), prp=NULL)
  # i <- i + 1
  # lbc[[i]] <- list(m='bayesglm', g=NULL, cfil=c(-14,-5,-2,-4,-1,-15), prp=NULL)
  # i <- i + 1
  # lbc[[i]] <- list(m='LMT', g=data.frame(iter=11), cfil=c(-17,-7,-3), prp=cs)
  # i <- i + 1
  # lbc[[i]] <- list(m='ORFlog', g=data.frame(mtry=3), cfil=c(-3), prp=NULL)
  # i <- i + 1
  # lbc[[i]] <- list(m='AdaBag', g=data.frame(mfinal=80, maxdepth=13)
  #               , cfil=c(-2,-24), prp=NULL)
  # start training the folds
  fmla <- as.formula('y ~ .'); metric <- 'ROC'
  # start for-loop for the kfold iteration
  totalsc <- 0
  for (j in 1:kfold) {
    print(sprintf('Starting fold %d -----', j))
    trn <- df[-split[,j],]
    # dfp = dataframe initialised with all class labels
    dfp <- df['y']
    # Each base classifier learn about training data and produce the predictions
    for (k in 1:i) {
      #assign the method to m
      #assign the tuning parameters to g
      #assign the subsetting of dataframe to cfil
      #assign the preprocessing method to prp
      m <- lbc[[k]]$m
      g <- lbc[[k]]$g
      cfil <- lbc[[k]]$cfil
      prp <- lbc[[k]]$prp
      print(sprintf('Engaging %s ...', m))
      if (m=='nnet') {
        fit <- train(fmla, data=trn[cfil], method=m, metric=metric
                     , trControl=ctrl, tuneGrid=g, preProc=prp, linout=F, trace=F)
      } else if (m=='gbm') {
        fit <- train(fmla, data=trn[cfil], method=m, metric=metric
                     , trControl=ctrl, tuneGrid=g, preProc=prp, verbose=F)
      } else {
        if (m=='svmRadial') require(kernlab)
        fit <- train(fmla, data=trn[cfil], method=m, metric=metric
                     , trControl=ctrl, tuneGrid=g, preProc=prp)
      }
      # make predictions on all rows
      p <- predict(fit, newdata=df[cfil], type='prob')
      # rename the yes column to the method name
      colnames(p)[2] <- m
      dfp <- cbind(dfp, p[2])
    }
    # return dfp here if you want the dataframe of base classifier predictions
    #return(dfp)
    
    # naive bayes model as the ensemble classifier
    # use the naive bayes to train dfp
    m <- 'nb'
    g <- data.frame(fL=0, usekernel=F, adjust=.01)
    # switch to nnet for the 5-tribe ensemble
    # m <- 'nnet'
    # g <- data.frame(.size=1, .decay=.1)
    prp <- NULL
    print(sprintf('Ensembling... using %s',m))
    # train on train set for the given fold
    fit <- train(fmla, data=dfp[-split[,j],], method=m, metric=metric
                 , trControl=ctrl, tuneGrid=g, preProc=prp)
    # predict on test set for the given fold
    tst <- dfp[split[,j],]
    p <- predict(fit, newdata=tst, type='prob')
    pred <- ROCR::prediction(p[,2], tst$y)
    mcc.perf <- ROCR::performance(pred, measure='mat')
    mcc.score <- max(slot(mcc.perf, 'y.values')[[1]], na.rm=TRUE)
    print(sprintf('MCC score = %.5f', mcc.score))
    totalsc <- totalsc + mcc.score
  }
  # end of kfold iterations
  # print out mean score
  print(sprintf('Mean MCC score for %i folds = %.5f', kfold, totalsc/kfold))
}

cv5ensemble(df)



### Sample output of ensemble learning-----------------------------------------
#[1] "Starting fold 1 -----"
#[1] "Engaging LogitBoost ..."
#[1] "Engaging rpart ..."
#[1] "Engaging knn ..."
#[1] "Engaging nb ..."
#[1] "Ensembling... using nb"
#[1] "MCC score = 0.60086"
#[1] "Starting fold 2 -----"
#[1] "Engaging LogitBoost ..."
#[1] "Engaging rpart ..."
#[1] "Engaging knn ..."
#[1] "Engaging nb ..."
#[1] "Ensembling... using nb"
#[1] "MCC score = 0.60109"
#[1] "Starting fold 3 -----"
#[1] "Engaging LogitBoost ..."
#[1] "Engaging rpart ..."
#[1] "Engaging knn ..."
#[1] "Engaging nb ..."
#[1] "Ensembling... using nb"
#[1] "MCC score = 0.62162"
#[1] "Starting fold 4 -----"
#[1] "Engaging LogitBoost ..."
#[1] "Engaging rpart ..."
#[1] "Engaging knn ..."
#[1] "Engaging nb ..."
#[1] "Ensembling... using nb"
#[1] "MCC score = 0.62273"
#[1] "Starting fold 5 -----"
#[1] "Engaging LogitBoost ..."
#[1] "Engaging rpart ..."
#[1] "Engaging knn ..."
#[1] "Engaging nb ..."
#[1] "Ensembling... using nb"
#[1] "MCC score = 0.61225"
#[1] "Mean MCC score for 5 folds = 0.61171"