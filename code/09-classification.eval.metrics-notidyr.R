# http://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html
# code in article wrapped in function, link at bottom of article
# wrapper function from here: https://github.com/saidbleik/Evaluation/blob/master/eval.R
# adapted by Maria Wang

# input actual & predicted vectors or actual vs predicted confusion matrix

ConfusionMatrix = function(actual=NULL, predicted=NULL, cm=NULL){
  if(is.null(cm)) {
    naVals = union(which(is.na(actual)), which(is.na(predicted)))
    if(length(naVals) > 0) {
      actual = actual[-naVals]
      predicted = predicted[-naVals]
    }
    f = factor(union(unique(actual), unique(predicted)))
    actual = factor(actual, levels = levels(f))
    predicted = factor(predicted, levels = levels(f))
    cm = as.matrix(table(Actual=actual, Predicted=predicted))
  }
  return(cm)
}


EvaluateMet = function(actual=NULL, predicted=NULL, cm=NULL){
  if(is.null(cm)) {
    naVals = union(which(is.na(actual)), which(is.na(predicted)))
    if(length(naVals) > 0) {
      actual = actual[-naVals]
      predicted = predicted[-naVals]
    }
    f = factor(union(unique(actual), unique(predicted)))
    actual = factor(actual, levels = levels(f))
    predicted = factor(predicted, levels = levels(f))
    cm = as.matrix(table(Actual=actual, Predicted=predicted))
  }

  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the classes
  q = colsums / n # distribution of instances over the predicted classes
  FP = cm[1,2]
  FN = cm[2,1]
  TP = cm[2,2]
  TN = cm[1,1]
  FPR = FP/(FP+TN)
  FNR = FN/(FN+TP)

  FDR = FP/(FP+TP)
  FOR = FN/(FN+TN)

  #accuracy
  accuracy = sum(diag) / n

  #per class prf
  recall = diag / rowsums #=specificity (neg recall) & sensitivity (pos recall)
  precision = diag / colsums #Negative Predictive Value & Positive Predictive Value
  f1 = 2 * precision * recall / (precision + recall)
  #f2 = (1+(2^2))/(2^2) * precision * recall / (precision + recall) # = 1.25/2*f1

  #random/expected accuracy
  expAccuracy = sum(p*q)

  #kappa
  kappa = (accuracy - expAccuracy) / (1 - expAccuracy)
  #tss = sum(recall)-1

  #random guess
  #rgAccuracy = 1 / nc
  #rgPrecision = p
  #rgRecall = 0*p + 1 / nc
  #rgF1 = 2 * p / (nc * p + 1)

  #random weighted guess
  #rwgAccurcy = sum(p^2)
  #rwgPrecision = p
  #rwgRecall = p
  #rwgF1 = p

  #### return
  metrics = rbind(
    FP = FP,
    FN = FN,
    TP = TP,
    TN = TN,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1 = f1,
    #F2 = f2,
    Kappa = kappa,
    FPR = FPR,
    FNR = FNR)#,
  # RandomGuessAccuracy = rgAccuracy,
  # RandomGuessPrecision = rgPrecision,
  # RandomGuessRecall = rgRecall,
  # RandomGuessF1 = rgF1,
  # RandomWeightedGuessAccuracy = rwgAccurcy,
  # RandomWeightedGuessPrecision = rwgPrecision,
  # RandomWeightedGuessRecall = rwgRecall,
  # RandomWeightedGuessF1 = rwgF1)

  metrics.avg <- (metrics[,1] + metrics[,2] )/2

  met.t.class0 <- as.data.frame(t(metrics[6:8,2]))
  names(met.t.class0) <- paste0(names(met.t.class0), ".0")

  met.t.class1 <- as.data.frame(t(metrics[6:8,2]))
  names(met.t.class1) <- paste0(names(met.t.class1), ".1")

  met.t <- as.data.frame(t(metrics.avg))
  met.t <- met.t %>% dplyr::select(Accuracy, Precision.avg=Precision, Recall.avg=Recall, F1.avg=F1, FP, TP, TN, FN, FPR, FNR, Kappa)

  met.t <- cbind(met.t, met.t.class1)

  return(met.t)
}



EvaluateMetRand = function(y){
  n = length(y) #sum(cm) number of instances = nrows = n obs
  nc = n_distinct(y) #nrow(cm) number of classes = 2 (converted or not)
  rowsums = table(y) # number of instances per class (n of actual converted and not converted)
  p = rowsums / n # actual converted and not converted over nrows

  #random guess
  rgAccuracy = 1 / nc
  rgPrecision = p
  rgRecall = 0*p + 1 / nc
  rgF1 = 2 * p / (nc * p + 1)
  rgF2 = 1.25 * p / (nc * p + 1)

  #random weighted guess
  rwgAccuracy = sum(p^2)
  rwgPrecision = p
  rwgRecall = p
  rwgF1 = p
  rwgF2 = 1.25/2*p

  randmetrics = rbind(rgAccuracy, rgPrecision, rgRecall, rgF1, rgF2, rwgAccuracy, rwgPrecision,   rwgRecall, rwgF1, rwgF2)

  randmet.avg <- (randmetrics[,1] + randmetrics[,2] )/2
  randmetrics = cbind(randmetrics, randmet.avg)
  return(randmetrics)
}

#rm(actual, predicted, cm)

#Testing with start_output data, created from 08-rubberexpansion_maxlikeli-parallel
#start_output <- read.csv('/output/start_output.2018-08-22.KHM-50-10-25.test2e4_all-lin.csv')
#start_output <- fread('C:/Users/bop17mw/Desktop/RubberExpansionModelDesktop/output/start_output.2018-09-28.150-10.MMSEA_19K.csv')
# start_output <- fread(paste0(IntFolder, 'start_output/start_output.2020-08-04.CHN_train10.80-10-linear.csv'))
#
# head(start_output)
# cmtry = ConfusionMatrix(actual=start_output$outcome_bin, predicted=start_output$convtd.in.2014)
# cmtry
#
#check = EvaluateMet(actual=start_output2$outcome_bin, predicted=start_output2$convtd.in.2013)
#check
#
# actual=start_output$outcome_bin
# predicted=start_output$convtd.in.2014
# cm=cmtry
