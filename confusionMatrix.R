# Code to create a comprehensive confusion matrix and evaluation criteria for 
# classification models. 

confusionMatrix <- function(df = NULL, model = NULL, x = NULL, y = NULL) {
  if (is.null(df)) {
    df <- data.frame(fits = predict(model, x), actuals = y)
    confuse <- with(df, table(actuals, fits))
    sensitivity <- confuse[2,2] / (confuse[2,2] + confuse[2,1])
    precision <- confuse[2,2] / (confuse[2,2] + confuse[1,2])
    specificity <- confuse[1,1] / (confuse[1,1] + confuse[1,2]) 
    overall_accuracy <- (confuse[1,1] + confuse[2,2]) / sum(confuse)
    f1_measure <- ((sensitivity * precision)/(sensitivity + precision)) * 2
    m <- as.data.frame(rbind(sensitivity, precision, specificity, 
                             overall_accuracy, f1_measure))
    m <- round(m,3)
    print(confuse)
    print(m)
  } else {
    confuse <- with(df, table(actuals, fits))
    sensitivity <- confuse[2,2] / (confuse[2,2] + confuse[2,1])
    precision <- confuse[2,2] / (confuse[2,2] + confuse[1,2])
    specificity <- confuse[1,1] / (confuse[1,1] + confuse[1,2]) 
    overall_accuracy <- (confuse[1,1] + confuse[2,2]) / sum(confuse)
    f1_measure <- ((sensitivity * precision)/(sensitivity + precision)) * 2
    m <- as.data.frame(rbind(sensitivity, precision, specificity, 
                             overall_accuracy, f1_measure))
    m <- round(m,3)
    print(confuse)
    print(m)
  }
}
