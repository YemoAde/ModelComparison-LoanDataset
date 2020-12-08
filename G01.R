source('RProg_Data_Cleaning.R')
library(Metrics)
library(caret)
library(vtreat)
# Package Exploration List
# 1. DMwR - 
# 2. FSelector - Information Gain
# 3. Metrics
# 4. caret

# Remove Missing Data
# Create a Balanced dataset
# Select the best Predictive Variables
# Create Train and Test
# Build a Logistic Regression Model
# Use Cross Validation on Logistic Regression Model

# Build a Tree Model
# Tune the Tree Model Through GridSearch

# Build an SVM Model
# Tune the svm model through tune.svm()


# Evaluate the accuracy of each model
# Get AUC of each model

# 1. Remove the Missing Data 
  merged_data <- na.omit(merged_data)
  merged_data %>% is.na %>% sum()
  # 0

# 1.1 Remove the Empty Strings on Marital Status
  merged_data <- merged_data %>% filter(Marital_Status != "")
# 1.2 Remove the Empty Strings on Profession
  merged_data <- merged_data %>% filter(Profession != "")
# 1.3 Remove the Empty Strings on Gender Status
  merged_data <- merged_data %>% filter(Gender != "")

# 1.4
  set.seed(123)
  sample_size <- floor(0.70 * nrow(merged_data))
  train_ind <- sample(seq_len(nrow(merged_data)), size = sample_size)
  # Train
  data_train <- merged_data[train_ind,]
  # Test
  data_test <- merged_data[-train_ind,]
  
# 1.5 Balancing the Train Data Set
  library(ROSE)
  # type coercion
  merged_data_balanced <- merged_data %>% mutate_if(is.character,as.factor)
  merged_data_balanced <- merged_data_balanced %>% mutate_if(is.integer,as.numeric)
  merged_data_balanced <- ROSE(Performance_Tag~., data=merged_data_balanced, seed=123)$data
  
  data_train_balanced <- data_train %>% mutate_if(is.character,as.factor)
  data_train_balanced <- data_train_balanced %>% mutate_if(is.integer,as.numeric)
  data_train_balanced <- ROSE(Performance_Tag~., data=data_train_balanced, seed=123)$data
  
# 1.6 Create Balanced Train and Test Set
  
  data_test_balanced <- data_test %>% mutate_if(is.character,as.factor)
  data_test_balanced <- data_test_balanced %>% mutate_if(is.integer,as.numeric)
  

# 2. Select important variables using information gain
  library(FSelector)
  merged_data$Performance_Tag <- factor(merged_data$Performance_Tag)
  info_gain <- information.gain(Performance_Tag ~ ., merged_data)
  info_gain <- info_gain %>% arrange(attr_importance)
  cols = rownames(info_gain)
  ggplot(info_gain, aes(x= reorder(cols, attr_importance), y = attr_importance)) + geom_col() + coord_flip()

# 2.1 List of Selected attributes
  selected_variables <- c("Performance_Tag", "Avg_CC_Utilization_12_months", 
                          "Trades_12_months", "Inquiries_12_months", "Outstanding_Balance",
                          "No_Of_30_DPD_6_months", "PL_Trades_12_months", 
                          "Months_In_Current_Residence","Total_No_of_trades", "No_Of_30_DPD_12_months",
                          "No_Of_90_DPD_12_months")
  mini_data <- subset(merged_data, select=selected_variables)
  
# 3. Logistic Regression Model
    # 3.1 Step Regression Model
      model_0 <- glm(Performance_Tag ~ . , family = binomial(link = 'logit'), data = data_train_balanced)
      step_model_0 <- step(model_0, trace = 0)
      model0_pred <- predict(step_model_0, newdata = data_test, type="response")
      mean(model0_pred == data_test$Performance_Tag)
      auc(model0_pred, data_test$Performance_Tag)
      
    # 3.1.1 Cross Validation
    (step_model_0.err <- cv.glm(merged_data, step_model_0, K = 10)$delta)

    
    # 3.2 Model based on Info gain
      logistic_model <- glm(Performance_Tag ~ ., family = "binomial", data = data_train_balanced[,selected_variables])
      logistic_model_pred <- predict(logistic_model, newdata = data_test, type = "response")
      
      logistic_model_predicted <- factor(ifelse(logistic_model_pred >= 0.5, "Yes", "No"))
      logistic_model_actual <- factor(ifelse(data_test$Performance_Tag==1,"Yes","No"))
      conf <- confusionMatrix(logistic_model_predicted, logistic_model_actual, positive = "Yes")
      conf
      table(logistic_model_actual, logistic_model_predicted)
      mean(logistic_model_actual == logistic_model_predicted)
    
      # Cutoff finding function
      predict_final_cutoff <- function(cutoff) 
      {
        predicted_perform <- factor(ifelse(logistic_model_pred >= cutoff, "Yes", "No"))
        conf <- confusionMatrix(predicted_perform, logistic_model_actual, positive = "Yes")
        acc <- conf$overall[1]
        sens <- conf$byClass[1]
        spec <- conf$byClass[2]
        out <- t(as.matrix(c(sens, spec, acc))) 
        colnames(out) <- c("sensitivity", "specificity", "accuracy")
        return(out)
      }
      
      s = seq(.1,.7,length=100)
      OUT = matrix(0,100,3)
      for(i in 1:100){
        OUT[i,] = predict_final_cutoff(s[i])
      }
      logistic_model_cutoff_final <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
      # Prediction on cutoff
      logistic_model_cutoff_pred <- factor(ifelse(logistic_model_pred >= logistic_model_cutoff_final, "Yes", "No"))
      table(logistic_model_actual, logistic_model_cutoff_pred)
      # Creating Confusion Matrix
      logistic_model_final <- confusionMatrix(logistic_model_cutoff_pred, logistic_model_actual, positive = "Yes")
      logistic_model_final
      
      auc(data_test$Performance_Tag, logistic_model_pred)
      ROC <- roc(data_test$Performance_Tag, logistic_model_pred)
      plot(ROC, col = "red")
    
    # # 3.2.1 5 fold CV
      nRows <- nrow(merged_data_balanced)
      splitPlan <- kWayCrossValidation(nRows, 5, NULL, NULL)
      cvOut <- NULL
      for(i in 1:5) {
        split <- splitPlan[[i]]
        targetModel <- glm(Performance_Tag ~ ., family = "binomial", data = merged_data_balanced[split$train, selected_variables ])
        cvOut[split$app] <- predict(targetModel, newdata = merged_data_balanced[split$app, selected_variables], type = 'response')
      }
      cv_conf <- confusionMatrix(factor(ifelse(merged_data_balanced$Performance_Tag==1, "Yes", "No")), 
                                 factor(ifelse(cvOut >= 0.5, "Yes", "No")),
                                 positive = "Yes")
      cv_conf
      auc(merged_data_balanced$Performance_Tag, cvOut)
      ROC <- roc(data_test$Performance_Tag, logistic_model_pred)
      plot(ROC, col = "red")
    

  
# 4 Random Forest
    library(randomForest)
    library(pROC)
    library(Metrics)
    #All
    rf_data_train_balanced <- data_train_balanced
    rf_data_train_balanced$Performance_Tag <- factor(ifelse(rf_data_train_balanced$Performance_Tag==1,"Yes","No"))
    rf_model_test_actual <- factor(ifelse(data_test$Performance_Tag==1,"Yes","No"))
    
    rf_model <- randomForest(Performance_Tag ~ ., data = rf_data_train_balanced)
    rf_model_pred <- predict(object = rf_model,newdata = data_test_balanced, type = "class")
    
    # Calculate the confusion matrix for the test set
    rf_conf <- confusionMatrix(data = rf_model_pred, reference = rf_model_test_actual)
    rf_conf
    
    rf_pred_prob <- predict(object = rf_model,newdata = data_test_balanced,type = "prob")
    auc(data_test_balanced$Performance_Tag, rf_pred_prob[, "Yes"])
    ROC <- roc(data_test_balanced$Performance_Tag, rf_pred_prob[, "Yes"])
    plot(ROC, col = "green")
    
    # Selected Important Variables
    rf_model2 <- randomForest(Performance_Tag ~ ., data = rf_data_train_balanced[,selected_variables])
    rf_model_pred2 <- predict(object = rf_model,newdata = data_test_balanced, type = "class")
    # Calculate the confusion matrix for the test set
    rf_conf2 <- confusionMatrix(data = rf_model_pred2, reference = rf_model_actual)
    rf_conf2
    
    # 4.2 Tuning via GridSearch
    mtry <- seq(4, ncol(rf_data_train_balanced) * 0.8, 2)
    nodesize <- seq(3, 8, 2)
    sampsize <- nrow(rf_data_train_balanced) * c(0.7, 0.8)
    hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)
    # Create an empty vector to store OOB error values
    oob_err <- c()
    # Write a loop over the rows of hyper_grid to train the grid of models
    for (i in 1:nrow(hyper_grid)) {
      
      # Train a Random Forest model
      model <- randomForest(formula = Performance_Tag ~ ., 
                            data = rf_data_train_balanced,
                            mtry = hyper_grid$mtry[i],
                            nodesize = hyper_grid$nodesize[i],
                            sampsize = hyper_grid$sampsize[i])
      
      # Store OOB error for the model                      
      oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
    }
    
    # Identify optimal set of hyperparmeters based on OOB error
    opt_i <- which.min(oob_err)
    print(hyper_grid[opt_i,])
      
  
  
    
# 5 Support Vector Machine
    library(e1071)
    sv_data_train_balanced <- data_train_balanced[, selected_variables]
    sv_data_train_balanced$Performance_Tag <- factor(ifelse(sv_data_train_balanced$Performance_Tag==1,"Yes","No"))
    svm_model<- svm(Performance_Tag ~ .,data = sv_data_train_balanced)
    
    svm_test_pred <- predict(svm_model, data_test[,selected_variables], type = "prob")
    sv_conf <- confusionMatrix(factor(ifelse(data_test$Performance_Tag==1, "Yes", "No")), svm_test_pred)
    sv_conf
    auc(data_test$Performance_Tag,as.numeric(ifelse(svm_test_pred=="Yes", 1, 0)))
    ROC <- roc(data_test$Performance_Tag, as.numeric(ifelse(svm_test_pred=="Yes", 1, 0)))
    plot(ROC, col = "blue")
    