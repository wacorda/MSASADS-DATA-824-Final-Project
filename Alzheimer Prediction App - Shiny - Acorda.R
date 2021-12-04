# Will Acorda
# DATA 824 Final Project
# 12.4.2021

# required libraries
library(shiny)
library(caret)
library(plotly)
library(ggplot2)
library(cowplot)
library(randomForest)
library(caret)
library(MLmetrics)
library(ggcorrplot)

####################### PLEASE CHANGE DIRECTORY ACCORDINGLY #############################################
# load final dataset w/ all variables
dat_all <- readRDS("C:\\Users\\wacor\\Documents\\MSASADS\\Fall 2021\\Final Project\\final_data.RDS")

# load final model created in a separate R script
# PLEASE CHANGE DIRECTORY ACCORDINGLY
mod_svmrad <- readRDS("C:\\Users\\wacor\\Documents\\MSASADS\\Fall 2021\\Final Project\\model_svmrad.RDS")
#########################################################################################################

# create modified data for just the model as this is what is used
# in the prediction portion of the vis
varis <- c("MMSE", "nWBV", "Age", "eTIV", "ASF", "CDR")
dat <- subset(dat_all, select = varis)

# create the UI
ui = fluidPage(
  
  titlePanel("Alzheimer's Prediction - DATA 824 Final Project"),
  
  # the sidebar inputs are for the prediction portion of the vis ONLY
  sidebarLayout(
    sidebarPanel(
      sliderInput("age",
                  "Age:",
                  min = 33,
                  max = 96,
                  value = 73),
      sliderInput("mmse",
                  "Mini-Mental State Examination Score (MMSE):",
                  min = 15,
                  max = 30,
                  value = 29),
      sliderInput("nwbv",
                  "Normalized Whole-Brain Volume (nWBV):",
                  min = 0.6440,
                  max = 0.8410,
                  value = 0.7485),
      sliderInput("etiv",
                  "Estimated Total Intracranial Volume (eTIV):",
                  min = 1123,
                  max = 1992,
                  value = 1446),
      sliderInput("asf",
                  "Atlas Scaling Factor (ASF):",
                  min = 0.881,
                  max = 1.563,
                  value = 1.214)
    ),
    
    # decided to use tabset for the layout
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Exploratory Data Analysis (EDA)",
                           fluidRow(plotOutput("plot1"),
                                    plotlyOutput("plotly1"), 
                                    plotOutput("plot2"),
                                    plotOutput("plot3"),
                                    plotOutput("plot4"),
                                    plotOutput("plot5")
                           )
                   ),
                  tabPanel("Model Selection", 
                           fluidRow(plotOutput("varimp"),
                                    textOutput("varimptxt"),
                                    tableOutput("models"),
                                    textOutput("modeltxt")
                          )
                  ),
                  tabPanel("Prediction", 
                           fluidRow(plotlyOutput("prediction"),
                                    textOutput("predtxt"),
                                    textOutput("predtxt2"))
                  )
      )
      
    )
  )
)

# Create the server
server = function(input, output) {
  obs = reactive({
    obs = dat[1, -which(colnames(dat) == 'CDR'), drop=FALSE]
    obs$Age = input$age
    obs$MMSE = input$mmse
    obs$nWBV = input$nwbv
    obs$eTIV = input$etiv
    obs$ASF = input$asf
    obs
  })
  
  # take inputs from user and predicts probability based on 
  # those values. 
  pred = reactive({
    pred = predict(mod_svmrad, obs(), type = 'prob')[,2]
  })
  
  # outputs the prediction from above into a gauge plot.
  output$prediction <- renderPlotly({
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = round(pred(), 2),
      title = list(text = "Probability of Having Dementia"),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
      bar = list(color = "#00BFC4"),
      axis =list(range = list(NULL, 1)),
      threshold = list(
      line = list(color = "black", width = 4),
      thickness = 1,
      value = round(pred(), 2))))
  })
  
  output$predtxt <- renderText("Use inputs to the left to predict the probability
                               of whether an individual has dementia.")
  
  output$predtxt2 <- renderText("NOTE: The probabilities are based on the
                               SVM - Radial model.")
  
  # outputs the EDA visualizations.
  output$plot1 <- renderPlot({
    dp1 <- dat_all %>% 
      ggplot(aes(x = Age, fill = CDR)) +
      geom_density(alpha = 0.5) +
      xlab("Age") +
      ylab("Density") +
      ggtitle("Density of Age Given Dementia (CDR)") +
      theme_minimal() 
    dp1
  })
  
  output$plotly1 <- renderPlotly({
    corr.plot <- dat_all %>%
      select(-M.F, -SES, -Educ, -CDR) %>%
      cor() %>%
      ggcorrplot(hc.order = TRUE, type = "lower",
                 colors = c("#F8766D", "#F5F5F5", "#00BFC4"),
                 title = "Correlation Heat Map")
    
    corr_int <- ggplotly(corr.plot)
  })
  
  output$plot2 <- renderPlot({
    bp1 <- dat_all %>% 
      ggplot(aes(x = MMSE, fill = CDR)) +
      geom_bar() +
      xlab("MMSE") +
      ylab("Frequency") +
      ggtitle("MMSE Given Dementia (CDR)") +
      theme_minimal() 
    
    bp2 <- dat_all %>% 
      ggplot(aes(x = Educ, fill = CDR)) +
      geom_bar(position = "dodge") +
      xlab("Educ") +
      ylab("Frequency") +
      ggtitle("Education Score Given Dementia (CDR)") +
      theme_minimal() 

    plot_grid(bp1, bp2,  ncol = 2, nrow = 1)
  })
  
  output$plot3 <- renderPlot({
    bp3 <- dat_all %>% 
      ggplot(aes(x = SES, fill = CDR)) +
      geom_bar(position = "dodge") +
      xlab("SES") +
      ylab("Frequency") +
      ggtitle("Socioeconomic Status Score Given Dementia (CDR)") +
      theme_minimal() 
    
    bp4 <- dat_all %>% 
      ggplot(aes(x = M.F, fill = CDR)) +
      geom_bar(position = "dodge") +
      xlab("Gender") +
      ylab("Frequency") +
      ggtitle("Gender Given Dementia (CDR)") +
      theme_minimal() 
    
    plot_grid(bp3, bp4, ncol = 2, nrow = 1)
  })
  
  output$plot4 <- renderPlot({
    boxp1 <- dat_all %>%
      ggplot(aes(x = CDR, y = ASF, fill = CDR)) +
      geom_boxplot() +
      xlab("CDR") +
      ylab("ASF") +
      ggtitle("ASF Given Dementia (CDR)") +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 20, face = "bold")) +
      theme_bw()
    
    boxp2 <- dat_all %>%
      ggplot(aes(x = CDR, y = MMSE, fill = CDR)) +
      geom_boxplot() +
      xlab("CDR") +
      ylab("MMSE") +
      ggtitle("MMSE Given Dementia (CDR)") +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 20, face = "bold")) +
      theme_bw() 
    
    plot_grid(boxp1, boxp2, ncol = 2, nrow = 1)
  })
  
  output$plot5 <- renderPlot({
    boxp3 <- dat_all %>%
      ggplot(aes(x = CDR, y = eTIV, fill = CDR)) +
      geom_boxplot() +
      xlab("CDR") +
      ylab("eTIV") +
      ggtitle("eTIV Given Dementia (CDR)") +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 20, face = "bold")) +
      theme_bw() 

    boxp4 <- dat_all %>%
      ggplot(aes(x = CDR, y = nWBV, fill = CDR)) +
      geom_boxplot() +
      xlab("CDR") +
      ylab("nWBV") +
      ggtitle("nWBV Given Dementia (CDR)") +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 20, face = "bold")) +
      theme_bw() 
    
    plot_grid(boxp3, boxp4, ncol = 2, nrow = 1)
    
  })
  
  # outputs the variable importance plot 
  output$varimp <- renderPlot({
    # training and validation dataset
    set.seed(1)
    train <- createDataPartition(dat_all$CDR, p = 0.8)[[1]] 
    dat_train <- dat_all[train,]
    dat_test <- dat_all[-train,]
    
    # feature selection
    rf_fit <- randomForest(CDR ~ ., data = dat_train, importance = TRUE)
    varImpPlot(rf_fit, type = 2)
  })
  
  output$varimptxt <- renderText("NOTE: Based on the MeanDecreaseGini,
                                 any feature that is less than 5 was 
                                 removed from the model. Three features
                                 were removed: Educ, SES and M.F.")
  
  output$models <- renderTable({varis <- c("MMSE", "nWBV", "Age", "eTIV", "ASF", "CDR")
  
  dat_final <- subset(dat_all, select = varis)
  dat_train <- subset(dat_all, select = varis)
  dat_test <- subset(dat_all, select = varis)
  
  # model building
  ctrl <- trainControl(method = "cv",
                       number = 5,
                       savePredictions = TRUE,
                       classProbs = TRUE,
                       summaryFunction = prSummary) 
  
  # LR
  set.seed(1)
  model_lr <- train(CDR ~ ., 
                    data = dat_train,
                    method = "glm",
                    family = "binomial",
                    trControl = ctrl,
                    metric = "F")
  model_lr
  
  # FDA
  set.seed(1)
  model_fda <- train(CDR ~ ., 
                     data = dat_train,
                     trControl = ctrl,
                     method = "fda",
                     metric = "F",
                     tuneLength = 3)
  model_fda
  
  # SVM - Radial
  set.seed(1)
  model_svm2 <- train(CDR ~ ., 
                      data = dat_train,
                      trControl = ctrl,
                      method = "svmRadial",
                      metric = "F",
                      tuneLength = 3)
  model_svm2

  # Decision Tree
  set.seed(1)
  model_dt <- train(CDR ~ ., 
                    data = dat_train,
                    trControl = ctrl,
                    method = "rpart",
                    metric = "F",
                    tuneLength = 3)
  model_dt
  
  # KNN
  set.seed(1)
  model_knn <- train(CDR ~ ., 
                     data = dat_train,
                     trControl = ctrl,
                     method = "knn",
                     metric = "F",
                     tuneLength = 3)
  model_knn
  
  # model assessment - using 5 folds
  set.seed(1)
  folds <- createFolds(dat_test$CDR, k = 5)
  lr_F1s <- matrix()
  fda_F1s <- matrix()
  svmrad_F1s <- matrix()
  dt_F1s <- matrix()
  knn_F1s <- matrix()
  
  for(i in 1:5) {
    testdat <- unlist(folds[i])
    model_lr
    lr_F1s[i] <- F1_Score(dat_test$CDR[testdat], predict(model_lr, dat_test[testdat,]))
    model_knn
    knn_F1s[i] <- F1_Score(dat_test$CDR[testdat], predict(model_knn, dat_test[testdat,]))
    model_dt
    dt_F1s[i] <- F1_Score(dat_test$CDR[testdat], predict(model_dt, dat_test[testdat,]))
    model_fda
    fda_F1s[i] <- F1_Score(dat_test$CDR[testdat], predict(model_fda, dat_test[testdat,]))
    model_svm2
    svmrad_F1s[i] <- F1_Score(dat_test$CDR[testdat], predict(model_svm2, dat_test[testdat,]))
  }
  
  # calculate the mean of the CV
  lr_mean_F1 <- mean(lr_F1s)
  dt_mean_F1 <- mean(dt_F1s)
  knn_mean_F1 <- mean(knn_F1s)
  fda_mean_F1 <- mean(fda_F1s)
  svmrad_mean_F1 <- mean(svmrad_F1s)
  
  f1_table <- data.frame(rbind(lr_mean_F1, dt_mean_F1, knn_mean_F1,
                               fda_mean_F1, svmrad_mean_F1))
  colnames(f1_table) <- c("F1 Score")
  f1_model <- data.frame(c("Logistic Regression", "Single Decision Tree",
                           "KNN", "Flexible DA", "SVM - Radial"))
  colnames(f1_model) <- c("Models")
  
  # final table after combining scores and model names
  finaltab <- cbind(f1_model, f1_table)})
  
  output$modeltxt <- renderText("NOTE: The final model selected was
                                 the SVM - Radial as it had the highest
                                 F1 Score (0.91).")
  
}

shinyApp(ui = ui, server = server)
