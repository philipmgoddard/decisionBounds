plotDecision <- function(pred1, pred2, trainModel, dP1 = 1, dP2 = 1, inputData) {
  min1 <- min(inputData[, pred1])
  max1 <- max(inputData[, pred1])
  min2 <- min(inputData[, pred2])
  max2 <- max(inputData[, pred2])

  otherData <- inputData[, !names(inputData) %in% c(pred1, pred2)]

  pred1vals <- seq(from = min1, to = max1, by = dP1)
  pred2vals <- seq(from = min2, to = max2, by = dP2)
  reps <- length(pred1vals) * length(pred2vals)

  # calculate the mean value of the other predictive factors
  # these will be held constant as we vary over the two inputs
  # of interest
  meanData <- data.frame(vapply(otherData,
                                function(x) rep(mean(x), reps),
                                numeric(reps)))
  nVar <- ncol(meanData)
  grid_vals <- expand.grid(pred1vals, pred2vals)
  plotData <- as.data.frame(matrix(NA, nrow = reps, ncol = (nVar + 2)))
  plotData[, 1:2] <- grid_vals
  plotData[, 3:(nVar+2)] <- meanData
  
  names(plotData)[1] <- pred1
  names(plotData)[2] <- pred2
  names(plotData)[3:(nVar+2)] <- names(meanData)
  
  # I have only tested for object of class train
  # produced from caret::train()
  predictions <- predict(trainModel, newdata = plotData)
  plotData <- cbind(plotData, predictions)

  return(plotData[, c(1:2, ncol(plotData))])
}
  
library(caret)
library(ggplot2)
library(FUNctions)
model1 <- train(Species~ ., data = iris, method = "lda" )
model2 <- train(Species~ ., data = iris, method = "rf" )
model3 <-train(Species~ ., data = iris, method = "nnet" )
model4 <-train(Species~ ., data = iris, method = "fda" )

p <- plotDecision("Petal.Length",
             "Petal.Width",
             model2,
             dP1 = 0.01,
             dP2 = 0.01,
             inputData = iris[, 1:4])

# All the other predictors are constant at their mean value, 
# the grid runs over the specified two predictors

# lets do some clever ggplotting to visualse the prediction regions
p1 <- ggplot(p, aes(x = Petal.Length, y = Petal.Width)) +
  geom_raster(aes(x = Petal.Length,
                  y = Petal.Width,
                  fill = factor(predictions)),
              alpha = 0.2,
              interpolate = TRUE) +
  scale_fill_manual(values = philTheme(), name = "Prediction Region") + 
  geom_point(data = iris, aes(x = Petal.Length,
              y = Petal.Width,
              shape  = Species),
              alpha = 0.6,
              size = 3) + 
  theme_bw()
