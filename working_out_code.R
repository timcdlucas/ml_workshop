library(boot)
library(ggplot2)
library(caret)

# Followed until 1977.
# So censoring that I will ignore.
data(melanoma, package = "boot")

head(melanoma)
dim(melanoma)


featurePlot(melanoma[, -1], melanoma$time)



tr <- trainControl(
        method = 'LGOCV',
        number = 1)
set.seed(1312)
m1 <- train(time ~ ., 
            data = melanoma,
            method = 'rf',
            trControl = tr)

set.seed(1312)
m2 <- train(time ~ ., 
            data = melanoma,
            method = 'lm',
            trControl = tr)
            
            



pl <- read.csv(
  file = 'https://raw.githubusercontent.com/timcdlucas/ml_workshop/master/planets.csv')

set.seed(31281)
pl1 <- train(g ~ ., 
            data = pl,
            method = 'rf',
            trControl = tr)

set.seed(31281)
pl2 <- train(g ~ 0 + I(m1 * m2 / d ^ 2), 
            data = pl,
            method = 'lm',
            trControl = tr)
            

