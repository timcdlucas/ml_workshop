# Slide 4

library(boot)
library(ggplot2)
library(pdp)
library(rpart.plot)


# Followed until 1977.
# So censoring that I will ignore.
data(melanoma, package = "boot")

head(melanoma)
dim(melanoma)

# Remove year variable.
# Due to the censoring this variable
# is very highly correlated to death time.
melanoma <- melanoma[, -5]

# Quick look at the data
featurePlot(melanoma[, -1], melanoma$time)
hist(melanoma$time)


# Slide 5.
library(caret)

m0 <- train(time ~ ., 
            data = melanoma,
            method = 'rpart2')

# It is quite a complicated object.
names(m0)

# Quick summary.
print(m0)



# Slide 7
rpart.plot(m0$finalModel)


# Slide 13
# Set up our out-of-sample validation.
# Leave group out cross validation.
# Hold out 75% of the data.
# Save preditions so we can plot observed vs fitted.
tr1 <- trainControl(
        method = 'LGOCV',
        p = 0.25,
        number = 1,
        savePredictions = TRUE)

# By default the random split is done in train()
# So we use the same seed for both models.
# See the index argument of trainControl() for alternative.
set.seed(1312)
m1 <- train(time ~ ., 
            data = melanoma,
            method = 'rpart2',
            trControl = tr1)
m1


set.seed(1312)
m2 <- train(time ~ ., 
            data = melanoma,
            method = 'lm',
            trControl = tr1)
m2            


            

plotCV <- function(t, print = TRUE, smooth = TRUE, alpha = 1){
  stopifnot(inherits(t, 'train'))
  
  
  d <- best_tune_preds(t)
  

  if('weights' %in% names(d)){
    p <- ggplot(d, aes(obs, pred, size = weights, colour = 'a'))
  } else { 
    p <- ggplot(d, aes(obs, pred, colour = 'a'))
  }
  p <- p + 
    geom_point(alpha = alpha) + 
    geom_abline(slope = 1, intercept = 0) +
    theme(legend.position = "none")
  
  if(smooth){
    p <- p + geom_smooth()
  }
  
  
  if(print) print(p)

  return(invisible(p))

} 

best_tune_preds <- function (t){

stopifnot(inherits(t, 'train'))

  row_matches <- sapply(1:length(t$bestTune), function(x) t$pred[, names(t$bestTune)[x]] == t$bestTune[[x]])
  best_rows <- rowMeans(row_matches) == 1

d <- t$pred[best_rows, ]

}


plotCV(m2, smooth = FALSE)
plotCV(m1, smooth = FALSE)






# Slide 16
pl <- read.csv(
  file = 'https://raw.githubusercontent.com/timcdlucas/ml_workshop/master/planets.csv')

set.seed(31281)
pl1 <- train(g ~ ., 
            data = pl,
            method = 'rf',
            trControl = tr1)

set.seed(31281)
pl2 <- train(g ~ 0 + I(m1 * m2 / d ^ 2), 
            data = pl,
            method = 'lm',
            trControl = tr1)
            
plotCV(pl1, smooth = FALSE, print = FALSE) + scale_y_log10() + scale_x_log10()
plotCV(pl2, smooth = FALSE, print = FALSE) + scale_y_log10() + scale_x_log10()




# Slide 19

tr2 <- trainControl(
        method = 'repeatedcv',
        number = 5,
        repeats = 3, 
        savePredictions = TRUE)


set.seed(1312)
m1 <- train(time ~ ., 
            data = melanoma,
            method = 'rpart2',
            tuneLength = 12,
            metric = 'MAE',
            trControl = tr2)
m1

plotCV(m1)

# Slide 22
plot(m1)















# Slide 37

tr2 <- trainControl(
        method = 'repeatedcv',
        number = 5,
        repeats = 3, 
        savePredictions = TRUE)

# Slide 37
my_metric <- 'MAE'



# Slide 40

# A good benchmark
set.seed(131210)
m1 <- train(time ~ .,
            data = melanoma,
            method = 'enet',
            tuneLength = 10,
            metric = my_metric,
            trControl = tr2)

plotCV(m1)



# Slide 41

# A good benchmark
set.seed(131210)
m2 <- train(time ~ .,
            data = melanoma,
            method = 'ppr',
            tuneLength = 10,
            metric = my_metric,
            trControl = tr2)

plotCV(m2)





# Slide 42

# A good benchmark
set.seed(131210)
m3 <- train(time ~ .,
            data = melanoma,
            method = 'ranger',
            tuneLength = 5,
            metric = my_metric,
            trControl = tr2)

plotCV(m3)




# Slide 43

# Try a few models. No free lunch.
# This one is slower. 
# xgboost has lots of parameters.
# So probably use grid search instead.
set.seed(131210)
m4 <- train(time ~ .,
            data = melanoma,
            method = 'xgbTree',
            tuneLength = 1, 
            metric = my_metric,
            trControl = tr2)

plotCV(m4)




# Slide 44

# A little bit slow.
set.seed(131210)
m5 <- train(time ~ .,
            data = melanoma,
            method = 'nnet',
            tuneLength = 10, 
            linout = TRUE,
            metric = my_metric,
            trControl = tr2)

plotCV(m5)







set.seed(1312)
m2 <- train(time ~ ., 
            data = melanoma,
            method = 'rpart2',
            tuneGrid = data.frame(maxdepth = 3),
            trControl = tr1)
m2            


plotCV(m2)


set.seed(1312)
m3 <- train(time ~ ., 
            data = melanoma,
            method = 'rpart2',
            tuneGrid = data.frame(maxdepth = 6),
            trControl = tr1)
m3


pdf('rpart_depth3.pdf')
rpart.plot(m2$finalModel)
dev.off()

pdf('rpart_depth6.pdf')
rpart.plot(m3$finalModel)
dev.off()














# Slide 34
d <- diamonds[, -1]

set.seed(1020)
c1 <- train(factor(price > 17000) ~ ., 
            data = d,
            metric = 'Accuracy',
            method = 'rpart2',
            tuneLength = 8,
            trControl = tr1)
c1$bestTune

pdf('rpart_acc.pdf')
plot(c1)
dev.off()

set.seed(1020)
c2 <- train(factor(price > 17000) ~ ., 
            data = d,
            metric = 'Kappa',
            method = 'rpart2',
            tuneLength = 8,
            trControl = tr1)
c2$bestTune

pdf('rpart_kappa.pdf')
plot(c2)
dev.off()

confusionMatrix(c2)
