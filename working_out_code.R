library(boot)
library(ggplot2)
library(caret)

library(pdp)
library(rpart.plot)

# Followed until 1977.
# So censoring that I will ignore.
data(melanoma, package = "boot")

head(melanoma)
dim(melanoma)

# Remove year variable.
melanoma <- melanoma[, -5]

featurePlot(melanoma[, -1], melanoma$time)








tr1 <- trainControl(
        method = 'LGOCV',
        p = 0.75,
        savePredictions = TRUE)
set.seed(1312)
m1 <- train(time ~ ., 
            data = melanoma,
            method = 'rpart2',
            tuneLength = 3,
            metric = 'MAE',
            trControl = tr1)
m1

pdf('rpart_tree.pdf')
rpart.plot(m1$finalModel)
dev.off()

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
            
plotCV(pl1, smooth = FALSE) + scale_y_log10() + scale_x_log10()
plotCV(pl2, smooth = FALSE) + scale_y_log10() + scale_x_log10()











# Full workflow.


tr2 <- trainControl(
        method = 'repeatedcv',
        number = 10,
        repeats = 5, 
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
ggsave('rpart2_scatter.pdf')

pdf('rpart_perf.pdf')
plot(m1)
dev.off()













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












set.seed(1312)
m4 <- train(time ~ ., 
            data = melanoma,
            method = 'rpart2',
            tuneGrid = data.frame(maxdepth = 30),
            trControl = tr)
m4        


pdf('rpart_1.pdf')
pdp::partial(m4, 
        pred.var = c('thickness'),
        plot = TRUE)
dev.off()

set.seed(1312)
m4b <- train(time ~ ., 
            data = melanoma,
            method = 'rpart2',
            tuneGrid = data.frame(maxdepth = 4),
            trControl = tr)
m4b        

pdf('rpart_2.pdf')

pdp::partial(m4b, 
        pred.var = c('thickness'),
        plot = TRUE)
dev.off()



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

# Are predictions the same between the two
table(best_tune_preds(c2)$pred == best_tune_preds(c1)$pred)

confusionMatrix(c2)
