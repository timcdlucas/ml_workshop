setwd('~/Dropbox/work/documents/rusers-caret-zoon/')
setwd('E://tim/Dropbox/work/documents/rusers-caret-zoon/')


library(dplyr)
library(caret)
library(ggplot2)

source('../../analysis/Other/theme_tcdl.R')

set.seed(3)
classif <- data_frame(x = runif(30),
                      y = round(x + runif(30, 0, 0.4)))


ggplot(classif, aes(x, y)) + 
  geom_jitter(width = 0, height = 0.1) +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'), se = FALSE) +
  scale_y_continuous(breaks=c(0, 1), labels=c('Not cat', 'Cat')) +
  labs(x = 'Feature / predictor variable', y = 'Class') +
  theme_pres2

ggsave('classification.pdf')

cols <- pokepal('charizard', spread = 2)
ggplot(classif, aes(x, y)) + 
  geom_jitter(width = 0, height = 0.1) +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'), se = FALSE) +
  scale_y_continuous(breaks=c(0, 1), labels=c('Sp. Absent', 'Sp. Present')) +
  labs(x = 'Feature / predictor variable', y = 'Class') +
  theme_pres2 +
  theme(axis.text.y = element_text(colour=cols))


ggsave('classification2.pdf')

set.seed(2)

regr <- data_frame(x = runif(30),
                   y = 10 * ((x - 0.5) ^ 2 + runif(30, 0, 0.4)))


ggplot(regr, aes(x, y)) + 
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = 'Feature / predictor variable', y = 'Response') +
  theme_pres2

ggsave('regression.pdf')




library(gganimate)

regr$fold <- 1:3


p <- ggplot(regr, aes(x, y)) + 
      geom_point(colour = 'grey') +
      geom_smooth(aes(group = fold, frame = fold), se = FALSE) +  
      geom_point(aes(frame = fold), color = "black", size = 1.2) +
      labs(x = 'Feature / predictor variable', y = 'Response') +
      theme_pres2

gganimate(p, "cv_regression.gif")




ggplot(regr, aes(x, y)) + 
  geom_point() +
  geom_smooth(se = FALSE, span = 0.3) +
  labs(x = 'Feature / predictor variable', y = 'Response') +
  theme_pres2
ggsave("scale-0.png")


ggplot(regr, aes(x, y)) + 
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = 'Feature / predictor variable', y = 'Response') +
  theme_pres2

ggsave("scale-1.png")


ggplot(regr, aes(x, y)) + 
  geom_point() +
  geom_smooth(se = FALSE, span = 1) +
  labs(x = 'Feature / predictor variable', y = 'Response') +
  theme_pres2


ggsave("scale-2.png")















m1 <- train(Species ~ ., 
            iris,
            method = 'gbm')





trainControl <- trainControl(iris$Species)

m1 <- train(Species ~ ., 
            iris,
            method = 'gbm')

ggplot(m1) + theme_pres2

ggsave('train_gbm.pdf')



m2 <- train(Species ~ ., 
            iris,
            method = 'nnet')
ggplot(m2) + theme_pres2

ggsave('train_nnet.pdf')





tr <- trainControl(method = 'cv', number = 5)

m3 <- train(Species ~ ., 
            iris,
            trControl = tr,
            method = 'nnet')



m4 <- train(Species ~ ., 
            iris,
            tuneLength = 10,
            method = 'nnet')
ggplot(m4) + theme_pres2
ggsave('train_nnet_tuneLength.pdf')




m5 <- train(Species ~ ., 
            iris,
            tuneGrid = expand.grid(size = c(1, 5, 10, 20), 
                                   decay = seq(0, 0.1, 0.01)),
            method = 'nnet')
ggplot(m5) + theme_pres2
ggsave('train_nnet_tuneGrid.pdf')










