data(wine) 
library(dplyr)

colnames(wine)[2:14] <- c('alcohol', 'malic_acid', 'ash', 'alcalinity', 'magnesium', 'phenosis', 'flavanoids', 'phenols', 'proanthocyanins', 'color', 'hue', 'diluted', 'proline')

write.table(wine, '~/Desktop/wine.txt', row.names = F, quote = F)

wine <- wine %>% as.tbl

library(corrplot)
wine_reg <- wine %>% select(-class)

# exercise visualize data
corrplot(wine_reg %>% cor, method = 'square')
corrplot(wine_reg %>% cor, method = 'number')

reg_mod <- lm(alcohol ~ ., data = wine_reg)
summary(reg_mod)

tidy(reg_mod) %>% mutate(factor = std.error / estimate)

library(broom)
X <- wine_reg %>% select(-alcohol) %>% as.matrix()

vif <- function(X){
  if(!is.matrix(X)) stop('x should be a matrix')
  vif_ret <- numeric(ncol(X))
  names(vif_ret) <- colnames(X)
  for(i in 1:ncol(X)){
    y <- X[ ,i]
    X_iter <- X[ ,-i]
    model <- lm.fit(X_iter, y)
    r_sq <- cor(y, model$fitted.values)^2
    vif_ret[i] <- 1 / (1 - r_sq)
  }
  return(vif_ret)
}

vif(X)
solve(t(X) %*% X)
vif(reg_mod)


library(MASS)
create_cor_matrix <- function(nr_x = 10,
                              cor_start = .1,
                              cor_end   = .5) {
  correlations <- runif( (nr_x^2 - nr_x) / 2 , cor_start, cor_end)
  Sigma <- diag(1, nr_x)
  Sigma[upper.tri(Sigma)] <- correlations
  Sigma[lower.tri(Sigma)] <- t(Sigma)[lower.tri(Sigma)]
  Sigma[lower.tri(Sigma)] <- correlations
  Sigma
}
                              
sampled <- mvrnorm(1000, rep(0, 10), create_cor_matrix(10, .4, .7))
sampled <- cbind(sampled, sampled[, 10] + rnorm(1000, 0, .3), 
                 sampled[, 10] + rnorm(1000, 0, .3), sampled[, 10] + rnorm(1000, 0, .3), sampled[, 10] + rnorm(1000, 0, .3))
sampled <- cbind(sampled, sampled[, 10] + 4)
sampled2 <- cbind(sampled  + rnorm(1000, 0, .3), 
                  sampled[, 10] + rnorm(1000, 0, .3), sampled[, 10] + rnorm(1000, 0, .3))
lm(V1 ~ ., data = sampled %>% as_data_frame)
lm(V1 ~ ., data = sampled2 %>% as_data_frame)

library(glmnet) 
y <- sampled[, 1] %>% unlist
X <- sampled[,-1] %>% as.matrix
mod1 <- cv.glmnet(X, y)
glmnet(X, y, lambda = mod1$lambda.min)$beta

y2 <- sampled2[, 1] %>% unlist
X2 <- sampled2[,-1] %>% as.matrix
mod2 <- cv.glmnet(X2, y2)
glmnet(X2, y2, lambda = mod2$lambda.min)$beta

wine <- wine %>% mutate(class = as.numeric(class == 1))
wine_sc <- scale(wine) %>% as_data_frame
wine_sc <- wine_sc %>% mutate(class = as.numeric(class > 1))
glm(class ~ ., data = wine_sc, family = 'binomial')
y <- wine_sc %>% select(class) %>% unlist
X <- wine_sc %>% select(-class) %>% as.matrix
mod <- cv.glmnet(X, y, family = 'binomial')
glmnet(X, y, family = 'binomial', lambda = mod$lambda.1se)$beta


