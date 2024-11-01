library(corrplot)
library(ggplot2)
library(ggcorrplot)


#Coverians + correlation (Kap 3-5)https://efif.sharepoint.com/sites/cph/Lyngby/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2Fcph%2FLyngby%2FShared%20Documents%2F4%2E%20Indhold%20%26%20Niveau%2FDAT%2FDAL%2Fmaterialer%5Fthor%2FDALE23%2Dopgaver%2Epdf&parent=%2Fsites%2Fcph%2FLyngby%2FShared%20Documents%2F4%2E%20Indhold%20%26%20Niveau%2FDAT%2FDAL%2Fmaterialer%5Fthor&p=true&ga=1
rnorm(6)
rnorm_100 <- rnorm(100)
hist(rnorm_100, breaks = 20)
col_a <- rnorm(6,5)
col_a <- round(col_a, 0)
col_b <- rnorm(6,5,0.5)
col_b <- round (col_b)
col_c <- rnorm(6,3)
col_d <- rnorm(6,1)
my_matrix <- round(data.frame(col_a,col_b,col_c, col_d))
mean(col_a)
my_mean(col_a)
my_cov(col_a,col_b)
my_corr(col_a, col_b)
cor(col_a, col_b)
corrplot::corrplot(cor(my_matrix))
ggcorrplot(my_matrix, hc.order = T, type = "lower", lab = T)

my_mean <- function(v){
  sum = 0
  for (i in 1:length(v)) {
  sum = sum + v[i]  
  }
  mm <- sum/length(v)
  return(mm)
}

my_cov <- function(v,w){
  m_v <- my_mean(v)
  m_w <- my_mean(w)
  sum = 0
  for (i in 1:length(w)) {
    t_v <- v[i]-m_v
    t_w <- w[i]-m_w
    t_p <- t_v*t_w
    sum <- sum + t_p
  }
  ret_cov <- sum / (length(w)-1)
  return(ret_cov)
}

# Correlation
my_sd <- function(v){
  sd_1 <- my_mean(v)
  sum = 0
  for (i in 1:length(v)) {
    temp <- (v[i]-sd_1)^2
    sum <- sum + temp
  }
  ret_sd <- sum/(length(v)-1)
  ret_sd <- sqrt(ret_sd_op)
  return(ret_sd)
}
my_corr <- function(v,w){
  # Get coveriance from my function.
  my_cov_t <- my_cov(v,w)
  my_sd_v <- my_sd(v)
  my_sd_w <- my_sd(w)
  ret_corr <- my_cov_t/(my_sd_v*my_sd_w)
  return(ret_corr)
}











