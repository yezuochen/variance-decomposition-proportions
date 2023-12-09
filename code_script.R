#### Applied Regression Analysis HW2 ####

# **Content**:
# * Reproduce TABLE 9.7 Variance Decomposition Proportions for the Webster, Gunst, and Mason [1974] Data
# * PROBLEMS: 9.24, 9.25, 9.26, 9.27

# Packages used
library(car)
library(magrittr)
library(mctest)
library(matlib)
library(knitr)

# Improt table from book ####
# Data is TABLE 9.4 Unstandardized Regressor and Response Variables from Webster, Gunst, and Mason [ 1974 ]
data <- read.table("tb9.4 Webster, Gunst, and Mason.txt", 
                   header = TRUE, 
                   row.names = "Observation")

read.table(header = TRUE, 
           row.names = "Observation",
           text = "
Observation y x1 x2 x3 x4 x5 x6
1 10.006 8.000 1.000 1.000 1.000 0.541 -0.099
2 9.737 8.000 1.000 1.000 0.000 0.130 0.070
3 15.087 8.000 1.000 1.000 0.000 2.116 0.115
4 8.422 0.000 0.000 9.000 1.000 -2.397 0.252
5 8.625 0.000 0.000 9.000 1.000 -0.046 0.017
6 16.289 0.000 0.000 9.000 1.000 0.365 1.504
7 5.958 2.000 7.000 0.000 1.000 1.996 -0.865
8 9.313 2.000 7.000 0.000 1.000 0.228 -0.055
9 12.960 2.000 7.000 0.000 1.000 1.380 0.502
10 5.541 0.000 0.000 0.000 10.000 -0.798 -0.399
11 8.756 0.000 0.000 0.000 10.000 0.257 0.101
12 10.937 0.000 0.000 0.000 10.000 0.440 0.432")

# Find VIF of Xs ####
# TABLE 9.5 VIF s for Acetylene Data and Webster, Gunst, and Mason Data
lm(y ~ ., data = data) %>% 
  vif()

vif <- seq(1,6)
names(vif) <- colnames(X)

for (i in seq(1,6)){
  r2 <- lm(as.formula(paste(colnames(X)[i], "~ .")), data = data.frame(X)) %>% 
    summary %>% 
    .[["r.squared"]] %>% 
    round(4)
  vif[i] <- round(1 / (1 - r2), 2)
}
print(vif) 

# Find eigenvectors ####
# TABLE 9.6 Eigenvectors for the Webster, Gunst, and Mason Data
# In mctest::eigprop, there is method to calculate eigen values.
# xz <- apply(x, 2, function(x) {
#   x/sqrt(sum(x^2))
# })
# corxz <- t(xz) %*% xz
# ev <- eigen(corxz)$values

X <- as.matrix(data[-1])
X_cen <- scale(X, scale = FALSE)
X_corxz <- scale(X, scale = FALSE) %>% 
  apply(2, function(x) x/sqrt(sum(x^2)))

eigen(t(X_corxz) %*% X_corxz)


# Variance Decomposition Proportions without mctest ####
# TABLE 9.7 Variance Decomposition Proportions for the Webster, Gunst, and Mason[1974] Data
# A. Regressors Centered
eigval_A <- eigen(t(X_corxz) %*% X_corxz)$values %>% round(5)
svd(X_corxz)

D_A <- svd(X_corxz)$d
eta_A <- max(D_A)/D_A %>% round(5)

vif_A <- data.frame(y = data$y, X_corxz) %>% 
  lm(y ~ ., data = .) %>% 
  vif
t_A <- svd(X_corxz)$v

vdp_A <- t(t_A^2 %*% diag(1/D_A)^2) %*% diag(1/vif_A) %>% round(4)
dimnames(vdp_A) <- list(NULL,colnames(X))

tb_A <- data.frame(Number = 1:6,
                   Eigenvalue = eigval_A,
                   `Condition Indices` = eta_A,
                   vdp_A)

# B. Regressors Not Centered
X_corxz0 <- cbind(x0=1, X) %>% 
  apply(2, function(x) x/sqrt(sum(x^2)))
eigval_B <- eigen(t(X_corxz0) %*% X_corxz0)$values %>% round(5)
svd(X_corxz0)

D_B <- svd(X_corxz0)$d
eta_B <- max(D_B)/D_B %>% round(5)

t_B <- svd(X_corxz0)$v

vif_B <- data.frame(y = data$y, cbind(x0=1, X)) %>% 
  lm(y ~ 0 + ., data = .) %>% 
  vif
t <- svd(X_corxz)$v
vdp_B <- t(t_B^2 %*% diag(1/D_B)^2) %*% diag(1/vif_B) %>% round(4)
dimnames(vdp_B) <- list(NULL, colnames(X_corxz0))

tb_B <- data.frame(Number = 1:7,
                   Eigenvalue = eigval_B,
                   `Condition Indices` = eta_B,
                   vdp_B)

# Variance Decomposition Proportions with mctest ####
# TABLE 9.7 Variance Decomposition Proportions for the Webster, Gunst, and Mason[1974] Data
md_A <- lm(data$y ~ 0 + X_cen)
md_B <- lm(data$y ~ X)

# A. Regressors Centered
eigprop(md_A)
# B. Regressors Not Centered
eigprop(md_B)

mc.plot(md_B)

#############################################################


