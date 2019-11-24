# machine learning bogen:

# PCA:
set.seed(11)
data_matrix <- matrix(rnorm(20), 4, 5)
mat <- sweep(data_matrix, 2, colMeans(data_matrix), "-")
# Sweep bruges her til at standardiserer data_martix

# PCA:
PCA <- prcomp(mat, scale. = F, center = F)
PCA_loadings <- PCA$rotation
PCA_scores <- PCA$x
eigenvalues <- (PCA$sdev)^2

# SVD:
SVD <- svd(mat)
svd_u <- SVD$u
svd_v <- SVD$v
svd_sigma <- diag(SVD$d)

all(round(PCA_loadings, 5) == round(svd_v, 5))
all(round(PCA_scores, 5) == round(svd_u %*% svd_sigma, 5))


variance <- eigenvalues * 100/sum(eigenvalues) 
cumvar <- cumsum(variance)
