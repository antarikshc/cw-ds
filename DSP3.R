data("iris")
data_iris <- iris[1:4]
Cov_data <- cov(data_iris)
Eigen_data <- eigen(Cov_data)
Eigen_data$values

PCA_data <- princomp(data_iris ,cor="False")
PCA_data$dev^2

PCA_data$loadings[,1:4]

Eigen_data$vectors

summary(PCA_data)

biplot(PCA_data)

screeplot(PCA_data, type ="lines")