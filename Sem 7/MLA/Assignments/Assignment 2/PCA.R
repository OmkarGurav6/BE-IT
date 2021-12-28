    
    # Iris dataset
    iris_data <- iris
    
    summary(iris_data)
    iris_data <- iris_data[,1:4]
    
    covmatrix <- cov(iris_data)
    covmatrix
    
    eigenvector <- eigen(covmatrix)
    eigenvector
    
    iris_PCA_using_prin <- princomp(iris_data)
    summary(iris_PCA_using_prin)
    
    iris_PCA_using_pr <- prcomp(iris_data)
    summary(iris_PCA_using_pr)
    
    # Comparing variance values
    eigenvector$values
    iris_PCA_using_prin$sdev^2
    iris_PCA_using_pr$sdev^2
    
    plot(iris_PCA_using_prin)
    screeplot(iris_PCA_using_prin, type = "lines")
    biplot(iris_PCA_using_prin)
    
    plot(iris_PCA_using_pr)
    screeplot(iris_PCA_using_pr, type = "lines")
    biplot(iris_PCA_using_pr)
    
    
    # gsp dataset
    
    gsp_data <- read.csv('C:/Users/DELL/Downloads/pca_gsp.csv')
    
    summary(gsp_data)
    gsp_data <- gsp_data[,2:14]
    
    covmatrix1 <- cov(gsp_data)
    covmatrix1
    
    eigenvector1 <- eigen(covmatrix1)
    eigenvector1
    
    gsp_PCA_using_prin <- princomp(gsp_data)
    summary(gsp_PCA_using_prin)
    
    gsp_PCA_using_pr <- prcomp(gsp_data)
    summary(gsp_PCA_using_pr)
    
    # Comparing variance values
    eigenvector1$values
    gsp_PCA_using_prin$sdev^2
    gsp_PCA_using_pr$sdev^2
    
    plot(gsp_PCA_using_prin)
    screeplot(gsp_PCA_using_prin, type = "lines")
    biplot(gsp_PCA_using_prin)
    
    plot(gsp_PCA_using_pr)
    screeplot(gsp_PCA_using_pr, type = "lines")
    biplot(gsp_PCA_using_pr)
