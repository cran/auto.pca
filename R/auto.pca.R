#' @title auto.pca
#'
#' @description PCA done by eigenvalue decomposition of a data correlation matrix, usually determines the number of factors by eigenvalue greater than 1 and automatically it gives the uncorrelated variables
#'
#' @param input_data
#'
#' @return Uncorrelated_Variables
#'
#' @examples auto.pca(attitude)
#'
#' @export

auto.pca<-function(input_data){

  input_data<-Filter(is.numeric,input_data) # Takes only numerical values

  eigen_Value <- eigen(cor(input_data)) # Calculating Eigen Value

  eigen_Value<-round(eigen_Value$values,digits=2)

  nfactors<-length(eigen_Value[eigen_Value>1]) # Count of Eigen Value greater than 1 determines the number of factors

  print(paste("Number of Factors identified based on eigen Value greater than 1 :",nfactors,"Factors"))

  pca<-principal(input_data,nfactors=nfactors,rotate="varimax",scores=T)

  print(pca)

  pca_loadings<-pca$loadings

  pca_loadings<-data.frame(adply(pca_loadings, c(1,2)))

  pca_loadings_matrix=data.frame(reshape(pca_loadings, timevar = "X2", idvar = "X1", direction = "wide",sep=""))

  names(pca_loadings_matrix)<-c("Variables",paste("RC",1:(length(names(pca_loadings_matrix))-1),sep="_"))

  pca_loadings=data.frame(reshape(pca_loadings, timevar = "X2", idvar = "X1", direction = "wide",sep=""))

  pca_loadings[,-1] <- as.data.frame(lapply(pca_loadings[,-1], function(x){x<-abs(x);replace(x, abs(x)<0.3,0) }))

  pca_loadings[,-1]<-pca_loadings[,-1][do.call(order, as.list(pca_loadings[,-1])),]

  Rowlist<-apply(pca_loadings[,-1], 2, function(x) pca_loadings[,1][which.max(x)])

  Select_PC<-as.character(pca_loadings$X1[pca_loadings$X1 %in% Rowlist])

  cat('\n','################################################################')

  cat('\n','#############  Uncorrelated Variables Identified  ##############')

  cat('\n','################################################################')

  cat('\n','                                                                ','\n')

  return(Select_PC)
}

