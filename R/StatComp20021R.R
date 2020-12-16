#' @title Use four inputs to calculate the empirical log likelihood of the mean of the original two-dimensional sample using R.
#' @description The theorem could be found in the book Empirical Likelihood by Art B.Owen.
#' @import MASS
#' @importFrom Rcpp evalCpp
#' @importFrom stats rchisq qchisq cov qqline qqplot var
#' @useDynLib StatComp20021
#' @param x the bootstrap sample in the first dimension
#' @param y the bootstrap sample in the second dimension
#' @param xbar the mean of the original sample in the first dimension
#' @param ybar the mean of the original sample in the second dimension
#' @return the empirical log likelihood of the mean in the original sample
#' @export
f <- function(x,y,xbar,ybar) {
  m1<-matrix(c(var(x),cov(x,y),cov(x,y),var(y)),2,2)
  m2<-matrix(c(mean(x)-xbar,mean(y)-xbar),2,1)
  m<-t(m2)%*%ginv(m1)%*%m2
  return(m)
}

#' @title A qqplot comparing the bootstrap values of the empirical log likelihood of the mean in the original two-dimensional sample to the chi-square distribution with 2 degree using R.
#' @description The theorem could be found in the book Empirical Likelihood by Art B.Owen.
#' @param data the original sample in n*2 matrix 
#' @param B the number of bootstrap times
#' @return a qqplot
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' dpplot(data,1000)
#' }
#' @export
dqqplot<-function(data,B){
  n<-nrow(data)
  m<-as.matrix(data)
  xbar<-mean(m[,1]);ybar<-mean(m[,2])
  y<- numeric(B)
  for (b in 1:B){
    i <- sample(1:n, size = n, replace = TRUE)
    xb<-m[i,1];yb<-m[i,2]
    y[b]<-n*f(xb,yb,xbar,ybar)
  }
  x<-rchisq(B,2)
  qqplot(x,y)
  qqline(x, distribution = function(p) qchisq(p, df = 2), col = 2)
}