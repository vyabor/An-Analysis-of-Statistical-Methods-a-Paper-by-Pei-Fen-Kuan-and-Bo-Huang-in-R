#' Lin and Stivers's MLE based test under heteroscedasticity
#' @description
#' Performs a two sample test based on a Student's t Distribution and an MLE test statistic under heteroscedasticity.
#' MLE.het(x,y,alternative = c('two.sided','greater','less'))
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @return
#' \item{z.LS}{The value of the MLE based test statistic under heteroscedasticity.}
#' \item{df}{degrees of freedom for the test.}
#' \item{p.value}{The p-value for the test.}
#' @examples
#' # Generating heteroscedastic data:
#' set.seed(123)
#' n <- 15
#' x1 <- 1:n
#' sd1 <- runif(n,0,5)
#' het1 <- rnorm(n,0,sd1*x1)
#' y1 <- x1+het1
#' y1[c(1,2,3,6)] <- NA
#'
#' set.seed(321)
#' x2 <- 1:n
#' sd2 <- runif(n,0,5)
#' het2 <- rnorm(n,0,sd2*x2)
#' y2 <- x2+het2
#' y2[c(10,13,14,15)] <- NA
#'
#' MLE.het(y1,y2,alternative = "two.sided") # Z.LS = 0.7982896 p.value = 0.4509315
#' MLE.het(y1,y2,alternative = "greater") # Z.LS = 0.7982896 p.value = 0.2254657
#' MLE.het(y1,y2,alternative = "less") # Z.LS = 0.7982896 p.value = 0.7745343
#' @export
MLE.het <- function(x,y,alternative='two.sided'){
  # handling potential error
  if(length(x) != length(y)){
    print('Both datasets have different dimensions. Running two sample t-test:')
    t.test(x,y,alternative=alternative,paired=F)
  }
  else{
    # organizing data and finding necessary values
    x.pair <- x[!is.na(x)==!is.na(y)]
    y.pair <- y[!is.na(x)==!is.na(y)]
    x.unpair <- x[!is.na(x) & is.na(y)]
    y.unpair <- y[is.na(x) & !is.na(y)]
    n1 <- length(x.pair)
    n2 <- length(x.unpair)
    n3 <- length(y.unpair)
    # more potential error
    if(n1==0 | length(y.pair)==0){
      print('Data is unpaired. Running two sample t-test')
      t.test(x,y,alternative = alternative)
    }
    else if(n2==0 & n3==0){
      print('All data is paired. Running paired t-test')
      t.test(x,y,alternative = alternative,paired = T)
    }
    else{
      # finding statistic and p.value
      T1.bar <- mean(x.pair)
      N1.bar <- mean(y.pair)
      T.bar <- mean(x.unpair,na.rm=T)
      N.bar <- mean(y.unpair,na.rm=T)
      s.TN1 <- cov(x.pair,y.pair)
      sdT1 <- sd(x.pair)
      sdN1 <- sd(y.pair)

      r <- s.TN1/(sdT1*sdN1)
      f <- n1*(n1+n3+n2*s.TN1/sdT1^2)/((n1+n2)*(n1+n3)-n2*n3*r^2)
      g <- n1*(n1+n2+n3*s.TN1/sdN1^2)/((n1+n2)*(n1+n3)-n2*n3*r^2)
      V1 <- ((f^2/n1+(1-f)^2/n2)*sdT1^2*(n1-1)+(g^2/n1+(1-g)^2/n3)*sdN1^2*(n1-1)-2*f*g*s.TN1*(n1-1)/n1)/(n1-1)
      Z.LS <- (f*(T1.bar-T.bar)-g*(N1.bar-N.bar)+T.bar-N.bar)/sqrt(V1)

      if(alternative=='two.sided'){
        p <- 2*pt(abs(Z.LS),df=n1,lower.tail = F)
      }
      else if(alternative=='greater'){
        p <- pt(Z.LS,df=n1,lower.tail = F)
      }
      else if(alternative=='less'){
        p <- pt(Z.LS,df=n1)
      }
      else{
        return('Error: alternative should be one of "two.sided", "less", "greater"')
      }
      return(list(Z.LS=Z.LS,df=n1,p.value=p))
    }
  }
}
