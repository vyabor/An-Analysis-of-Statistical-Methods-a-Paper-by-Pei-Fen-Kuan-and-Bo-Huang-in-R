#' Ekbohm's MLE based test under homoscedasticity
#' @description
#' Performs a two sample test based on a Student's t Distribution and an MLE test statistic under homoscedasticity.
#' MLE.hom(x,y,alternative = c('two.sided','greater','less'))
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @return
#' \item{Z.E}{The value of the MLE based test statistic under homoscedasticity.}
#' \item{df}{degrees of freedom for the test.}
#' \item{p.value}{The p-value for the test.}
#' @examples
#' # Generating homoscedastic data:
#' set.seed(123)
#' n <- 15
#' x1 <- 1:n
#' het1 <- rnorm(n,0,5)
#' y1 <- x1+het1
#' y1[c(1,2,3,6)] <- NA
#'
#' set.seed(321)
#' x2 <- 1:n
#' het2 <- rnorm(n,0,5)
#' y2 <- x2+het2
#' y2[c(10,13,14,15)] <- NA
#'
#' MLE.hom(y1,y2,alternative = "two.sided") # Z.E = -0.2249877 p.value = 0.8284157
#' MLE.hom(y1,y2,alternative = "greater") # Z.E = -0.2249877 p.value = 0.5857922
#' MLE.hom(y1,y2,alternative = "less") # Z.E = -0.2249877 p.value = 0.4142078
#' @export
MLE.hom <- function(x,y,alternative='two.sided'){
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
      var.T <- var(x.unpair,na.rm=T)
      var.N <- var(y.unpair,na.rm=T)

      r <- s.TN1/(sdT1*sdN1)
      sig_hat_squared <- (sdT1^2*(n1-1)+sdN1^2*(n1-1)+(1+r^2)*(var.T*(n2-1)+var.N*(n3-1)))/(2*(n1-1)+(1+r^2)*(n2+n3-2))
      f.star <- (n1*(n1+n3+n2*r))/((n1+n2)*(n1+n3)-n2*n3*r^2)
      g.star <- (n1*(n1+n2+n3*r))/((n1+n2)*(n1+n3)-n2*n3*r^2)
      V1.star <- sig_hat_squared*((2*n1*(1-r)+(n2+n3)*(1-r^2))/((n1+n2)*(n1+n3)-n2*n3*r^2))
      Z.E <- (f.star*(T1.bar-T.bar)-g.star*(N1.bar-N.bar)+T.bar-N.bar)/sqrt(V1.star)

      if(alternative == 'two.sided'){
        p <- 2*pt(abs(Z.E),df=n1,lower.tail = F)
      }
      else if(alternative == 'greater'){
        p <- pt(Z.E,df=n1,lower.tail = F)
      }
      else if(alternative == 'less'){
        p <- pt(Z.E,df=n1)
      }
      else{
        return('Error: alternative should be one of "two.sided", "less", "greater"')
      }
      return(list(Z.E=Z.E,p.value=p))
    }
  }
}

