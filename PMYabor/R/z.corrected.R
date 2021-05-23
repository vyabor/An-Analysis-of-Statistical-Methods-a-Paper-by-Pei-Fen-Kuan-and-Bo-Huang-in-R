#' Looney and Jones's corrected z-test
#' @description
#' Performs a two sample z-test based on a modified variance estimation of the standard z-test by accounting for the correlation among the n1 matched pairs.
#' z.corrected(x,y,alternative = c('two.sided','greater','less'))
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @return
#' \item{z.stat}{The value of the corrected z statistic.}
#' \item{p.value}{The p-value for the test.}
#' @examples
#' x <- c(120,212,NA,300,323,NA,NA,96,178,164)
#' y <- c(NA,153,304,177,NA,90,89,200,110,40)
#' z.corrected(x,y,alternative = "two.sided") # z.stat = 1.252635 p.value = 0.2103387
#' z.corrected(x,y,alternative = "greater") # z.stat = 1.252635 p.value = 0.1051694
#' z.corrected(x,y,alternative = "less") # z.stat = 1.252635 p.value = 0.8948306
#' @export
z.corrected <- function(x,y,alternative='two.sided'){
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
    T.star <- c(x.pair,x.unpair)
    T.star.bar <- mean(T.star,na.rm=T)
    N.star <- c(y.pair,y.unpair)
    N.star.bar <- mean(N.star,na.rm=T)
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
      var.T <- var(T.star,na.rm=T)
      var.N <- var(N.star,na.rm=T)
      s.TN1 <- cov(x.pair,y.pair)
      z <- (T.star.bar - N.star.bar)/(sqrt((var.T/(n1+n2))+(var.N/(n1+n3))-(2*n1*s.TN1)/((n1+n2)*(n1+n3))))
      if(alternative=='two.sided'){
        p <- 2*pnorm(abs(z),lower.tail = F)
      }
      else if(alternative=='greater'){
        p <- pnorm(z,lower.tail = F)
      }
      else if(alternative=='less'){
        p <- pnorm(z)
      }
      else{
        return('Error: alternative should be one of "two.sided", "less", "greater"')
      }
      return(list(z.stat=z,p.value=p))
    }
  }
}
