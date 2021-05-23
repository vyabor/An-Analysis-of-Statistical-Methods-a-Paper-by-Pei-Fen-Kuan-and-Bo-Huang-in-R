#' Liptak's weighted Z-test
#' @description
#' Performs a two sample weighted Z-test based on a pooled p-value.
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#' @param alternative.pool a character string specifying the alternative hypothesis for the weighted Z-test, must be one of "greater" (default), "less", or "two.sided".
#' @param alternative.t a character string specifying the alternative hypothesis in the event of a standard two sample t-test, must be one of "two.sided" (default), "greater" or "less".
#' @return
#' \item{z1i}{the value of the weighted z1i statistic.}
#' \item{z2i}{the value of the weighted z2i statistic.}
#' \item{p.value}{the p-value for the test.}
#' @examples
#' x <- c(120,212,NA,300,323,NA,NA,96,178,164)
#' y <- c(NA,153,304,177,NA,90,89,200,110,40)
#' liptak.z(x,y,alternative.pool = "greater") # z1i = 1.113677 z2i = 0.4201559 p.value = 0.124683
#' liptak.z(x,y,alternative.pool = "less") # z1i = -1.113677 z2i = -0.4201559 p.value = 0.875317
#' liptak.z(x,y,alternative.pool = "two.sided") # z1i = 0.6267314 z2i = -0.4520169 pc = 0.4010029 p.value = 0.8020059
#' @export
liptak.z <- function(x,y,alternative.pool='greater',alternative.t='two.sided'){
  # handling potential error
  if(length(x) != length(y)){
    print('Both datasets have different dimensions. Running two sample t-test:')
    t.test(x,y,alternative=alternative.t,paired=F)
  }
  else{
    # organizing data and finding lengths
    x.pair <- x[!is.na(x)==!is.na(y)]
    y.pair <- y[!is.na(x)==!is.na(y)]
    x.unpair <- x[!is.na(x) & is.na(y)]
    y.unpair <- y[is.na(x) & !is.na(y)]
    n1 <- length(x.pair)
    n2 <- length(x.unpair)
    n3 <- length(y.unpair)
    w1 <- sqrt(2*n1)
    w2 <- sqrt(n2+n3)
    # more handling of potential error
    if(n1==0 | length(y.pair)==0){
      print('Data is unpaired. Running two sample t-test')
      t.test(x,y,alternative = alternative.t)
    }
    else if(n2==0 & n3==0){
      print('All data is paired. Running paired t-test')
      t.test(x,y,alternative = alternative.t,paired = T)
    }
    else{
      # conducting the weighted Z-test
      if(alternative.pool != 'greater' & alternative.pool != 'two.sided' & alternative.pool != 'less'){
        return('Error: alternative.pool should be one of "two.sided", "less", "greater"')
      }
      else{
        p1i <- t.test(x.pair,y.pair,alternative = alternative.pool,paired = T)$p.value
        p2i <- t.test(x.unpair,y.unpair,alternative = alternative.pool,paired = F)$p.value
        z1i <- qnorm(1-p1i)
        z2i <- qnorm(1-p2i)
        pci <- 1-pnorm((w1*z1i+w2*z2i)/sqrt((w1^2)+(w2^2)))
        if(alternative.pool=='two.sided'){
          if(pci < 0.5){
            pci.star <- 2*pci
          }
          else{
            pci.star <- 2*(1-pci)
          }
          return(list('two.sided weighted Z-test', z1i = z1i, z2i = z2i,pc = pci, p.value=pci.star))
        }
        else{
          return(list(test='weighted Z-test',z1i = z1i,z2i = z2i,p.value=pci))
        }
      }
    }
  }
}
