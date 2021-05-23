#' Kim et al's modified t-test
#' @description
#' Performs a two sample t-test based on Kim et al's modified t statistic.
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @return
#' \item{t3}{The value of the modified t statistic.}
#' \item{p.value}{The p-value for the test.}
#' @examples
#' x <- c(120,212,NA,300,323,NA,NA,96,178,164)
#' y <- c(NA,153,304,177,NA,90,89,200,110,40)
#' kim.t(x,y,alternative = "two.sided") # t3 = 1.141308 p.value = 0.2537419
#' kim.t(x,y,alternative = "greater") # t3 = 1.141308 p.value = 0.1268709
#' kim.t(x,y,alternative = "less") # t3 = 1.141308 p.value = 0.8731291
#' @export
kim.t <- function(x,y,alternative = 'two.sided'){
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
    nh <- 2/((1/n2)+(1/n3))
    D <- x.pair-y.pair
    D.bar <- mean(D,na.rm=T)
    T.bar <- mean(x.unpair,na.rm=T)
    N.bar <- mean(y.unpair,na.rm=T)
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
      # finding the t statistic and p.value
      var.D <- var(D,na.rm = T)
      var.T <- var(x.unpair,na.rm=T)
      var.N <- var(y.unpair,na.rm=T)
      t3 <- (n1*D.bar+nh*(T.bar-N.bar))/sqrt(n1*var.D+(nh^2)*((var.N/n3)+(var.T/n2)))
      if(alternative=='two.sided'){
        p <- 2*pnorm(abs(t3),lower.tail = F)
      }
      else if(alternative == 'greater'){
        p <- pnorm(t3,lower.tail = F)
      }
      else if(alternative == 'less'){
        p <- pnorm(t3)
      }
      else{
        return('Error: alternative should be one of "two.sided", "less", "greater"')
      }
      return(list(t3=t3,p.value=p))
    }
  }
}

