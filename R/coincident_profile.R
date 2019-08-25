#' @title Coincident Profile
#'
#' @description Returns the coincident profile developed by Martinez et al. (2016).
#'  The ideal result is finding the maximum p-value for the lag = 0; otherwise
#'  maximum p-value for negative lags suggest leading from x to y, or maximum
#'  p-value for positive lags suggest leading from y to x.
#' @param x Univariate time series
#' @param y Univariate time series
#' @param frequ Frequency of x and y. x and y must have the same frequency
#' @param MLag Maximum lag for the coincident profile
#' @param nvar1 Name of x
#' @param nvar2 Name of y
#' @param print.graf If TRUE returns a panel 2x2 where the superior panel has both
#'  plots of x and y with their turning points (maximums black dots lines and minimums red dots lines).
#'  The inferior left panel has the matplot of x and y standardized, respectively (x-mean(x))/sd(x).
#'  The inferior right panel has the coincident profile where \emph{TP} shows the number
#'  of turning points used.
#' @param iyear The year of the first observation. A single number
#' @param lyear The year of the last observation. A single number
#' @param imonth The amount of months for the first year. A single number
#' @param lmonth The amount of months for the last year. A single number
#' @param tit1 Title for the plot x
#' @param tit2 Title for the plot y
#' @param tit3 Title for the plot x and y
#' @return The coincident profile
#' @import zoo
#' @importFrom graphics abline axis barplot legend
#'  matplot par plot text
#' @export
#' @details The main output contains two objects: the coincident
#'  profile (Profile) and both the lag which has the highest probability and
#'  the number of turning points considered to the calculus of the
#'  coincident profile (MainLag).
#' @author Wilmer O Martinez R
#' @references Martinez, W and Nieto, Fabio H and Poncela, P (2016)
#'  "Choosing a dynamic common factor as a coincident index",
#'  \emph{Statistics and Probability Letters}, (109), 89-98.
#'  \url{http://dx.doi.org/10.1016/j.spl.2015.11.008}.
#' @references Banerji, A., (1999)
#'  "The lead profile and others non-parametrics tools to evaluate survey
#'  series as leading indicators",
#'  \emph{Survey Data for Industry, Research and Economic Policy,
#'   selected papers presented at the 24th CIRET Conference, Willington, New Zealand.}
#' @examples
#' set.seed(123)
#' w <- seq(-3, 7, length.out = 100)
#' x1 <- sin(pi*w)+rnorm(100,0,0.1)
#' x2 <- sin(pi*w-0.1)+rnorm(100,0,0.1)
#' coincident_profile(x1, x2, 4, 5, "name.x", "name.y", TRUE, 1991, 2015, 4, 4)
#'
#' # In this example x leads y three periods
#' set.seed(123)
#' w <- seq(-3, 7, length.out = 100)
#' x <- sin(pi*w)+rnorm(100,0,0.1)
#' y <- sin(pi*w-1)+rnorm(100,0,0.1)
#' coincident_profile(x, y, 4, 6, "name.x", "name.y", TRUE, 1991, 2015, 4, 4)

coincident_profile <- function(x, y, frequ = 12, MLag = 6, nvar1 = "name.x", nvar2 = "name.y",
                               print.graf = FALSE, iyear = 1, lyear = numeric(), imonth = 12,
                               lmonth = numeric(), tit1 = "Title.x", tit2 = "Title.y",
                               tit3 = "Title.x.y"){

  data1_TP <- TP_BryBoschan(x, frequ = frequ)
  data2_TP <- TP_BryBoschan(y, frequ = frequ)

  comp.min <- dif_TP(data1_TP, data2_TP,1)
  comp.max <- dif_TP(data1_TP, data2_TP,2)

  difer <- c(comp.min[,3],comp.max[,3])
  if(!is.null(difer)){
    difer <- difer[abs(difer) < 10]
    #library(coin)
    #library(exactRankTests)
    if( length(difer) >= 3){
      K <- -MLag:MLag
      #if( any(difer != 0) ){
      pvalor <- NULL
      for(k in K){
        p1 <- exactRankTests::perm.test(difer,  paired=TRUE, alternative="two.side",
                        mu=k, exact=T, conf.int=FALSE, conf.level=0.95)
        pvalor <- rbind(pvalor,cbind(p1$p.value*100,k))
      }
      pvalor <- data.frame(pvalor)
      rownames(pvalor) <- paste("p(", K , ")"  , sep = "")
      colnames(pvalor) <- c("p.value","lags")

      lags1 <- data.frame(pvalor[which(pvalor$p.value == max(pvalor$p.value)), c("lags")])
      colnames(lags1) <- "lags"
      if(nrow(lags1) > 1) lags1 <- lags1[which(lags1$lags == max(lags1$lags)), c("lags")]

      period <- data.frame(cbind(lags1, nvar1, nvar2, length(difer) ))
      colnames(period) <- c("lag_MaxP-value", nvar1, nvar2, "Amount_TP_used")

      if(print.graf == TRUE){

        sequ.1 <- c(round(seq(iyear, lyear, by=1),0))
        freq <- c(imonth, frequ, lmonth)

        sequ <- NULL
        for(i.1 in 1:length(sequ.1)){
          #i.1 =1
          if(i.1 == 1){
            sequ.2 <- c(rep(sequ.1[i.1],freq[1]))
          }else if(i.1 < length(sequ.1)){
            sequ.2 <- c(rep(sequ.1[i.1],freq[2]))
          }else if(i.1 == length(sequ.1)){
            sequ.2 <- c(rep(sequ.1[i.1],freq[3]))
          }
          sequ <- c(sequ, sequ.2)
        }

        tit <- paste("Coincident profile",sep="")

        #windows()
        #quartz(width=10,height=6)

        oldpar <- par(las =1,mfrow=c(2,2))
        on.exit(par(oldpar))

        plot(x, type="l",main=tit1, ylab="", xlab="Months", axes=FALSE)
        axis(2)
        axis(1, 1:length(x),sequ)
        maximos <- data1_TP[which(data1_TP$Max_Min == 2 ), "Time"]
        minimos <- data1_TP[which(data1_TP$Max_Min == 1 ), "Time"]

        abline(v = t(maximos),lty=3)
        abline(v = t(minimos),lty=3,col="red")

        plot(y ,type="l",main=tit2, ylab="", xlab="Months", axes = FALSE)
        axis(2)
        axis(1, 1:length(y),sequ)
        maximos <- data2_TP[which(data2_TP$Max_Min == 2 ), "Time"]
        minimos <- data2_TP[which(data2_TP$Max_Min == 1 ), "Time"]
        abline(v = t(maximos),lty=3)
        abline(v = t(minimos),lty=3,col="red")

        y1 <- cbind((x-mean(x))/sd(x), (y-mean(y))/sd(y))
        matplot(x=(1:length(x)), y1, main=tit3,
                xlab="Months", ylab= "", type ="l", col=c(1,2),lwd = 1.5,
                lend=2, lty=c(1,2),axes =FALSE)
        legend("topleft", c(nvar1, nvar2), pch = 20, col=c(1,2), cex=0.8)
        axis(2,-4:4,-4:4)
        axis(1, 1:length(x),sequ)

        Ft1 <- pvalor$p.value
        barplot(Ft1, xlab ="Lag", main=tit, ylab="p-value", ylim = c(0, 100), names.arg=c(K))
        abline(h=10, col = "red",lwd = 2)
        text(2,90,paste("Tp = ",length(difer),sep=""))
      }
    }else warning("The amount of comparisons is less than 4")
  }else stop("There are not enough turning points to compare")
  return(list("Profile" = pvalor, "MainLag" = period))
} #End function

