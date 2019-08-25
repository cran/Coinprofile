#' @title Closest differences
#'
#' @description Finds closest differences between two sets of turning points
#' @param dato1 A vector of data
#' @param dato2 A vector of data2
#' @param minmax Indicator variable 1 minimum, 2 maximum
#' @return Differences between turning points
#' @import zoo
#' @export
#' @details From Martinez et al. (2016): "When the number of turning points in the two
#'  time series that are being compared is not the same,
#'  we recommend choosing the pairs of turning points, i.e.
#'  peak–peak and trough–trough, in the following way:
#'  take the first peak of the first time series, then
#'  compute the chronological difference with each one
#'  of the peaks of the second time series and choose
#'  the peak in this time series that leaves the smallest
#'  algebraic difference. Now, consider the second peak
#'  of the first time series and compute the differences
#'  with all the peaks of the second one but the peaks
#'  that precedes the one chosen in the previous step.
#'  Take from the second time series that for which the
#'  difference is the smallest. This procedure for pairing
#'  peaks is repeated until either all the peaks in the first
#'  time series have been considered or all the peaks in the
#'  second series have been exhausted."
#'
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' dif_TP(TP_BryBoschan(x),TP_BryBoschan(y))
#'
dif_TP <- function(dato1, dato2, minmax = 1){

  dato1 <- data.frame(dato1)
  dato2 <- data.frame(dato2)
  if(nrow(dato1) > 0 & nrow(dato2) > 0){

    x <- data.frame(dato1[which(dato1$Max_Min == minmax), "Time"])
    y <- data.frame(dato2[which(dato2$Max_Min == minmax), "Time"])

    pegas <- NULL
    if(nrow(x) > 0 & nrow(y) > 0){
      for(i in 1:nrow(y)){
        #i=1
        pega <- cbind(x, c(rep(y[i,],nrow(x))) )
        pegas <- rbind(pegas, pega)
      }

      colnames(pegas) <- c("time","time2")
      pegas$difer <- pegas$time - pegas$time2
      pegas$difer2 <- abs(pegas$time - pegas$time2)

      dejas <- pegas

      dejas2 <- NULL
      for(i in unique(dejas$time) ){
        deja <- dejas[which(dejas$time == i),]
        deja1 <- deja[which(deja$difer2 == min(deja$difer2)),]

        if( nrow(deja1) > 1 ){
          deja2 <- deja1[which(deja1$difer == min(deja1$difer)),]
          dejas2 <- rbind(dejas2, deja2)
        }else if( nrow(deja1) == 1 ){
          dejas2 <- rbind(dejas2, deja1 )
        }
      }
      dejas3 <- NULL
      for(i in unique(dejas2$time2) ){
        deja <- dejas2[which(dejas2$time2 == i),]
        deja1 <- deja[which(deja$difer2 == min(deja$difer2)),]
        if( nrow(deja1) > 1 ){
          deja4 <- deja1[which(deja1$difer == min(deja1$difer)),]
          dejas3 <- rbind(dejas3, deja4)
        }else if( nrow(deja1) == 1 ){
          dejas3 <- rbind(dejas3, deja1 )
        }
      }

      #################################################################################
      #################################################################################
      x <- data.frame(dato2[which(dato2$Max_Min == minmax), "Time"])
      y <- data.frame(dato1[which(dato1$Max_Min == minmax), "Time"])

      pegas <- NULL
      for(i in 1:nrow(y)){
        #i=1
        pega <- cbind(x, c(rep(y[i,],nrow(x))) )
        pegas <- rbind(pegas, pega)
      }

      colnames(pegas) <- c("time","time2")
      pegas$difer <- pegas$time - pegas$time2
      pegas$difer2 <- abs(pegas$time - pegas$time2)

      dejas <- pegas

      dejas4 <- NULL
      for(i in unique(dejas$time) ){
        deja <- dejas[which(dejas$time == i),]
        deja1 <- deja[which(deja$difer2 == min(deja$difer2)),]
        if( nrow(deja1) > 1 ){
          deja2 <- deja1[which(deja1$difer == min(deja1$difer)),]
          dejas4 <- rbind(dejas4, deja2)
        }else if( nrow(deja1) == 1 ){
          dejas4 <- rbind(dejas4, deja1 )
        }
      }
      dejas5 <- NULL
      for(i in unique(dejas4$time2) ){
        deja <- dejas4[which(dejas4$time2 == i),]
        deja1 <- deja[which(deja$difer2 == min(deja$difer2)),]
        if( nrow(deja1) > 1 ){
          deja2 <- deja1[which(deja1$difer == min(deja1$difer)),]
          dejas5 <- rbind(dejas5, deja2)
        }else if( nrow(deja1) == 1 ){
          dejas5 <- rbind(dejas5, deja1 )
        }
      }

      colnames(dejas5) <- c(paste(colnames(dejas3),".2",sep=""))
      dejas6 <- merge(dejas3, dejas5, by.x="time", by.y="time2.2")

      if(nrow(dejas6)>0){
        dejas6$depura <- 0
        for(i in 1:nrow(dejas6)){
          if(dejas6[i,3] == -dejas6[i,6]){
            dejas6[i,8] <- 1
          }
        }

        dejas6 <- dejas6[which(dejas6$depura == 1),-c(ncol(dejas6))]
      }else dejas6 <- NULL
    }else dejas6 <- NULL

  }else dejas6 <- NULL

  return(dejas6)
}# End function





