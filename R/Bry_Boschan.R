#' @title Turning points
#'
#' @description Calculates turning points of a time series using the Bry and Boschan (1971) methodology
#' @param x univariate time series
#' @param frequ Frequency of the x, 12 monthly or 4 quarterly. Default value 12.
#' @param year The start year of the time series. Default value 1.
#' @param month The start month of the time series. Default value 1.
#' @return The dates of the turning points
#' @import zoo
#' @importFrom stats sd
#' @export
#' @author Wilmer O Martinez R
#' @references Bry, G. and Boschan, C. (1971)
#'  Cyclical Analysis of Time Series: Selected Procedures
#'  and Computer Programs,
#'  \emph{National Bureau of Economic Research, Inc} \bold{71}(1), 7-63.
#' @references Burns, A.F. and Mitchell, W. (1946)
#'  Measuring Business Cycles,
#'   \emph{National Bureau of Economic Research, NBER, New York.}
#' @examples
#' x <- rnorm(100)
#' TP_BryBoschan(x)
#'
TP_BryBoschan <- function(x, frequ = 12, year = 1, month = 1){

  ###########################################################################################

  serie_orig <- x
  dat_orig <- datos <- as.matrix(serie_orig)

  # frequ = 12 for monthly series
  # frequ = 4 for monthly series

  ### if report.graf = 1, produce a report graf


  ##############  CURVE OF SPENCER WITH 15 SIMETRIC POINTS           ###########

  if(frequ == 12){
    pe_spenceri <- c(-3,-6,-5,3,21,46,67,74,67,46,21,3,-5,-6,-3)
    pe_spencer <- c(-3,-6,-5,3,21,46,67,74,67,46,21,3,-5,-6,-3)/sum(pe_spenceri)
    ini.1 = 8
    fin.1 = 6
    dif.1 = 5
  }else if(frequ == 4){
    # QUATERLY CASE
    pe_spenceri <- c(-3, 12, 17, 12, -3)
    pe_spencer <- c(-3, 12, 17, 12, -3)/sum(pe_spenceri)
    ini.1 = 3
    fin.1 = 1
    dif.1 = 3
  }else {
    stop("This function can only handle monthly and quaterly time series")
  }
  orden <- length(pe_spencer)
  h <- pe_spencer

  media_m1 <- NULL
  for (j in 1:(orden-ini.1)){
    media_m1 <- rbind( media_m1, mean( datos[1:j,] ) )
  }

  media_m2 <- NULL
  for (j in (nrow(datos)-fin.1):nrow(datos)){
    media_m2 <- rbind( media_m2, mean( datos[j:nrow(datos),] ) )
  }

  n <- nrow(datos)-orden + 1
  media_mo <- NULL
  for (j in 1:n){
    s1 <- sum(h*datos[j:(j+orden-1),])
    media_mo <- rbind( media_mo,  s1)
  }

  media_s <- rbind(media_m1,media_mo, media_m2)
  rownames(media_s) <- paste( c(1:nrow(datos)) , sep = "" )
  ##############################################



  ###########             CORRECTION EXTREME VALUES                       #####

  raz <- datos / media_s
  for(i in 1:nrow(raz)){
    if(is.na(raz[i,]) | raz[i,] == -Inf | raz[i,] == Inf) raz[i,] <- 1
  }

  datos1 <- datos
  for(i in 1:nrow(datos)){
    if(abs(raz[i,]) > stats::sd(raz,na.rm = T)*3.5)
      datos1[i,] <- media_s[i,]
  }
  #######################################################################

  #######              CURVE MM(12)   AND    MM(6)                        ####

  mediaMov_h <- function(orden, h, datos){

    media_m2a <- NULL
    for (i in 1:orden){
      media_m2a <- rbind( media_m2a, mean( datos1[1:i,] ) )
    }

    media_m2b <- NULL
    for (i in (nrow(datos)-orden+1):nrow(datos)){
      media_m2b <- rbind( media_m2b, mean( datos1[i:nrow(datos),] ) )
    }

    n <- nrow(datos1)-2*orden
    media_mov <- NULL
    for (j in 1:n){
      s1 <- sum(h*datos1[j:(j+2*orden),])
      media_mov <- rbind( media_mov,  s1)
    }

    media_ord <- rbind(media_m2a,media_mov,media_m2b)
    rownames(media_ord) <- paste( c(1:nrow(datos1)) , sep = "" )
    return(media_ord)
  }

  if(frequ == 12) mm12 <- mediaMov_h(12,1/25,datos1) else mm12 <- mediaMov_h(3,1/5,datos1)

  ###########################################################################################
  ###########################################################################################

  #######        THE NEXT FUNCTION DETERMIN LOCAL MAXs AND MINs                 ####

  # CHOOSE EXTREME VALUES BETWEEN +-5

  max_min_locales <- function(dat_orig, datos){

    cual_max <- NULL
    for(i in 1:(nrow(datos)- dif.1)){
      dat5 <- datos[i:(i+ (dif.1-1)),]
      cual_max <- rbind(cual_max, which.max(dat5))
    }
    cual_max1 <- 0:(nrow(cual_max)-1)
    cual_max <- cual_max + cual_max1

    cand_max1 <- NULL
    for(k in dif.1:nrow(cual_max)){
      if(cual_max[k,] == cual_max[(k-(dif.1-1)),])
        cand_max1 <- rbind(cand_max1,cual_max[k,])
    }

    cual_min <- NULL
    for(i in 1:(nrow(datos)-dif.1)){
      dat5 <- datos[i:(i+ (dif.1-1) ),]
      cual_min <- rbind(cual_min, which.min(dat5))
    }
    cual_min1 <- 0:(nrow(cual_min)-1)
    cual_min <- cual_min + cual_min1

    cand_min1 <- NULL
    for(k in dif.1:nrow(cual_min)){
      if(cual_min[k,] == cual_min[(k- (dif.1-1) ),])
        cand_min1 <- rbind(cand_min1,cual_min[k,])
    }

    cand_max1 <- data.frame(cand_max1)
    if(nrow(cand_max1) > 0){
      cand_max1$indi <- 2
      colnames(cand_max1) <- c("time","max_min")
    }else
      cand_max1 <- NULL

    cand_min1 <- data.frame(cand_min1)
    if(nrow(cand_min1) > 0){
      cand_min1$indi <- 1
      colnames(cand_min1) <- c("time","max_min")
    }else
      cand_min1 <- NULL

    if(nrow(data.frame(cand_min1)) > 0 | nrow(data.frame(cand_max1)) > 0 ){

      max_minlo <- rbind(cand_max1 , cand_min1)
      max_minlo <- stats::na.omit(max_minlo)
      max_minlo <- max_minlo[order(max_minlo$time),]

      max_minlo$distan <- 0
    }else max_minlo <- data.frame(max_minlo <- NULL)

    if(nrow(max_minlo) > 2){
      for(p in 2:nrow(max_minlo)){
        max_minlo[p,3] <- max_minlo[p,1] - max_minlo[(p-1),1]
      }

      max_minlo$valor <- dat_orig[c(max_minlo$time),]

      max_minlo2 <- max_minlo
      max_minlo2$quitar <- 0

      max_minlo2 <- max_minlo2[,-c(6,7)]

      for(q in 2:nrow(max_minlo2)){
        if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 1){
          max_minlo2[q,5] <- min(max_minlo2[q,4], max_minlo2[(q-1),4])
          max_minlo2[(q-1),5] <- min(max_minlo2[q,4], max_minlo2[(q-1),4])
        }
      }

      r=1
      while(r < 10 & nrow(max_minlo2)>1 ){
        for(q in 2:nrow(max_minlo2)){
          if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 1){
            max_minlo2[q,5] <- min(max_minlo2[q,5], max_minlo2[(q-1),5])
            max_minlo2[(q-1),5] <- min(max_minlo2[q,5], max_minlo2[(q-1),5])
          }
          else if( max_minlo2[q,2] != max_minlo2[(q-1),2] & max_minlo2[q,2] == 1){
            max_minlo2[q,5] <- max_minlo2[q,4]
          }
        }
        r <- r+1
      }


      for(q in 2:nrow(max_minlo2)){
        if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 2){
          max_minlo2[q,5] <- max(max_minlo2[q,4], max_minlo2[(q-1),4])
          max_minlo2[(q-1),5] <- max(max_minlo2[q,4], max_minlo2[(q-1),4])
        }
      }

      r=1
      while(r < 10){
        for(q in 2:nrow(max_minlo2)){
          if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 2){
            max_minlo2[q,5] <- max(max_minlo2[q,5], max_minlo2[(q-1),5])
            max_minlo2[(q-1),5] <- max(max_minlo2[q,5], max_minlo2[(q-1),5])
          }
          else if( max_minlo2[q,2] != max_minlo2[(q-1),2] & max_minlo2[q,2] == 2){
            max_minlo2[q,5] <- max_minlo2[q,4]
          }
        }
        r <- r+1
      }

      if(max_minlo2[1,5] == 0) max_minlo2[1,5] <- max_minlo2[1,4]

      max_minlo3 <- NULL

      for(q in 1:nrow(max_minlo2)){
        if( max_minlo2[q,4] == max_minlo2[q,5]){
          max_minlo3 <- rbind(max_minlo3, max_minlo2[q,])
        }
      }

      max_minlo3 <- data.frame(max_minlo3)

      if(nrow(max_minlo3) >= 3){
        for(q in 2:nrow(max_minlo3)){
          max_minlo3[q,3] <- max_minlo3[q,1] - max_minlo3[(q-1),1]
        }
        max_minlo3$distan2 <- 0

        for(q in 3:nrow(max_minlo3)){
          max_minlo3[q,6] <- max_minlo3[q,1] - max_minlo3[(q-2),1]
        }

        max_minlo4 <- NULL
        max_minlo4 <- max_minlo3[1,]

        for(q in 2:nrow(max_minlo3)){
          if(max_minlo3[q,3] >= 3)
            max_minlo4 <- rbind(max_minlo4, max_minlo3[q,])
        }

        max_minlo2 <- max_minlo4
        max_minlo2$quitar <- 0

        for(q in 2:nrow(max_minlo2)){
          if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 1){
            max_minlo2[q,5] <- min(max_minlo2[q,4], max_minlo2[(q-1),4])
            max_minlo2[(q-1),5] <- min(max_minlo2[q,4], max_minlo2[(q-1),4])
          }
        }
        r=1
        while(r < 10){
          for(q in 2:nrow(max_minlo2)){
            if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 1){
              max_minlo2[q,5] <- min(max_minlo2[q,5], max_minlo2[(q-1),5])
              max_minlo2[(q-1),5] <- min(max_minlo2[q,5], max_minlo2[(q-1),5])
            }
            else if( max_minlo2[q,2] != max_minlo2[(q-1),2] & max_minlo2[q,2] == 1){
              max_minlo2[q,5] <- max_minlo2[q,4]
            }
          }
          r <- r+1
        }

        for(q in 2:nrow(max_minlo2)){
          if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 2){
            max_minlo2[q,5] <- max(max_minlo2[q,4], max_minlo2[(q-1),4])
            max_minlo2[(q-1),5] <- max(max_minlo2[q,4], max_minlo2[(q-1),4])
          }
        }
        r=1
        while(r < 10){
          for(q in 2:nrow(max_minlo2)){
            if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 2){
              max_minlo2[q,5] <- max(max_minlo2[q,5], max_minlo2[(q-1),5])
              max_minlo2[(q-1),5] <- max(max_minlo2[q,5], max_minlo2[(q-1),5])
            }
            else if( max_minlo2[q,2] != max_minlo2[(q-1),2] & max_minlo2[q,2] == 2){
              max_minlo2[q,5] <- max_minlo2[q,4]
            }
          }
          r <- r+1
        }

        if(max_minlo2[1,5] == 0) max_minlo2[1,5] <- max_minlo2[1,4]

        ###############################################################################
        max_minlo5 <- NULL
        for(q in 1:nrow(max_minlo2)){
          if( max_minlo2[q,4] == max_minlo2[q,5]){
            max_minlo5 <- rbind(max_minlo5, max_minlo2[q,])
          }
        }
        #colnames(max_minlo5)[5] <- "quitar2"
        return(max_minlo5)
      }else if(nrow(max_minlo) == 1){
        max_minlo$valor <- dat_orig[c(max_minlo$time),]
        max_minlo$quitar <- 0
        max_minlo$distan2 <- 0
        return(max_minlo)
      }else if(nrow(max_minlo) == 0){
        return(max_minlo <- NULL)
      }

    }

  }


  ###########################################################################################
  ###########################################################################################

  ###           FUNCTION THAT DROP CONSECUTIVE MAXs OR MINs

  consecu_min_max <- function(dat){
    if(nrow(dat) > 1){
      max_minlo2 <- dat
      max_minlo2[,5] <- 0
      #max_minlo2$quitar <- 0

      for(q in 2:nrow(max_minlo2)){
        if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 1 & max_minlo2[q,4] != max_minlo2[(q-1),4]){
          max_minlo2[q,5] <- min(max_minlo2[q,4], max_minlo2[(q-1),4])
          max_minlo2[(q-1),5] <- min(max_minlo2[q,4], max_minlo2[(q-1),4])
        }else if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 1 & max_minlo2[q,4] == max_minlo2[(q-1),4]){
          max_minlo2[q,5] <- -1
          max_minlo2[(q-1),5] <- -1
        }
      }
      aa1 <- max_minlo2[,5] != -1
      max_minlo2 <- max_minlo2[aa1,]
      r=1
      while(r < 10){
        for(q in 2:nrow(max_minlo2)){
          if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 1){
            max_minlo2[q,5] <- min(max_minlo2[q,5], max_minlo2[(q-1),5])
            max_minlo2[(q-1),5] <- min(max_minlo2[q,5], max_minlo2[(q-1),5])
          }
          else if( max_minlo2[q,2] != max_minlo2[(q-1),2] & max_minlo2[q,2] == 1){
            max_minlo2[q,5] <- max_minlo2[q,4]
          }
        }
        r <- r+1
      }

      for(q in 2:nrow(max_minlo2)){
        if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 2 & max_minlo2[q,4] != max_minlo2[(q-1),4]){
          max_minlo2[q,5] <- max(max_minlo2[q,4], max_minlo2[(q-1),4])
          max_minlo2[(q-1),5] <- max(max_minlo2[q,4], max_minlo2[(q-1),4])
        }else if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 1 & max_minlo2[q,4] == max_minlo2[(q-1),4]){
          max_minlo2[q,5] <- -1
          max_minlo2[(q-1),5] <- -1
        }
      }
      aa1 <- max_minlo2[,5] != -1
      max_minlo2 <- max_minlo2[aa1,]

      r=1
      while(r < 10){
        for(q in 2:nrow(max_minlo2)){
          if( max_minlo2[q,2] == max_minlo2[(q-1),2] & max_minlo2[q,2] == 2){
            max_minlo2[q,5] <- max(max_minlo2[q,5], max_minlo2[(q-1),5])
            max_minlo2[(q-1),5] <- max(max_minlo2[q,5], max_minlo2[(q-1),5])
          }
          else if( max_minlo2[q,2] != max_minlo2[(q-1),2] & max_minlo2[q,2] == 2){
            max_minlo2[q,5] <- max_minlo2[q,4]
          }
        }
        r <- r+1
      }

      if(max_minlo2[1,5] == 0) max_minlo2[1,5] <- max_minlo2[1,4]

      ###############################################################################
      max_minlo5 <- NULL
      for(q in 1:nrow(max_minlo2)){
        if( max_minlo2[q,4] == max_minlo2[q,5]){
          max_minlo5 <- rbind(max_minlo5, max_minlo2[q,])
        }
      }
      ###############################################################################
      return(max_minlo5)
    }else return(max_minlo5 <- NULL)
  } # END FUNCTION

  ###########################################################################################
  ###########################################################################################

  ###########################################################################################
  ################   STEP 2                                  #########

  ##  COMPARE TURNING POINTS BETWEEN MM12 AND SPENCER CURVE

  etapa2a <- max_min_locales(dat_orig,mm12)
  etapa2b <- max_min_locales(dat_orig,datos)

  eliminar_r <- function(daticos,var){
    if(nrow(daticos) > 1){
      daticos$elimi <- 0
      for(q in 2:nrow(daticos)){
        if(daticos[q,var] == daticos[(q-1),var]){
          daticos[q,ncol(daticos)] <- 1
        }
      }
      #daticos <- subset(daticos, elimi == 0, select=c(names(daticos)[1:(ncol(daticos)-1)]) )
      aa2 <- daticos$elimi == 0
      daticos <- daticos[aa2,-c(ncol(daticos))]
    }else daticos <- daticos
    return(daticos)
  }

  if( nrow(data.frame(etapa2a)) > 0 &  nrow(data.frame(etapa2b)) > 0 ){
    etapa2a$origen <- 10
    etapa2b$origen <- 10
    etapa2c <- rbind(etapa2a,etapa2b)
    etapa2c <- etapa2c[order(etapa2c$time),]
    etapa2c <- eliminar_r(etapa2c,1)
    etapa2 <- consecu_min_max(etapa2c)
    etapa2c <- unique(etapa2c)
  }else if( nrow(data.frame(etapa2a)) > 0 &  nrow(data.frame(etapa2b)) == 0 ){
    etapa2a$origen <- 10
    etapa2c <- rbind(etapa2a)
    etapa2c <- etapa2c[order(etapa2c$time),]
    etapa2c <- eliminar_r(etapa2c,1)
    etapa2 <- consecu_min_max(etapa2c)
    etapa2 <- unique(etapa2)
  }else if( nrow(data.frame(etapa2a)) == 0 &  nrow(data.frame(etapa2b)) > 0 ){
    etapa2b$origen <- 10
    etapa2c <- rbind(etapa2b)
    etapa2c <- etapa2c[order(etapa2c$time),]
    etapa2c <- eliminar_r(etapa2c,1)
    etapa2 <- consecu_min_max(etapa2c)
    etapa2 <- unique(etapa2)
  }else etapa2 <- NULL


  ###########################################################################################
  ###########################################################################################

  #############               STEP 3                           #######

  ## COMPARE TURNING POINTS FROM STEP 2 AND MM6 CURVE

  mm6 <- mediaMov_h(3,1/5,datos1)


  etapa3a <- max_min_locales(dat_orig, mm6)
  etapa3b <- etapa2

  if( nrow(data.frame(etapa3a)) > 0 &  nrow(data.frame(etapa3b)) > 0 ){
    etapa3a$origen <- 10
    etapa3b$origen <- 10
    etapa3c <- rbind(etapa3a,etapa3b)
    etapa3c <- etapa3c[order(etapa3c$time),]
    etapa3c <- eliminar_r(etapa3c,1)
    etapa3 <- consecu_min_max(etapa3c)
    etapa3 <- unique(etapa3)
  }else if( nrow(data.frame(etapa3a)) > 0 &  nrow(data.frame(etapa3b)) == 0 ){
    etapa3a$origen <- 10
    etapa3c <- rbind(etapa3a)
    etapa3c <- etapa3c[order(etapa3c$time),]
    etapa3c <- eliminar_r(etapa3c,1)
    etapa3 <- consecu_min_max(etapa3c)
    etapa3 <- unique(etapa3)
  }else if( nrow(data.frame(etapa3a)) == 0 &  nrow(data.frame(etapa3b)) > 0 ){
    etapa3b$origen <- 10
    etapa3c <- rbind(etapa3b)
    etapa3c <- etapa3c[order(etapa3c$time),]
    etapa3c <- eliminar_r(etapa3c,1)
    etapa3 <- consecu_min_max(etapa3c)
    etapa3 <- unique(etapa3)
  }else etapa3 <- NULL
  ###########################################################################################
  ###########################################################################################

  #######                 STEP 4                                  ####

  ## COMPARE TURNING POINTS FROM STEP 3 AND MM6 CURVE

  etapa4a <- max_min_locales(dat_orig,datos)
  etapa4b <- etapa3

  if( nrow(data.frame(etapa4a)) > 0 &  nrow(data.frame(etapa4b)) > 0 ){
    etapa4a$origen <- 10
    etapa4b$origen <- 10
    etapa4c <- rbind(etapa4a,etapa4b)
    etapa4c <- etapa4c[order(etapa4c$time),]
    etapa4c <- eliminar_r(etapa4c,1)
    etapa4d <- consecu_min_max(etapa4c)
    etapa4e <- unique(etapa4d)
  }else if( nrow(data.frame(etapa4a)) > 0 &  nrow(data.frame(etapa4b)) == 0 ){
    etapa4a$origen <- 10
    etapa4c <- rbind(etapa4a)
    etapa4c <- etapa4c[order(etapa4c$time),]
    etapa4c <- eliminar_r(etapa4c,1)
    etapa4d <- consecu_min_max(etapa4c)
    etapa4e <- unique(etapa4d)
  }else if( nrow(data.frame(etapa4a)) == 0 &  nrow(data.frame(etapa4b)) > 0 ){
    etapa4b$origen <- 10
    etapa4c <- rbind(etapa4b)
    etapa4c <- etapa4c[order(etapa4c$time),]
    etapa4c <- eliminar_r(etapa4c,1)
    etapa4d <- consecu_min_max(etapa4c)
    etapa4e <- unique(etapa4d)
  }else etapa4e <- NULL

  if( nrow(data.frame(etapa4e)) > 0 ){

    max_minlo <- etapa4e
    max_minlo$distan <- 0
    for(p in 2:nrow(max_minlo)){
      max_minlo[p,3] <- max_minlo[p,1] - max_minlo[(p-1),1]
    }

    max_minlo$distan2 <- 0
    if(nrow(data.frame(max_minlo)) > 2){
      for(q in 3:nrow(max_minlo)){
        max_minlo[q,6] <- max_minlo[q,1] - max_minlo[(q-2),1]
      }
    }

    max_minlo4 <- NULL
    max_minlo4 <- max_minlo[1,]
    for(q in 2:nrow(max_minlo)){
      if(max_minlo[q,3] >= dif.1)
        max_minlo4 <- rbind(max_minlo4, max_minlo[q,])
    }

    max_minlo4 <- consecu_min_max(max_minlo4)

    if( length(max_minlo4) == 0){
      warning("No turning points found")
    }else if( nrow(max_minlo4) >= 3){
      max_minlo <- max_minlo4
      max_minlo$distan <- 0
      for(p in 2:nrow(max_minlo)){
        max_minlo[p,3] <- max_minlo[p,1] - max_minlo[(p-1),1]
      }
      max_minlo$distan2 <- 0
      for(q in 3:nrow(max_minlo)){
        max_minlo[q,6] <- max_minlo[q,1] - max_minlo[(q-2),1]
      }

    }else if( nrow(max_minlo4) >= 2){
      max_minlo <- max_minlo4
      max_minlo$distan <- 0
      for(p in 2:nrow(max_minlo)){
        max_minlo[p,3] <- max_minlo[p,1] - max_minlo[(p-1),1]
      }
      max_minlo$distan2 <- 0
    }else{
      max_minlo <- max_minlo4
      max_minlo$distan <- 0
      max_minlo$distan2 <- 0
    }
    if(max_minlo[1,1] < 6) max_minlo <- max_minlo[2:nrow(max_minlo),]
    if(max_minlo[nrow(max_minlo),1] > (nrow(datos)-6)) max_minlo <- max_minlo[1:(nrow(max_minlo)-1),]

    etapa4 <- max_minlo
    etapa4 <- etapa4[,-c(5,7)]

  }else etapa4 <- NULL

    # Define dates
    ndat <- 1:length(x)
    serie.x <- stats::ts(ndat, start=c(year, month), frequency = frequ)
    dates.x <- data.frame(as.yearmon(stats::time(serie.x)), ndat)
    colnames(dates.x) <- c("Date","time")

  if(length(etapa4) > 0){
    etapa4 <- merge(dates.x, etapa4, by.x="time",by.y="time", all.y=T)
    colnames(etapa4) <- c("Time", "Date", "Max_Min", "Fase", "value", "cycle")
  }else warning("No Turning points found")

  #######################################################################
  return(etapa4)
} #  End function bry_boschan
