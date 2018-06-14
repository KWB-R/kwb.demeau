#' Helper function for dominantTravelTimes
#' @param domTimes intermediate result of function kwb.demeau::dominantTravelTimes
#' @return list with aggregated dominant travel times (median!)
dominantTravelTimesAgg <- function(domTimes) {
  
  tmp1 <- merge(aggregate(as.formula("TIME_day ~ Name"), 
                          data = domTimes,
                          FUN = median),
                aggregate(as.formula("modelled ~ Name"), 
                          data = domTimes,
                          FUN = median))
  x <- merge(tmp1, aggregate(as.formula("maxConc ~ Name"), 
                             data = domTimes,
                             FUN = median))
  x$offset <- x$maxConc / 2 - x$modelled
  x$Label <- sprintf("%s (t.dom: %3.2f days)", x$Name, x$TIME_day)
  x <- x[order(x$TIME_day),]
  
  return(x)
}


#' Dominant travel time: data preprocessing
#'  
#' @param concModelled as retrieved by kwb.demeau::soluteModelled()
#' @param offset offset (Default: 0.01) used for filtering soluteModel results
#' (i.e. maxConc/2 +- offset)
#' @return list with dominant travel times with sublists "raw" (multiple values for 
#' each TIME_day possible) and "agg" (median "TIME_day" and "modelled" concentration)
dominantTravelTimes <- function(concModelled, 
                                offset = 0.01){
  
  domTimes <- data.frame()
  for (obsWell in unique(concModelled$Name) ) {
    maxConc <- max(concModelled[concModelled$Name == obsWell,"modelled"])
    cond <- concModelled$Name == obsWell & concModelled$modelled >= (maxConc/2 - offset) & concModelled$modelled <= (maxConc/2 + offset) 
    tmp  <- concModelled[cond, ]
    
    dominantTime  <- cbind(tmp, maxConc = rep(maxConc,nrow(tmp)))
    if (obsWell == unique(concModelled$Name)[1]) {
      domTimes <- dominantTime
    } else {
      domTimes <- rbind(domTimes, dominantTime)
    }
  }
  
  
  x <- list(raw = domTimes, 
            agg = dominantTravelTimesAgg(domTimes))
  return(x)
}
