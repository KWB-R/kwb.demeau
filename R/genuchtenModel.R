#' vanGenuchten : single model
#' 
#' @param pressureHead  pressureHead
#' @param alpha alpha
#' @param beta beta
#' @return data.frame with columns "pressureHead, alpha, beta, effSaturation, Kr"
#' @references http://wwwbrr.cr.usgs.gov/projects/GW_Unsat/vs2di/hlp/solute/vanGenuchten.html


genuchtenModel <- function(pressureHead, alpha, beta) {
  if (pressureHead < 0) {
    gamma <- 1 - 1 / beta
    C <- abs(alpha * pressureHead) ^ (beta - 1)
    D <- 1 + abs(alpha * pressureHead) ^ beta
    
    effSaturation <- 1 / (1 + abs(alpha * pressureHead) ^ beta) ^ gamma
    
    Kr <- (1 - C * D ^ (-gamma)) ^ 2 / D ^ (gamma / 2)
  } else {
    effSaturation <- 1
    Kr <- 1
  }
  return(
    data.frame(
      pressureHead = pressureHead,
      alpha = alpha,
      beta = beta,
      effSaturation = effSaturation,
      Kr = Kr
    )
  )
}

#' vanGenuchten : helper for multiple models 
#'  
#' @param pressureHeads  pressureHeads
#' @param alpha alpha to be used for van Genuchten model 
#' @param beta betaa to be used for van Genuchten model 
#' @return data.frame with columns "pressureHead, alpha, beta, effSaturation, Kr"
#' @references http://wwwbrr.cr.usgs.gov/projects/GW_Unsat/vs2di/hlp/solute/vanGenuchten.html
genuchten <- function(pressureHeads = -rev(seq(0,1000,5)),
                      alpha = 1,
                      beta = 2) {
  tmp <- data.frame()
  for (pressureHead in pressureHeads) {
    tmp <- rbind(tmp, genuchtenModel(pressureHead,
                                     alpha = alpha,
                                     beta = beta))
  }
  return(tmp)
}

#' vanGenuchten : multiple models
#'  
#' @param pressureHeads  pressureHeads
#' @param alphas vector of alphas to be used for multiple model construction
#' @param betas vector of betas to be used for multiple model construction
#' @return data.frame with columns "pressureHead, alpha, beta, effSaturation, Kr"
#' @references http://wwwbrr.cr.usgs.gov/projects/GW_Unsat/vs2di/hlp/solute/vanGenuchten.html

genuchtenModels <- function(pressureHeads = -rev(seq(0,6,0.5)), 
                            alphas=seq(1,2,0.5), 
                            betas=1:5) {
res <- data.frame()
for (myalpha in alphas) {
  for (mybeta in betas) {
    g <- genuchten(pressureHeads = pressureHeads, 
                   alpha = myalpha, 
                   beta = mybeta)

    res <- rbind(res, g)
  }
}
res$label <- sprintf("alpha = %2.2f , beta = %2.2f",res$alpha, res$beta)
return(res)
}

#' vanGenuchten : plot models
#'  
#' @param models  as retrieved by genuchtenModels()
#' @param ... further arguments passed to lattice::xyplot()
#' @return plot of all genuchten models 
#' 
plotGenuchtenModels <- function(models = genuchtenModels(), ...) {
print(lattice::xyplot(as.formula("pressureHead ~ effSaturation | label"), 
                ylim = rev(c(min(models$pressureHead),0)),
                xlim = c(0,1), type = "b", pch = 16, ...,
  data = models, auto.key = TRUE, as.table = TRUE))
}
