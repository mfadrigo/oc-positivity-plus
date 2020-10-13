##
#####	Collection of functions written by D. Gillen that are used
#####	throughout Stat 255
#####
#####	Author: D. Gillen
#####	Course: Stat 255, Spring 2020
##
##
#####
#####	ifelse1() :	Helper function that allows for 
#####							returning a multivariate response
#####							based upon a univariate logical comparison
#####
##

ifelse1 <- function (test, yes, no){
  if (test) yes
  else no
}

#####	Function to compute robust se for glms
##
robust.se.glm<-function(glm.obj){
  ## 	Compute robust (sandwich) variance estimate
  if (is.matrix(glm.obj$x)) 
    xmat<-glm.obj$x
  else {
    mf<-model.frame(glm.obj)
    xmat<-model.matrix(terms(glm.obj),mf)		
  }
  umat <- residuals(glm.obj,"working")*glm.obj$weights*xmat
  modelv<-summary(glm.obj)$cov.unscaled
  robust.cov <- modelv%*%(t(umat)%*%umat)%*%modelv
  
  ##	Format the model output with p-values and CIs
  s <- summary( glm.obj) 
  robust.se <- sqrt( diag( robust.cov )) 
  z <- glm.obj$coefficients/robust.se
  p <- 2*pnorm( -abs( z ) ) 
  ci95.lo <- glm.obj$coefficients - qnorm( .975 ) * robust.se
  ci95.hi <- glm.obj$coefficients + qnorm( .975 ) * robust.se
  rslt <- cbind( glm.obj$coefficients, robust.se, ci95.lo, ci95.hi, z, p ) 
  dimnames(rslt)[[2]] <- c( dimnames( s$coefficients )[[2]][1], "Robust SE", "ci95.lo", "ci95.hi", dimnames( s$coefficients )[[2]][3:4] ) 
  rslt 
}



#  Function to exponentiate coefficients and produces CIs for GLMs
glmCI <- function( model, transform=TRUE, robust=FALSE ){
  link <- model$family$link
  coef <- summary( model )$coef[,1]
  se <- ifelse1( robust, robust.se.glm(model)[,2], summary( model )$coef[,2] )
  zvalue <- coef / se
  pvalue <- 2*(1-pnorm(abs(zvalue)))
  
  if( transform & is.element(link, c("logit","log")) ){
    ci95.lo <- exp( coef - qnorm(.975) * se )
    ci95.hi <- exp( coef + qnorm(.975) * se )
    est <- exp( coef )
  }
  else{
    ci95.lo <- coef - qnorm(.975) * se
    ci95.hi <- coef + qnorm(.975) * se
    est <- coef
  }
  rslt <- round( cbind( est, ci95.lo, ci95.hi, zvalue, pvalue ), 4 )
  colnames( rslt ) <- ifelse1( 	robust, 	
                                c("Est", "robust ci95.lo", "robust ci95.hi", "robust z value", "robust Pr(>|z|)"),
                                c("Est", "ci95.lo", "ci95.hi", "z value", "Pr(>|z|)") )			
  colnames( rslt )[1] <- ifelse( transform & is.element(link, c("logit","log")), "exp( Est )", "Est" )
  rslt
}

