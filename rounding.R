# Rounding functions

# Round towards minus infinity
round.floor = function(x, digits=0) { 
  return( floor( x / 10^-digits ) * 10^-digits )
}

# Round towards plus infinity
round.ceiling = function(x, digits=0) {
  return( ceiling( x / 10^-digits ) * 10^-digits )
}

# Round towards infitity (i.e. round away from zero)
round.toInf = function(x, digits=0) {
  return( sign(x) * ceiling( abs( x / 10^-digits ) ) * 10^-digits )
}

# Round towards zero (i.e. round away from infinity)
round.toZero = function(x, digits=0) { 
  return( sign(x) * floor( abs( x / 10^-digits ) ) * 10^-digits )
}

# Round towards even (nearest integer, but tiebreaker goes to even)
round.even = function(x, digits=0){
  return( round( x / 10^-digits ) * 10^-digits )
}

# Round towards odd (nearest integer, but tiebreaker goes to odd)
round.odd = function(x, digits=0){
  if(length(x)>1E7){stop("round.odd will not run with length(x)>1E7")}
  Y <- (x / 10^-digits) %% 2 - 1
  Z <- abs(Y)
  return({x/10^-digits - (Z<=0.5)*Y + (Z>0.5)*(1 - Z)*sign(Y)}*10^-digits)
}

# Round half (with tie-breaker specified by 'half')
round.half = function(x, digits=0, half='even'){
  return( 
    switch( half,
            'up'         =   floor( x / 10^-digits + 0.5 ) * 10^-digits,
            'down'       = ceiling( x / 10^-digits - 0.5 ) * 10^-digits,
            'toInf'      = sign(x) *   floor( abs( x / 10^-digits ) + 0.5 ) * 10^-digits,
            'toZero'     = sign(x) * ceiling( abs( x / 10^-digits ) - 0.5 ) * 10^-digits,
            'even'       = round.even(x, digits),
            'odd'        = round.odd(x, digits),
            'stochastic' = {Y <- rbinom(x,1,0.5)==1
                            x[Y]  <- floor( x[Y] / 10^-digits + 0.5 ) * 10^-digits
                            x[-Y] <- ceiling( x[-Y] / 10^-digits - 0.5 ) * 10^-digits
                            return(x)
            }
    )
  )
}

# Round towards values (with tie-breaker specified by 'half')
round.values = function(x, digits=0, half='up'){ }