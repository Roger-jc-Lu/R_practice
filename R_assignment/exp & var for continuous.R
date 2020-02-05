exp_exp = function(lambda){
  f = function(x){x*lambda*((exp(1))^(-lambda*x))}
  exp <- integrate(f = f,lower = 0,upper = Inf)
  return(exp$value)
}

var_exp = function(lambda){
  mu <- exp_exp(lambda)
  f = function(x){x^2*lambda*((exp(1))^(-lambda*x))}
  var <- (integrate(f = f,lower = 0,upper = Inf))$value - (mu^2)
  return(var)
}

exp_gamma = function(alpha, lambda){
  f = function(x){(x*(x^(alpha - 1))*(lambda^(alpha))*((exp(1))^(-lambda*x)))/gamma(alpha)}
  exp <- integrate(f = f,lower = 0,upper = Inf)
  return(exp$value)
}

var_gamma = function(alpha, lambda){
  mu <- exp_gamma(alpha, lambda)
  f = function(x){(x^2*(x^(alpha - 1))*(lambda^(alpha))*((exp(1))^(-lambda*x)))/gamma(alpha)}
  var <- (integrate(f = f,lower = 0,upper = Inf))$value - (mu^2)
  return(var)
}

exp_chi_square = function(r){
  f = function(x){(x*(x^(r/2 - 1))*((1/2)^(r/2))*((exp(1))^(-1/2*x)))/gamma(r/2)}
  exp <- integrate(f = f,lower = 0,upper = Inf)
  return(exp$value)
}

var_chi_square = function(r){
  mu <- exp_chi_square(r)
  f = function(x){(x^2*(x^(r/2 - 1))*((1/2)^(r/2))*((exp(1))^(-1/2*x)))/gamma(r/2)}
  var <- (integrate(f = f,lower = 0,upper = Inf))$value - (mu^2)
  return(var)
}

exp_norm = function(exp, var){
  f = function(x){(x*exp(1)^(-(x-exp)^2/(2*var)))/sqrt(2*pi*var)}
  exp <- integrate(f = f,lower = -Inf,upper = Inf)
  return(exp$value)
}

var_norm = function(exp, var){
  mu <- exp_norm(exp, var)
  f = function(x){(x^2*exp(1)^(-(x-exp)^2/(2*var)))/sqrt(2*pi*var)}
  var <- (integrate(f = f,lower = -Inf,upper = Inf))$value - (mu^2)
  return(var)
}