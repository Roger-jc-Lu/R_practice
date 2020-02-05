exp_binom = function(n, p){
  exp <- 0
  for (i in 0:n){
    f = i * dbinom(i, size = n, prob = p)
    exp = exp + f
  }
  return(exp)
}

var_binom = function(n, p){
  exp <- exp_binom(n, p)
  var <- 0
  for (i in 0:n){
    f = i^2 * dbinom(i, size = n, prob = p)
    var = var + f
  }
  var = var - exp^2
  return(var)
}

exp_geom = function(p){
  exp <- 0
  for (i in 0:99999){
    f = i * dgeom(i, prob = p)
    exp = exp + f
  }
  exp = exp + 1
  return(exp)
}

var_geom = function(p){
  exp <- exp_geom(p)
  var <- 0
  for (i in 0:99999){
    f = i^2 * dgeom(i, prob = p)
    var = var + f
  }
  var = var - (exp - 1)^2
  return(var)
}

exp_negBinom = function(r, p){
  exp <- 0
  for (i in 0:99999){
    f = i * dnbinom(i, size = r, prob = p)
    exp = exp + f
  }
  exp = exp + r
  return(exp)
}

var_negBinom = function(r, p){
  exp <- exp_negBinom(r, p)
  var <- 0
  for (i in 0:99999){
    f = i^2 * dnbinom(i, size = r, prob = p)
    var = var + f
  }
  var = var - (exp - r)^2
  return(var)
}

exp_hyperGeom = function(N, K, n){
  exp <- 0
  for (i in 0:n){
    f = i * dhyper(i, m = K, n = N - K, k = n)
    exp = exp + f
  }
  return(exp)
}

var_hyperGeom = function(N, K, n){
  exp <- exp_hyperGeom(N, K, n)
  var <- 0
  for (i in 0:n){
    f = i^2 * dhyper(i, m = K, n = N - K, k = n)
    var = var + f
  }
  var = var - exp^2
  return(var)
}