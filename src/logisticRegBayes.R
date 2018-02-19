source("mcmc.R")

gen.default.init = function(X) {
  list(coef=double(NCOL(X)), ll=-Inf)
}

logisticRegBayes = function(y, X, init=gen.default.init(X),
                            cs=1, B=1000, burn=5000, thin=1, print_every=0) {
  update = function(state) {
    ll = function(b) {
      p = sigmoid(X %*% b) * .999
      sum(y * log(p) + (1-y) * log(1-p))
    }
    lp = function(b) 0
    state$coef = mh_mv(state$coef, ll, lp, cs)
    state$ll = ll(state$coef)
    state
  }

  init = gen.default.init(X)
 
  gibbs(init, update, B, burn, thin, print_every)
}


