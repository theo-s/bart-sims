# Define all estimators:========================================================
# BART with standard hyperparameters
bart_fit <- function(Xobs,
                     Yobs,
                     seed=1,
                     note = NA) {

  library(dbarts)

  e <- bart(x.train = Xobs,
            y.train = Yobs,
            keeptrees = TRUE,
            verbose = FALSE,
            nskip = 1,
            ntree = 1,
            seed = seed,
            ndpost = 20e3)

  return(list("bart" = e))
}

# BART with only grow and prune moves
bart_fit_grow_prune <- function(Xobs,
                                Yobs,
                                seed=1,
                                note = NA) {

  library(dbarts)

  probs <- c(1-1e-5,1e-5,0,.5)
  names(probs) <- c("birth_death", "change","swap","birth")

  e <- bart(x.train = Xobs,
            y.train = Yobs,
            keeptrees = TRUE,
            verbose = FALSE,
            nskip = 1,
            ntree = 1,
            proposalprobs = probs,
            seed = seed,
            ndpost = 20e3)

  return(list("bart" = e))
}

bart_bartMachine <- function(Xobs,
                             Yobs,
                             seed=1,
                             note = NA) {

  library(bartMachine)

  e <- bartMachine(X = Xobs,
                   y = Yobs,
                   num_trees = 1,
                   mh_prob_steps = c(.5,.5,0),
                   num_burn_in = 15e3,
                   num_iterations_after_burn_in = 5e3)

  return(list("bart" = e))
}


bart_predict <- function(estimator, feat) {
    s <- predict(estimator, feat)$bart
    #return(apply(s, 2, mean))
    return(s) #for now return entire posterior sample
}


