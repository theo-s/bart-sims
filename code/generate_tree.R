library(Rforestry)


for (seed in 1:10) {
  set.seed(seed)
  X <- data.frame(V1 = matrix(rnorm(1000), ncol = 1))
  Y <- rnorm(1000)
  tree <- forestry(x= X,
                   y = Y,
                   mtry = 1,
                   ntree = seed,
                   seed = 101,
                   maxDepth = 7)
  saveForestry(tree, filename = paste0("code/saved_tree",seed,".RDS"))
  
}

p_before <- predict(tree, newdata = X)
tree_after <- loadForestry("code/saved_tree.RDS")
p <- predict(tree_after, newdata = X)
all.equal(p_before, p)

