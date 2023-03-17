library(Rforestry)

set.seed(101)
X <- data.frame(matrix(rnorm(1000*10), ncol = 10))
Y <- rnorm(1000)

tree <- forestry(x= X,
                 y = Y,
                 mtry = 1,
                 ntree = 1,
                 seed = 101,
                 maxDepth = 7)
saveForestry(tree, filename = "code/saved_tree.RDS")

p_before <- predict(tree, newdata = X)
tree_after <- loadForestry("code/saved_tree.RDS")
p <- predict(tree_after, newdata = X)
all.equal(p_before, p)

