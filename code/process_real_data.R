library(readr)
set.seed(1)

# Process abalone
abalone <- read_csv("data/abalone.csv", col_names = FALSE)
abalone$X1 <- as.integer(as.factor(abalone$X1))

colnames(abalone) = c(paste0("X",1:(ncol(abalone)-1)), "y")
write.csv(abalone, "data/abalone_processed.csv", row.names = FALSE)

train_idx <- sample(1:nrow(abalone), size = round(.85*nrow(abalone)), replace = FALSE)
test_idx <- setdiff(1:nrow(abalone), train_idx)

write.csv(abalone[train_idx,], "data/aba_train.csv", row.names = FALSE)
write.csv(abalone[test_idx,], "data/aba_test.csv", row.names = FALSE)

# Process CA housing
housing <- read_csv("data/housing.csv")
housing$ocean_proximity <- as.integer(as.factor(housing$ocean_proximity))
housing$y = housing$median_house_value
housing = housing[,-9]

write.csv(housing, "data/housing_processed.csv", row.names = FALSE)
train_idx <- sample(1:nrow(housing), size = round(.85*nrow(housing)), replace = FALSE)
test_idx <- setdiff(1:nrow(housing), train_idx)

write.csv(housing[train_idx,], "data/house_train.csv", row.names = FALSE)
write.csv(housing[test_idx,], "data/house_test.csv", row.names = FALSE)


# Process satellite image data
satellite <- read_csv("data/satellite.csv")


satellite = satellite[,-ncol(satellite)]
satellite = satellite+.05
satellite$y = satellite$V1
satellite = satellite[,-1]
colnames(satellite) = c(paste0("X",1:(ncol(satellite)-1)), "y")
write.csv(satellite, "data/satellite_processed.csv", row.names = FALSE)

train_idx <- sample(1:nrow(satellite), size = round(.85*nrow(satellite)), replace = FALSE)
test_idx <- setdiff(1:nrow(satellite), train_idx)
write.csv(satellite[train_idx,], "data/sat_train.csv", row.names = FALSE)
write.csv(satellite[test_idx,], "data/sat_test.csv", row.names = FALSE)




# Process diabetes data
diabetes = read.delim("data/diabetes.tab.txt")
colnames(diabetes) = c(paste0("X",1:(ncol(diabetes)-1)), "y")

train_idx <- sample(1:nrow(diabetes), size = round(.85*nrow(diabetes)), replace = FALSE)
test_idx <- setdiff(1:nrow(diabetes), train_idx)
write.csv(diabetes[train_idx,], "data/dia_train.csv", row.names = FALSE)
write.csv(diabetes[test_idx,], "data/dia_test.csv", row.names = FALSE)




# Read in tumor data 
tumor = read.delim("data/1201_BNG_breastTumor.tsv")

colnames(tumor) = c(paste0("X",1:(ncol(tumor)-1)), "y")

train_idx <- sample(1:nrow(tumor), size = round(.85*nrow(tumor)), replace = FALSE)
test_idx <- setdiff(1:nrow(tumor), train_idx)
write.csv(tumor[train_idx,], "data/tumor_train.csv", row.names = FALSE)
write.csv(tumor[test_idx,], "data/tumor_test.csv", row.names = FALSE)


# Read in echo months data 
echo = read.delim("data/1199_BNG_echoMonths.tsv")

colnames(echo) = c(paste0("X",1:(ncol(tumor)-1)), "y")

train_idx <- sample(1:nrow(echo), size = round(.85*nrow(echo)), replace = FALSE)
test_idx <- setdiff(1:nrow(echo), train_idx)
write.csv(echo[train_idx,], "data/echo_train.csv", row.names = FALSE)
write.csv(echo[test_idx,], "data/echo_test.csv", row.names = FALSE)




