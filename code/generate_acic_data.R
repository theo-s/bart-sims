
# Generate 2019 data -----------------------------------------------------------
# Read in 2019 contest data
data_2019 <- read.csv("~/Desktop/bart-sims/data/testdataset7.csv")

out_2019 <- read.csv("~/Desktop/bart-sims/data/testdataset7_cf.csv")
data_2019$Ytrue <- ifelse(data_2019$A, out_2019$EY1_i, out_2019$EY0_i)
write.csv(data_2019, "~/Desktop/bart-sims/data/ACIC2019.csv", row.names = FALSE)


# Generate 2018 data -----------------------------------------------------------
data_2018 <- read.csv("~/Desktop/bart-sims/data/ACIC2018synthetic_data.csv")

# Rename columns
colnames(data_2018) <- c("schoolid", "Tr","Y","S3","C1","C2","C3","XC","X1","X2","X3","X4","X5")
# Generate tau
tau_true <- function(X) {
  return(0.228 + ifelse(X$X1 < 0.07, .05, 0) + ifelse(X$X2 < -0.69, .05, 0) +
           ifelse(X$C1 %in% c(1,13,14), -.08,0))
}
data_tau <- tau_true(data_2018)
data_2018$Ytrue <- ifelse(data_2018$Tr, data_2018$Y+data_tau, data_2018$Y)
# Add noise to true potential outcomes to give an SNR of 2
data_2018$Y <- data_2018$Ytrue + rnorm(nrow(data_2018), sd = sqrt(var(data_2018$Y)/2))
write.csv(data_2018, "~/Desktop/bart-sims/data/ACIC2018.csv", row.names = FALSE)

# Generate 2017 data -----------------------------------------------------------

# Read in 2017 contest data
data_2017 <- read.csv("~/Desktop/bart-sims/data/X.csv", header = FALSE)
ytrue_2017 <- read.csv("~/Desktop/bart-sims/data/dgp.csv")
y_2017 <- read.csv("~/Desktop/bart-sims/data/1.csv", header = FALSE)

# mu is outcome under no treatment, tau is the cate
data_2017$Y <- y_2017$V2
data_2017$Ytrue <- ifelse(y_2017$V1, ytrue_2017$mu+ytrue_2017$alpha, ytrue_2017$mu)
write.csv(data_2017, "~/Desktop/bart-sims/data/ACIC2017.csv", row.names = FALSE)


# Generate 2016 data -----------------------------------------------------------
# Read in 2016 contest data
data_2016 <- read.csv("~/Desktop/bart-sims/data/ACIC2016_x.csv")
out_2016 <- read.csv("~/Desktop/bart-sims/data/ACIC2016_zymu_1.csv")

data_2016$Y <- ifelse(out_2016$z, out_2016$y1, out_2016$y0)
data_2016$Ytrue <- ifelse(out_2016$z, out_2016$mu1, out_2016$mu0)
write.csv(data_2016,"~/Desktop/bart-sims/data/ACIC2016.csv", row.names = FALSE)
