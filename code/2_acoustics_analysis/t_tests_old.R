## OLD STATISTICAL ASSESSMENT OF SEASONALITY USING T-TESTS

## month-to-month comparisons of % presence distribution
pvs1 <- matrix(NA,12,1)
s1 <- allmonths %>% filter(month == 1) # peak month
for (m in 2:12) {
  s2 <- allmonths %>% filter(month == m)
  t <- t.test(s1$perc,s2$perc)
  pvs1[m] <- t$p.value
}
stars1 <- as.data.frame(matrix(NA,4,2))
colnames(stars1) <- c("y","x")
stars1$y <- 5
stars1$x <- which(pvs1 < 0.05)

pvs2 <- matrix(NA,12,1)
s7 <- allmonths %>% filter(month == 7) # trough month
for (m in c(1:6,8:12)) {
  s2 <- allmonths %>% filter(month == m)
  t <- t.test(s7$perc,s2$perc)
  pvs2[m] <- t$p.value
}
stars2 <- as.data.frame(matrix(NA,6,2))
colnames(stars2) <- c("y","x")
stars2$y <- 5
stars2$x <- which(pvs2 < 0.05)