library(tidyverse)

almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}

data <- read.delim("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06FI05.txt", sep =" ", header = FALSE) %>%
  select(V3, V5, V7) %>%
  rename(Y = V3,
         X1 = V5,
         X2 = V7)


m1 <- lm(Y ~ X1 + X2, data = data)
summary(m1)

confint(m1)

almost_sas(m1)
 