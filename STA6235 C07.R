library(tidyverse)

x1 <- c(19.5, 24.7, 30.7, 29.8, 19.1, 25.6, 31.4, 27.9,
        22.1, 25.5, 31.1, 30.4, 18.7, 19.7, 14.6, 29.5,
        27.7, 30.2, 22.7, 25.2)
    
x2 <- c(43.1, 49.8, 51.9, 54.3, 42.2, 53.9, 58.5, 52.1,
        49.9, 53.5, 56.6, 56.7, 46.5, 44.2, 42.7, 54.4,
        55.3, 58.6, 48.2, 51.0)

x3 <- c(29.1, 28.2, 37.0, 31.1, 30.9, 23.7, 27.6, 30.6,
        23.2, 24.8, 30.0, 28.3, 23.0, 28.6, 21.3, 30.1,
        25.7, 24.6, 27.1, 27.5)

y <- c(11.9, 22.8, 18.7, 20.1, 12.9, 21.7, 27.1, 25.4,
       21.3, 19.3, 25.4, 27.2, 11.7, 17.8, 12.8, 23.9,
       22.6, 25.4, 14.8, 21.1)

one <- tibble(y, x1, x2, x3)

# matrix of scatterplots
# the 2 tells it to start with col 2 and end with col 4 (so skip the y col)
pairs(one[,2:4], pch = 19)

# this one suppresses the lower half of the matrix
#pairs(one[,2:4], pch = 19, lower.panel = NULL)

# a second option for matrix of scatterplots
# plot(one, pch=3, cex=1, col="#69b3a2")

# matrix of correlation values
# the 2 tells it to start with col 2 and end with col 4 (so skip the y col)
cor(one[,2:4])

# how to run tests using full and reduced models
# goal: compare y ~ x1 + x2 + x3 to y ~ x1

full <- lm(y ~ x1 + x2 + x3, data=one) # Full Model
reduced <- lm(y ~ x1, data=one) # Reduced model

out <- anova(reduced, full)

# check values
# anova(full)
# anova(reduced)
























