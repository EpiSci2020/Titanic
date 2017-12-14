#library(tidyverse)
library(rethinking)
library(readr)
ti <- read.csv("train.csv")
str(ti)
 #prepare data for Stan
ti_s <- ti[, c(2,3,5,6)]
colnames(ti_s) <- c("survived", "class", "sex", "age")
str(ti_s)
table(ti_s$class)
summary(ti_s)
ti_s1 <- ti_s 
ti_s1$sex <- as.integer(ti_s$sex)
str(ti_s1)
#**********************
m1 <- glm(survived ~ class + age + sex, data = ti_s1)
summary(m1)
#*********************************
#model with no NA's
ti_s2 <- na.omit(ti_s1)
m2 <- map(
  alist(
    survived ~ dbinom(1, p),
    logit(p) <- a + bc*class + bs*sex,
    a ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    bs ~ dnorm(0, 1)
  ),
  data = ti_s2)

m3 <- map(
  alist(
    survived ~ dbinom(1, p),
    logit(p) <- a + bc*class + bs*sex + ba*age,
    a ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    bs ~ dnorm(0, 1),
    ba ~ dnorm(0, 1)
  ),
  data = ti_s2)


    
    
    
    
    
    

      
      
      
      
      
      
      
      
      
      
      
      
      









