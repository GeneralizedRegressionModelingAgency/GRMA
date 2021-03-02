xs <- rnorm(30)
z <- 3*xs
p <-  1/(1+exp(-z)) 
ys = rbinom(30,1,p )

data <- data.frame(x = xs, y = ys)
write.csv(data, here::here("learnR_data","data_ex1.csv"))
