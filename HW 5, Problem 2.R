# Eric Zhang
# UNI ez2232
# Stat 4400
# HW 5


# ------------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------------


theta <- seq(0.01, 4, 0.01)
l <- length(theta)


# ------------------------------------------------------------------------------------
# Part a
# ------------------------------------------------------------------------------------


library(ggplot2)

graph_exp <- exp(-theta)
delta_1 <- exp(-1)*dnorm(theta, mean = 1, sd = 0.003)/dnorm(0, mean = 0, sd = 0.003)
delta_2 <- exp(-2)*dnorm(theta, mean = 2, sd = 0.003)/dnorm(0, mean = 0, sd = 0.003)
delta_3 <- exp(-4)*dnorm(theta, mean = 4, sd = 0.003)/dnorm(0, mean = 0, sd = 0.003)

plot(theta, graph_exp, xlab = "x", ylab="p(x|theta=1)", type = "l",
     ylim = range(graph_exp),
     main = "Plot of p(x|theta=1) on [0,4]", col="red")
par(new = T)
plot(theta, delta_1, axes = F, xlab = "", ylab = "", col = "blue",
     type = "l", ylim = range(graph_exp))
par(new = T)
plot(theta, delta_2, axes = F, xlab = "", ylab = "", col = "blue",
     type = "l", ylim = range(graph_exp))
par(new = T)
plot(theta, delta_3, axes = F, xlab = "", ylab = "", col = "blue",
     type = "l", ylim = range(graph_exp))
par(new = F)

# ------------------------------------------------------------------------------------
# Part g
# ------------------------------------------------------------------------------------

library(reshape2)

# For storing Prior, 256 posterior distributions, alpha_n, beta_n
log_q_n <- mat.or.vec(257, l)
alpha <- mat.or.vec(257, 1)
beta <- mat.or.vec(257, 1)

# Storing alpha_0, beta_0
alpha[1] <- 2
beta[1] <- 0.2

# Log Likelihood of Prior Distribution
log_q_n[1, 1 : l] <- (alpha[1] - 1) * log(theta) + 
  (alpha[1]) * log(beta[1]) - beta[1] * theta -
  lgamma(alpha[1])

# 256 samples from Exp(1)
samples <- qexp(runif(256, min=0, max = 1), 1)

# Online Updating
for (m in 2:257){
  
  # Update parameters
  alpha[m] <- alpha[m - 1] + 1
  beta[m] <- beta[m - 1] + samples[m - 1]
  
  # Update Log Likelihood
  log_q_n[m, 1 : l] <- log_q_n[(m - 1), 1 : l] + log(theta) + 
    alpha[m] * log(beta[m]) -
    alpha[m - 1] * log(beta[m - 1]) -
    samples[m - 1] * theta + 
    lgamma(alpha[m - 1]) - lgamma(alpha[m])
}

# Final q_n values
q_n <- exp(log_q_n)

# Plotting curves

# Creating Data Frame for plotting
plot_frame <- melt(q_n[c(5, 9, 17, 257), 1:l])
names(plot_frame)[names(plot_frame) == "Var1"] <- "n_Points"
names(plot_frame)[names(plot_frame) == "value"] <- "Density"

# Appending Values of theta
plot_frame <- plot_frame[order(plot_frame[, 1]), ]
plot_frame <- cbind(plot_frame, rep(theta, 4))
names(plot_frame)[names(plot_frame) == "Var2"] <- "i"
names(plot_frame)[names(plot_frame) == "rep(theta, 4)"] <- "Theta"

plot_frame[, 1] <- (plot_frame[, 1] == 1) * 4 + (plot_frame[, 1] == 2) * 8 +
      (plot_frame[, 1] == 3) * 16 + (plot_frame[, 1] == 4) * 256

#str(plot_frame)


graph <- ggplot(data = plot_frame, aes(x = Theta, y = Density, group = n_Points,
      color = factor(n_Points)))
graph + geom_line(size = 1) + scale_color_manual(values = c("red", "blue", "violet", "orange"),
      name = "Number of Points") + ggtitle("Plots of the Posterior Distributions by # of Included Data Points")
