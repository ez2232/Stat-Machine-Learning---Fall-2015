# Eric Zhang
# UNI ez2232
# Stat 4400
# HW 2


#--------------------------------------------------------------------
#--------------------------------------------------------------------
#-------------------------------Part 1-------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------

# Classification function

classify <- function(S,z){
	q <- sign(S %*% z)
	return(q)
}

#--------------------------------------------------------------------
#--------------------------------------------------------------------
#-------------------------------Part 2-------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------

library(ggplot2)

# Specify # dimensions for data points

	d <- 2

# Generate random data matrix and classifications
# according to given normal vector and offset constant

	c <- -3
	true_hyperplane <- c(1:d, -c)

random_data <- fakedata(true_hyperplane,50)
	S <- random_data[[1]]
	y <- random_data[[2]]

# Plot of the points by class and the true separating hyperplane
p <- qplot(S[,1],S[,2],color=y) + 
	geom_abline(intercept=c/true_hyperplane[2],slope = -true_hyperplane[1]/true_hyperplane[2])
p

# Learning rate as a function of k, the iteration number

	learn_rt <- function(k){return(1/k)}

# generate initial random guess for normal vector of separating hyperplane

	init <- runif(d+1)
	z <- as.matrix(init/as.numeric(sqrt(t(init)%*%init)))

# initial plot of guess

	p + geom_abline(intercept = -z[3]/z[2], slope = -z[1]/z[2], color = "red")

# classification result of guess

	y_hat <- classify(S,z)

# number of errors

	err_sum <- sum(abs(y_hat - y)/2)
	err_sum

# number of iterations for perceptron algorithm

	trial_num <- 100

# Perceptrain function

perceptrain <- function(S,y,y_init,z_init,err_sum_init,k_max){

	#storing initial guess, error count
	Z_history <- rbind(t(z_init))
	err_sum_new <- err_sum_init
	err_sum_history <- rbind(err_sum_new)

	#initial y_hat
	y_hat_new <- y_init
	y_history <- rbind(t(y_hat_new))

	#initial guess for z
	z_new <- z_init

	#for diagnostics
	grad_c <- t(abs(y-y_hat_new)/2*(-y_hat_new)) %*% S
	grad_c_history <- rbind(grad_c)
	chg <- (1)*grad_c
	chg_history <- rbind(chg)

	#dummy variable, trial vector
	trials <- c(1:k_max)

	#loop gradient descent algorithm
	for (n in trials) {

		#computing the gradient of the cost function
		grad_c <- t(abs(y-y_hat_new)/2*(-y_hat_new)) %*% S

		#updated guess for z
		chg <- (1)*(learn_rt(n) * t(grad_c))
		z_new <- z_new + chg

		#updated classification estimates
		y_hat_new <- classify(S,z_new)

		#updated number of errors
		err_sum_new <- sum(abs(y_hat_new - y)/2)

		#storing results and diagnostics
		grad_c_history <- rbind(grad_c_history, grad_c)
		Z_history <- rbind(Z_history, t(z_new))
		err_sum_history <- rbind(err_sum_history, err_sum_new)
		y_history <- rbind(y_history, t(y_hat_new))
		chg_history <- rbind(chg_history,t(chg))

		#print(grad_c)
		#print(z_new)
		#print(err_sum_new)

		if (err_sum_new < 1) {break}
	}
	
	#return of function
	return(list(z_new,Z_history,err_sum_history, y_history, grad_c_history, chg_history))
}

training_result <- perceptrain(S,y,y_hat,z,err_sum,trial_num)

	z_final <- training_result[[1]]
	z_hist <- training_result[[2]]
	err_sum_hist <- training_result[[3]]
	y_hist <- training_result[[4]]
	grad_c_hist <- training_result[[5]]
	chg_hist <- training_result[[6]] 

# plotting final result
# black is true hyperplane, red is initial guess, blue is result of training

	qplot(S[,1],S[,2],color=y) + 
	geom_abline(intercept = -z_final[3]/z_final[2], slope = -z_final[1]/z_final[2], color = "blue") +
	geom_abline(intercept = -z[3]/z[2], slope = -z[1]/z[2], color = "red") +
	geom_abline(intercept=c/true_hyperplane[2],slope = -true_hyperplane[1]/true_hyperplane[2])

#--------------------------------------------------------------------
#--------------------------------------------------------------------
#-------------------------------Part 3-------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------

# Specify # dimensions for data points

	d <- 2

# generate random normal vector of separating hyperplane

	init <- runif(d+1)
	z_true <- as.matrix(init/as.numeric(sqrt(t(init)%*%init)))

# generate initial random guess for normal vector of separating hyperplane

	init <- runif(d+1)
	z <- as.matrix(init/as.numeric(sqrt(t(init)%*%init)))

# generate 100 training data points, 100 test data points

	training_data <- fakedata(z_true,100)
		S_1 <- training_data[[1]]
		y_1 <- training_data[[2]]

	test_data <- fakedata(z_true,100)
		S_test <- test_data[[1]]
		y_test <- test_data[[2]]

# classification result of guess on training data

	y_hat <- classify(S_1,z)

# number of errors

	err_sum <- sum(abs(y_hat - y)/2)

# number of iterations for perceptron algorithm

	trial_num <- 100

training_result <- perceptrain(S_1,y_1,y_hat,z,err_sum,trial_num)

	z_final <- training_result[[1]]
	z_hist <- training_result[[2]]
	err_sum_hist <- training_result[[3]]
	y_hist <- training_result[[4]]
	grad_c_hist <- training_result[[5]]
	chg_hist <- training_result[[6]] 

# classification result of training on test data

	y_hat_test <- classify(S_test,z_final)

# number of errors made on classifying the test data

	sum(abs(y_hat_test - y_test)/2)


#--------------------------------------------------------------------
#--------------------------------------------------------------------
#-------------------------------Part 4-------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------

# plotting test data against trained classifier

	qplot(S_test[,1],S_test[,2],color=y_test,xlab="x",ylab="y",main="Test Data vs. Trained Perceptron Classifier")+ 
	geom_abline(intercept = -z_final[3]/z_final[2], slope = -z_final[1]/z_final[2], color = "red")

# new plot

	x11()

# plotting training data and trajectory of the training algorithm

	l <- qplot(S_1[,1],S_1[,2],color=y_1,xlab="x",ylab="y",main="Training Data and Trajectory of Perceptron Algorithm")+
	theme(legend.position = "none")
	
	for (n in c(1:nrow(z_hist)))
	{
	  l <- l + geom_abline(intercept = -z_hist[n,3]/z_hist[n,2], slope = -z_hist[n,1]/z_hist[n,2], 
	    color = factor(n))
	}
	
  l
	
