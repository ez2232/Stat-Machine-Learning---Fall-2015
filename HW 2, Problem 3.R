# Eric Zhang
# UNI ez2232
# Stat 4400
# HW 2

#--------------------------------------------------------------------
#--------------------------------------------------------------------
#-------------------------------Setup--------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------

library(ggplot2)
library("e1071")

setwd("C:/Users/Jack/Documents/R")

# loading the csv data into R

	y_orig <- read.csv(file="uspscl.txt",head=FALSE)
	S_orig <- read.csv(file="uspsdata.txt",head=FALSE,sep="	")

# randomly setting aside 20% of data (40 rows of S) as test set

	rand_samp <- sample(c(1:nrow(S_orig)), nrow(S_orig)/5, replace = FALSE)
	remaining <- setdiff(c(1:nrow(S_orig)), rand_samp)

	test_S <- S_orig[rand_samp,]
	test_y <- y_orig[rand_samp,]

# remaining 80% of the data as training data

	train_S <- S_orig[remaining,]
	train_y <- y_orig[remaining,]

# training linear SVM with soft margin, cross-validating to determine
# the soft margin parameter

	#tuning svm by cross validation on cost parameter
	#storing results

		obj <- tune.svm(train_S, train_y, kernel = "linear",
			cost = 2^(-5:5), cross=5)
		    error_rates <- obj$performances

		    		
# training SVM with soft margin and RBF kernel, cross-validating to determine
# the soft margin parameter and kernel bandwidth
		
		#tuning svm by cross validation on cost and bandwidth parameters
		
		  obj2 <- tune.svm(train_S, train_y, kernel = "radial",
		    cost = 2^(-5:5), cross=5)
		
		#error rates varying by the parameters
		
		  error_rates2 <- obj2$performances


#--------------------------------------------------------------------
#--------------------------------------------------------------------
#-------------------------------Part 1-------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------

	#plotting the error rate as a function of gamma

		qplot(gamma_list, rowSums(err_store), xlab="gamma", ylab="error count",
			geom = c("point", "smooth")) + 
			scale_x_log10()


#--------------------------------------------------------------------
#--------------------------------------------------------------------
#-------------------------------Part 2-------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------


