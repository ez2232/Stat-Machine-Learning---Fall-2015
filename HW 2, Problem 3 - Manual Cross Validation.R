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
	
	#number k for cross-validation
	k <- 10
	
	#size of each cross-validation bin
	bin_size <- nrow(train_S)/k
	
	#splitting training data in k random groups
	remain <- c(1:(k*bin_size))
	part <- sample(remain, bin_size, replace = FALSE)
	cross_valid_indices <- part
	remain <- setdiff(c(1:(k*bin_size)), part)
	
	for (n in c(1:(k-2))){
	  part <- sample(remain, bin_size, replace = FALSE)
	  cross_valid_indices <- c(cross_valid_indices, part)
	  remain <- setdiff(remain, part)
	}
	
	cross_valid_indices <- c(cross_valid_indices, remain)
	
	#vector of cost parameter choices
	margin_param <- 2^(-15:15)
	
	#matrix to store error results for each choice of cost parameter and validation block
	err_store <- mat.or.vec(length(margin_param), k+1)

	#tuning svm by cross validation on cost parameter
	
	for (m in c(1:length(margin_param))){
	  
	  #storing the value of the margin parameter
	  err_store[m,1] <- margin_param[m]
	  
	  for (p in c(1:k)){
	    
	    #determining the indices of the validation and training datasets
	    valid_indices <- cross_valid_indices[c((bin_size*(p-1)+1):(bin_size*p))]
	    train_indices <- setdiff(cross_valid_indices, valid_indices)
	    
	    #training the svm
	    model <- svm(train_S[train_indices,], train_y[train_indices], kernel = "linear"
	                 , cost = margin_param[m], type = "C-classification")
	    
	    #applying the trained svm to the validation data
	    prediction <- predict(model, train_S[valid_indices,])
	    
	    #results of the prediction versus actual
	    result <- table(prediction,train_y[valid_indices])
	    
	    #storing the count of errors
	    err_store[m, p+1] <- result[1,2] + result[2,1]
	    
	    #clearing the model
	    rm(result)
	    rm(prediction)
	    rm(model)
	  }
	  
	}
	
	#the overall error rate for each margin parameter value
	table_1 <- cbind(err_store[,1], rowSums(err_store[,2:(k+1)])/(k*bin_size))

		    		
# training SVM with soft margin and RBF kernel, cross-validating to determine
# the soft margin parameter and kernel bandwidth
	
	  #vector of cost parameter choices
	  margin_param_2 <- 5^(-5:5)

	  #vector of kernel bandwidth choices
	  band_param <- 5^(-5:5)
	
	  #matrix to store error results for each choice of cost parameter and validation block
	  err_store_2 <- mat.or.vec(length(margin_param_2)*length(band_param), k+2)
		
	  #tuning svm by cross validation on cost parameter
	
	  for (m in c(1:length(margin_param_2))){
	    
	    for (q in c(1:length(band_param))){
	      
	      #storing the values of the parameters
	      err_store_2[((m-1)*length(band_param)+q),1] <- margin_param_2[m]
	      err_store_2[((m-1)*length(band_param)+q),2] <- band_param[q]
	    
	      for (p in c(1:k)){
	    
	        #determining the indices of the validation and training datasets
	        valid_indices <- cross_valid_indices[c((bin_size*(p-1)+1):(bin_size*p))]
	        train_indices <- setdiff(cross_valid_indices, valid_indices)
	        
	        #training the svm
	        model <- svm(train_S[train_indices,], train_y[train_indices], kernel = "radial"
	                 , cost = margin_param_2[m], gamma = band_param[q], type = "C-classification")
	        
	        #applying the trained svm to the validation data
	        prediction <- predict(model, train_S[valid_indices,])
	        
	        #results of the prediction versus actual
	        result <- table(prediction,train_y[valid_indices])
	    
	        #storing the count of errors
	        err_store_2[((m-1)*length(band_param)+q), p+2] <- result[1,2] + result[2,1]
	        
	        #clearing the model
	        rm(result)
	        rm(prediction)
	        rm(model)
	      }
	    }
	  }
	  
	  #the overall error rate for each combination of parameter values
	  table_2 <- cbind(err_store_2[,1:2], rowSums(err_store_2[,3:(k+2)])/(k*bin_size))


#--------------------------------------------------------------------
#--------------------------------------------------------------------
#-----------------------------Question 1-----------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------

	#part (a)
	#plotting the error rate as a function of margin parameter

		qplot(table_1[,1], table_1[,2], xlab="margin tradeoff parameter", ylab="error rate",
		    main="Average Error Rate as a Function of Margin Tradeoff Parameter",
			  geom = c("point", "smooth")) + 
			  scale_x_log10()
	  
	#part (b)
	#plotting the error rate as a function of margin and bandwidth parameters
	  
	  library(scatterplot3d)
	  
	  x11()

	  scatterplot3d(table_2[,1], table_2[,2], table_2[,3], 
	       xlab = "margin parameter", ylab = "bandwidth parameter", zlab = "error rate",
	       color = "blue", main= "Average Error Rate as a Function of Margin and Bandwidth Parameters")

#--------------------------------------------------------------------
#--------------------------------------------------------------------
#-----------------------------Question 2-----------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------

	  #choice of optimal margin parameter from Question 1, part (a)
	  
	    m_opt <- table_1[6,1]
	  
	    #training with optimal parameter on entire training dataset
	  
  	    model_1 <- svm(train_S, train_y, kernel = "linear"
	        , cost = m_opt, type = "C-classification")
	  
	    #applying the trained svm to the test data
	  
  	    prediction_1 <- predict(model_1, test_S)
	    
	    #results of the prediction versus actual
  	    
	      result_1 <- table(prediction_1, test_y)
	    
	    #storing the error rate on the test data
	      
	      err_rate_1 <- (result_1[1,2] + result_1[2,1])/length(test_y)
	  
	  #choices of optimal parameters from Question 1, part (b)
	  #for this model, I found that rows 69, 80, 91, 102, 113 all gave the lowest error,
	  #so I picked row 69.

	      m_opt2 <- table_2[69,1]
	      b_opt <- table_2[69,2]
	  
	      #training with optimal parameters on entire training dataset
	  
	        model_2 <- svm(train_S, train_y, kernel = "radial"
	          , cost = m_opt2, gamma = b_opt, type = "C-classification")
	        
	      #applying the trained svm to the test data
	  
  	      prediction_2 <- predict(model_2, test_S)
	    
	      #results of the prediction versus actual
  	    
  	      result_2 <- table(prediction_2, test_y)
	    
	      #storing the error rate on the test data
	      
  	      err_rate_2 <- (result_2[1,2] + result_2[2,1])/length(test_y)
  	      
  	 #compare error rate of linear model to error rate of radial model
  	 #if value below > 0, radial model has smaller error rate on test data, else 
  	 #if value below < 0, linear model performed better on the test data
  	      
  	      err_rate_1 - err_rate_2
