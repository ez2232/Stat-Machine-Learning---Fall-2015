# Eric Zhang
# UNI ez2232
# Stat 4400
# HW 3



#--------------------------------------------------------------------
#--------------------------------------------------------------------
#----------------------------Parts 1, 2, 3---------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#---------------Setup: Writing Functions, Loading Data --------------
#--------------------------------------------------------------------

setwd("C:/Users/Jack/Documents/R")


# loading the csv data into R

	y_data <- t(read.csv(file="uspscl.txt",head=FALSE)) #1 x 200
	S_data <- t(read.csv(file="uspsdata.txt",head=FALSE,sep="	")) #265 x 200
	
# number of data points
	
	n_pts <- length(y_data)
	
# the classification routine for a decision stump
  
  classify <- function(data, pars_input){
    
    j <- pars_input[1,1]
    theta <- pars_input[1,2]
    m <- pars_input[1,3]
    
    label <- data[j, ]
    label <- t(as.matrix(((label > theta)*2-1)*m)) #apply classification rule
    
    #output is row vector of classification labels
    return(label)
  }	

# the weak learner training routine
  
  train <- function(S_train, w_train, y_train){
    
    #number of data points
    n_pts <- ncol(S_train)
    
    #number of dimensions
    n_dim <- nrow(S_train)
    
    #saves, for each dimension, the optimal threshold value, orientation
    #as well as the weighted score for this choice of classifier
    best_store <- mat.or.vec(n_dim, 3)
  
    for (n in 1:n_dim){
      
      #sorting the jth dimension of each data point in asc order
      sort_result <- sort(S_train[n, ], decreasing = F, index.return = T)
      
      #sorted values along the jth dimension
      num_line <- sort_result[[1]]
      
      #the permutation vector
      sort_indices <- sort_result[[2]]
      
      #sorting y and w according to the permutation vector
      y_sort <- y_train[sort_indices]
      w_sort <- w_train[sort_indices]
      
      #left_store keeps track of the total score (# falling below the 
      #threshold that were classified correctly), weighted by w
      #at each of the possible threshold locations
      #similar for right_store, except it looks that points above
      #the threshold that were classified correctly
      #each matrix has two rows because the points above the threshold
      #can be classified as either +1 or -1
      left_store_sum <- mat.or.vec(2,(n_pts+1))
      right_store_sum <- mat.or.vec(2,(n_pts+1))
      
      #to the left of all the points,there are no under-threshold
      #points
      left_store_sum[1,1] <- 0
      left_store_sum[2,1] <- 0
      
      #to the right of all the points,there are no over-threshold
      #points
      right_store_sum[1,(n_pts+1)] <- 0
      right_store_sum[2,(n_pts+1)] <- 0
      
      #stores the locations of the possible threshold points
      threshold_pts <- mat.or.vec(1,(n_pts+1))
      
      #setting the threshold points
      threshold_pts[1,1] <- num_line[1] - 1
      threshold_pts[1,(n_pts+1)] <- num_line[n_pts] + 1 
      threshold_pts[1,2:n_pts] <- (num_line[2:n_pts] + num_line[1:(n_pts-1)])/2
      
      #scanning from left to right and right to left, adding up the weights of the points
      #that were correctly classified
      for (m in 1:n_pts){
        left_store_sum[1,m+1] = left_store_sum[1,m] + w_sort[m] * (y_sort[m] == 1)
        left_store_sum[2,m+1] = left_store_sum[1,m] + w_sort[m] * (y_sort[m] == -1)
        right_store_sum[1,(n_pts+1-m)] = right_store_sum[1,(n_pts+2-m)] 
          + w_sort[(n_pts+1-m)] * (y_sort[(n_pts+1-m)] == 1)
        right_store_sum[2,(n_pts+1-m)] = right_store_sum[1,(n_pts+2-m)] 
          + w_sort[(n_pts+1-m)] * (y_sort[(n_pts+1-m)] == -1)
      }
      
      #list of scores for each threshold point for the positive and negative orientations
      pos_orient_scores <- left_store_sum[2,] + right_store_sum[1,]
      neg_orient_scores <- left_store_sum[1,] + right_store_sum[2,]
      
      #best weighted score among threshold point/orientation combinations
      best_score <- max(c(pos_orient_scores,neg_orient_scores))
      best_store[n, 3] <- best_score
      
      #which orientation and threshold point(s) gave the best weighted score
      pos_ind <- 1*(pos_orient_scores == best_score)
      neg_ind <- 1*(neg_orient_scores == best_score)
      
      #the list of optimal threshold points and their orientation (encoded in sign)
      opt_list <- threshold_pts * (pos_ind - neg_ind)
      
      #case analysis of # of optimal points
      if (sum(opt_list != 0) == 1){
        best_store[n, 1] <- sum(opt_list) #optimal threshold location (incl orientation)
      }
      else {
        #picks random optimal threshold point
        best_store[n, 1] <- sample(opt_list[opt_list != 0], 1)
      }
      
      best_store[n, 2] <- sign(best_store[n, 1]) #optimal orientation
      best_store[n, 1] <- abs(best_store[n, 1]) #remove orientation from threshold location info
      
    }
    
    #picking out the rows # that had the best weighted score
    all_rows <- c(1:n_dim)
    opt_rows <- all_rows[best_store[,3] == max(best_store[,3])]
    
    #picking out a random optimal row
    pick_opt <- sample(opt_rows, 1)
    
    #output: vector - (j, theta, m)
    pars <- t(as.matrix(c(pick_opt, best_store[pick_opt,1:2])))
    
    return(pars)
  }


# evaluates the boosting classifier on X
  
	agg_class <- function(X_in, alpha_in, allPars_in){
	  
	  #number of classifiers
	  
	  B <- length(alpha_in)
	  
	  #number of data points
	  
	  n_pts_1 <- ncol(X_in)
	  
	  #vector storing the result for each classifier over points
	  
	  result_class <- mat.or.vec(B, n_pts_1)
	  
	  #computing the result for each classifier
	  
	  for (r in 1:B){
	    
	    result_class[r, ] <- classify(X_in, t(as.matrix(allPars_in[r, ])))
	    
	  }
	  
	  #boosting classifier result, row vector
	  
	  c_hat <- sign(t(alpha_in) %*% result_class)
	  
	  return(c_hat)
	}
	
	
# the AdaBoost Algorithm
	
	Adaboost <- function(S_train, B, y_train){
	  
	  # the number of dimensions in each data point
	
	  n_dim <- nrow(S_train)
	  
	  # the number of data points
	  
	  n_pts <- ncol(S_train)
	  
	  # initial vector of weights, as row vector
	
	  w <- (mat.or.vec(n_pts,1)+1)
	  w <- t(as.matrix(w/length(w)))
	  
	  # vector for storing the alphas for each classifier
	  
	  alpha <- as.matrix(mat.or.vec(B, 1))
	  
	  # matrix for storing the parameters of all weak learners
	  
	  allPars <- mat.or.vec(B, 3)
	  
	  # looping over all classifier numbers
	  
	  for (t in 1:B){
	    
	    #training the weak learner on the weighted training data
	    
	    pars <- train(S_train, w, y_train)
	    
	    #run the classification routine
	    
	    label <- classify(S_train, pars)
	    
	    #compute normalized weighted error
	    
	    err <- (sum(abs(label - y_train)/2 * w))/sum(w)

	    #compute voting weight for classifier n
	    
	    a <- log((1-err)/err, base = exp(1))
	    
	    #recompute weights
	    
	    w <- w * exp(a * abs(label - y_train)/2)
	    
	    #store voting weight, parameters for classifier n
	    
	    alpha[t, 1] <- a
	    allPars[t, ] <- pars
	  }
	  
	  return(list(alpha, allPars))
	}
	
# cross-validation on the Adaboost algorithm
	
	Cross_valid <- function(S_data, y_data, B_Max){
	  
	  # number of data points
	
	  n_pts <- length(y_data)
	
    # random determination of 5 blocks for cross-validation
	  
	  remaining <- (1:n_pts)
	  samp <- sample(remaining, n_pts/5, replace = F)
	  blocks <- samp
	  
	  for (s in 1:4){
	    remaining <- setdiff(remaining, samp)
	    samp <- sample(remaining, n_pts/5, replace = F)
	    blocks <- c(blocks, samp)
	  }
	  
	  # matrix for storing "training" and "test" errors over all cross-validation iterations
	  # each row corresponds to a different number of weak classifiers specified to Adaboost
	  # first 5 columns are for "training" error, last 5 columns are for "test" error
	  
	  err_storage <- mat.or.vec(B_Max, 10)
	  
	  # looping over number of weak classifiers
	  
	  for (B in 1:B_Max){
	    
	    # looping over the 5 choices of validation blocks
	    
	    for (k in 1:5){
	      
	      #indices for training and validation data
	      
	      valid_indices <- blocks[((k-1)*n_pts/5+1):(k*n_pts/5)]
	      train_indices <- setdiff(1:n_pts, valid_indices)
	      
	      #validation data
	      
	      S_valid <- S_data[, valid_indices]
	      y_valid <- t((y_data[, valid_indices]))
	      
	      #training data
	      
	      S_train <- S_data[, train_indices]
	      y_train <- t(y_data[, train_indices])
	      
	      #run Adaboost on training data
	      
	      Ada_result <- Adaboost(S_train, B, y_train)
	      
	      alpha <- Ada_result[[1]]
	      allPars <- Ada_result[[2]]
	      
	      #run resulting classifier on "training" and "test" data
	      
	      valid_class <- agg_class(S_valid, alpha, allPars)
	      train_class <- agg_class(S_train, alpha, allPars)
	      
	      #num errors on "training" and "test" data
	      
	      num_err_valid <- sum(abs(y_valid - valid_class)/2)
	      num_err_train <- sum(abs(y_train - train_class)/2)
	      
	      #storing error counts
	      
	      err_storage[B, k] <- num_err_train
	      err_storage[B, k+5] <- num_err_valid
	    }
	  }
	  
	  return(err_storage)
	}

#--------------------------------------------------------------------
#-------------------------------Part 4-------------------------------
#--------------------------------------------------------------------

	# number of weaker learners for AdaBoost
	
	B_Max <- 20
	
	# run Cross-validated Adaboost on USPS data
	
	err_result <- Cross_valid(S_data, y_data, B_Max)
	
	# compute average "training" and "test" error for each value of B
	
	train_test_err <- mat.or.vec(B_Max,2)
	
	for (n in 1:B_Max){
	  train_test_err[n, 1] <- sum(err_result[n,1:5])/(4*n_pts)
	  train_test_err[n, 2] <- sum(err_result[n,6:10])/n_pts
	}

	# plot training and test error as a function of b, the number of weak learners
	
	plot(1:B_Max, train_test_err[ ,2], xlab = "number of weak learners (b)", ylab = "classification error rate",
	  main="Training (blue) and Test Error (red) Rates vs. # of Decision Stump Learners",
	  col = "red", xlim = c(1,10), ylim = c(0,0.6))
	points(1:B_Max, train_test_err[ ,1], col = "blue")
  
  