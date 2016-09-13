# Eric Zhang
# UNI ez2232
# Stat 4400
# HW 4



#---------------------------------------------------------
#--------------------------Setup--------------------------
#---------------------------------------------------------


setwd("C:/Users/Jack/Documents/R")


# loading the data into R

	H <- matrix(readBin("histograms.bin", "double", 640000), 
	            40000, 16)
	
  
#---------------------------------------------------------
#--------------------------Part 1-------------------------
#---------------------------------------------------------
	
# the EM algorithm for a multinomial finite mixture
  
  Multinomial_EM <- function(H, K, tau){
    
    #number of histogram bins
    bin_n <- ncol(H)
    
    #number of data points
    n_pts <- nrow(H)
    
    #matrix of centroids
    centroid <- mat.or.vec(K, bin_n)
    
    #initializing centroids
    for (m in 1:K){
      hist_1 <- runif(bin_n, min = 0, max = 1)
      centroid[m, ] <- hist_1 / sum(hist_1)
    }
    
    #assignment probabilities
    a <- mat.or.vec(n_pts, K)
    
    #adding small constant to empty bins in input
    H <- H + 0.01 * (H==0)
    
    #initial measure of change of assignments
    assign_delta <- 99999
    
    #initial mixture weights
    c <- runif(K, min = 0, max = 1)
    c <- c / sum(c)
    
    #interating over E/M steps
    while(assign_delta > tau){
      
      #saving old a
      a_old <- a
      
      #E-step
      phi <- exp(H %*% log(t(centroid), base = exp(1)))
      a <- (t(t(phi) * c)) / rowSums(t(t(phi) * c))
      
      #M-step
      c <- colSums(a) / n_pts
      b <- t(a) %*% H
      centroid <- b / rowSums(b)
      
      #measure of change of assignments
      if(sum(abs(a_old)) > 0){
        assign_delta <- norm(a - a_old, 'o')
      }
    }
    
    #hard assignments
    m <- max.col(a)
    
    #return
    return(m)
    
  }
  
	
#---------------------------------------------------------
#--------------------------Part 2-------------------------
#---------------------------------------------------------
	
	m_k_3 <- Multinomial_EM(H, 3, 0.01)
	m_k_4 <- Multinomial_EM(H, 4, 0.01)
	m_k_5 <- Multinomial_EM(H, 5, 0.01)
	
	
	m_plot_3 <- matrix(m_k_3, nrow = 200, ncol = 200, byrow = T)
	m_plot_4 <- matrix(m_k_4, nrow = 200, ncol = 200, byrow = T)
	m_plot_5 <- matrix(m_k_5, nrow = 200, ncol = 200, byrow = T)
	
	
	test <- t(apply(m_plot_4, 2, rev))
	
#---------------------------------------------------------
#--------------------------Part 3-------------------------
#---------------------------------------------------------

	x11()
	image(seq(4, 800, by = 4), seq(4, 800, by = 4),
	      t(apply(m_plot_3, 2, rev)), 
	      xlab = "", ylab = "",
	      main = "Segmentation of Image for K = 3, tau = 0.01",
	      col = gray.colors(10))
	x11()
  image(seq(4, 800, by = 4), seq(4, 800, by = 4), 
        t(apply(m_plot_4, 2, rev)),
        xlab = "", ylab = "",
        main = "Segmentation of Image for K = 4, tau = 0.01",
        col = gray.colors(10))
  x11()
  image(seq(4, 800, by = 4), seq(4, 800, by = 4), 
        t(apply(m_plot_5, 2, rev)),
        xlab = "", ylab = "",
        main = "Segmentation of Image for K = 5, tau = 0.01",
        col = gray.colors(10))
  
  