build3dprofile <- function(x, data) {
  
  f <- fetchOSD(x)
  h <- horizons(f)
  
  h$thk <- (h$bottom - h$top)/100
  
  n <- length(h$hzID)
  
  fpc <- list()
  
  cl <- c()
  
  dps <- list()
  fnl <- list()
  
  mtrx <- matrix(nrow = 4, ncol = 8)
  
  mtrd <- c()
  
  for (i in 1:n) {
    
    cl[i] <- c(h$soil_color[i])
    
    mtrd <- c(-50, h$top[i], -50, 50, 50, h$top[i], -50, 50, -50, h$bottom[i], -50, 50, 50, h$bottom[i], -50, 50, -50, h$top[i], 50, 50, 50, h$top[i], 50, 50, -50, h$bottom[i], 50, 50, 50, h$bottom[i], 50, 50)
    
    dps[i] <- list(matrix(data = mtrd, nrow = 4, ncol = 8))
    
    fpc[i] <- list(cube3d(color = cl[i]))
    
    fpc[[i]]$vb <- dps[[i]]
    
    cnt1 <- 0
    
    cnt2 <- 0
    
    fpc[[i]] <- rotate3d(obj = fpc[[i]], pi, 1, 0, 0)
    
    fpc[[i]] <- scale3d(obj = fpc[[i]], 50,100,50)
    
    fpc[[i]] <- translate3d(obj = fpc[[i]], x = cnt1, y = (data@data@max)+500, z = cnt2)
    
    
    
    fnl <- shade3d(fpc[[i]])
    
  }
  
  
  
  return(fnl)
}