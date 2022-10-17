build3dprofilehex <- function(x, data) {
  
  f <- fetchOSD(x)
  # f <- fetchOSD("Marshall")
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
    
    mtrd <- c(0, h$top[i], -55, 50,
              -40, h$top[i], -25, 50,
              -40, h$top[i], 25, 50,
              0, h$top[i], 55, 50,
              40, h$top[i], 25, 50,
              40, h$top[i], -25, 50,
              0, h$bottom[i], -55, 50,
              -40, h$bottom[i], -25, 50,
              -40, h$bottom[i], 25, 50,
              0, h$bottom[i], 55, 50,
              40, h$bottom[i], 25, 50,
              40, h$bottom[i], -25, 50
              )
    
    dps[i] <- list(matrix(data = mtrd, nrow = 4, ncol = 12))
    
    fpc[i] <- list(extrude3d(c(0, -40, -40, 0, 40, 40), c(-55, -25, 25, 55, 25, -25)))
    
    fpc[[i]]$vb <- dps[[i]]
    
    fpc[[i]]$material$color <- cl[i]
    
    cnt1 <- 0
    
    cnt2 <- 0
    
    fpc[[i]] <- rotate3d(obj = fpc[[i]], pi, 1, 0, 0)
    fpc[[i]] <- rotate3d(obj = fpc[[i]], pi/2, 0, 1, 0)
    
    fpc[[i]] <- scale3d(obj = fpc[[i]], 50,100,50)
    
    fpc[[i]] <- translate3d(obj = fpc[[i]], x = cnt1, y = (data@data@max)+500, z = cnt2)
    # fpc[[i]] <- translate3d(obj = fpc[[i]], x = cnt1, y = 500, z = cnt2)
    
    
    fnl <- shade3d(fpc[[i]])
    
  }
  
  
  
  return(fnl)
}
