
radf2 <- function(data, minw, lag = 0) {
  
  # class checks
  sim_index <- seq(1, NROW(data), 1)
  if (any(class(data) %in% c("mts", "ts"))) {
    if (all(time(data) == sim_index)) {
      dating <- sim_index
    }else{
      dating <- time(data) %>%
        as.numeric() %>%
        date_decimal()
      if (frequency(data) %in% c(1, 4, 12)) {
        dating <- dating %>%
          round_date("month") %>%
          as.Date()
      }else if (frequency(data) == 52) {
        dating <- dating %>%
          as.Date()
      }else{
        dating <- dating %>%
          round_date("day") %>%
          as.Date()
      }
    }
  } else if (is.data.frame(data)) {
    date_index <- purrr::detect_index(data, lubridate::is.Date)
    if (as.logical(date_index)) {
      dating <- data[, date_index, drop = TRUE]
      data <- data[, -date_index, drop = FALSE]
    } else {
      dating <- sim_index
    }
  } else if (class(data) %in% c("numeric", "matrix")) {
    dating <- sim_index
  } else {
    stop("Unsupported class", call. = FALSE)
  }
  x <- as.matrix(data)
  nc <- NCOL(data)
  nr <- NROW(data)
  # args
  if (is.null(colnames(x))) colnames(x) <- paste("Series", seq(1, nc, 1))
  if (missing(minw)) minw <-  floor((r0 <- 0.01 + 1.8 / sqrt(nr)) * nr)
  # checks
  exuber:::assert_na(data)
  exuber:::assert_positive_int(minw, greater_than = 2)
  exuber:::assert_positive_int(lag, strictly = FALSE)
  
  point <- nr - minw - lag
  
  adf <- sadf <- gsadf <- drop(matrix(0, 1, nc,
                                      dimnames = list(NULL, colnames(x))))
  badf <- bsadf <- matrix(0, nr - 1 - lag, nc, dimnames = list(NULL, colnames(x)))
  
  for (i in 1:nc) {
    
    # yxmat <- exuber:::unroot(, lag = lag)
    results <- rls.gsadf(x[, i], wmin = minw, lags = lag)
    # return(results)

    adf[i]  <- results$adf
    sadf[i] <- results$sadf
    gsadf[i] <- results$gsadf
    # badf[, i] <- results$badf
    bsadf[, i] <- results$bsadf
  }
  
  bsadf_panel <- apply(bsadf, 1, mean)
  gsadf_panel <- max(bsadf_panel)
  
  value <- structure(list(adf = adf,
                          # badf = badf,
                          sadf = sadf,
                          bsadf = bsadf[-c(1:(minw-1)),],
                          gsadf = gsadf,
                          bsadf_panel = bsadf_panel,
                          gsadf_panel = gsadf_panel),
                     index = dating,
                     lag = lag,
                     minw = minw,
                     lag = lag,
                     col_names = colnames(x),
                     class = "radf")
  
  return(value)
}





rls.gsadf <- function(yy,lags,wmin){
  rdata=embed(yy,lags+2);
  y=rdata[,1];
  if(lags==0){
    x=cbind(1,rdata[,-1]);
  }else{
    x=cbind(1,rdata[,2],rdata[,2:(lags+1)]-rdata[,3:(lags+2)]);
  }
  
  start <- wmin;
  end <- length(y);
  total <- end-start+1;
  nc <- NCOL(x); # number of coefficients
  tstat <- matrix(data=-999,nrow=end,ncol=total);
  
  for(j in 1:total){
    for(i in j:total){
      sx <- x[j:(start+i-1),]; # the subset of x
      sy <- y[j:(start+i-1)];
      if(i==j){
        tsx <- t(sx); # transpose of sx
        g <- qr.solve(tsx%*%sx); # GAIN MATRIX
        b <- as.matrix(g%*%tsx%*%sy); # vector of coefficients
        res <- sy-sx%*%b;
        sqres <- sum(res^2) ;
        vares <- sqres/(start+i-j-nc);
        sb <- sqrt(vares*diag(g));
        tstat[wmin+i-1,j] <- (b[2]-1)/sb[2];
      }else{
        tsxn <- as.matrix(x[start+i-1,]);
        syn <- as.matrix(y[start+i-1]);
        sxn <- t(tsxn);
        kaka<-  as.vector( (1+sxn%*%g%*%tsxn)^(-1) , "numeric");
        g <- g - kaka*(g%*%tsxn)%*%(sxn%*%g);
        b <- b-g%*%tsxn%*%(sxn%*%b-syn); # vector of coefficients
        res <- sy-sx%*%b;
        sqres <- sum(res^2) ;
        vares <- sqres/(start+i-j-nc);
        sb <- sqrt(vares*diag(g));
        tstat[wmin+i-1,j] <- (b[2]-1)/sb[2];
      }
    }
  }
  adf=tstat[end,1];
  badf = tstat[,1];
  bsadf=apply(tstat,1,max);
  sadf=max(tstat[,1]);
  gsadf=max(bsadf);
  return(list(sadf=sadf,gsadf=gsadf,bsadf=bsadf,adf=adf))
}