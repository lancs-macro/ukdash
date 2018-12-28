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
  bsadf=apply(tstat,1,max);
  sadf=max(tstat[,1]);
  gsadf=max(bsadf);
  return(list(sadf=sadf,gsadf=gsadf,bsadf=bsadf,adf=adf))
}


## This function is faster and can be used for generating critical values
## SADF and GSADF testing regression: y on intercept and lag y 
## yy: level of series; xx: lag of series; wins: minimum window size
gsadf_slr=function(yy,xx,wins){
  N=length(yy);
  repet=N-wins+1;
  #tstat_M=-999;
  tstat_M = matrix(data=-999,nrow=N,ncol=repet);
  for(j in 1:repet){
    for(i in j:repet){
      x=xx[j:(wins+i-1)];
      y=yy[j:(wins+i-1)];
      T=wins+i-j;
      #end=wins+i-1;
      if(i==j){
        Sx=sum(x);
        Sy=sum(y);
        Sxx=sum(x*x);
        Sxy=sum(x*y);
      }else{
        Sx=Sx+x[T];
        Sy=Sy+y[T];
        Sxx=Sxx+x[T]*x[T];
        Sxy=Sxy+x[T]*y[T];
      }
      meanx=Sx/T;
      meany=Sy/T;
      den=Sxx/T-meanx*meanx;
      beta=(Sxy/T-meanx*meany)/den;
      alpha=meany-beta*meanx;
      u=y-alpha-beta*x;
      Suu=sum(u*u);
      sbeta=sqrt(Suu/(T-2)/den/T);
      tstat_M[wins+i-1,j]=(beta-1)/sbeta;
    }
  }
  bsadf=apply(tstat_M,1,max);
  badf=tstat_M[,1];
  sadf=max(badf);
  gsadf=max(bsadf);
  return(list(sadf=sadf,gsadf=gsadf,bsadf=bsadf,badf=badf,tstat_M=tstat_M))
}




