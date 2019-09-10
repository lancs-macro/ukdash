hp <- 2.0
if (hp > 0) {
  hp_string <- paste0("grew at ", hp, "%")
} else{
  hp_string <- paste0("fell at ", hp, "%")
}

library(lubridate)

Y <- year(Sys.Date())
Q <- quarter(Sys.Date())

# 
# cat("House Price Index\n")
# 
# cat("Current status", paste0("(",Y,"Q",Q, "):\n"))
# 
# cat("The regional house price index", hp_string, "over the last 12 months.\n")
# 
# writeLines("Exuberance: Note  in  the  graph  below  that  the  statistic  (BSADF)  employed  to  determine whether 
# real  house  prices  are  in  an  exuberant  phase  is  below  the  explosive  threshold,  
# i.e. no exuberance.Probability  of  exuberance:The  probability  of  entering  in  an  exuberant  phase within  
# the  next quarteris   0.5%   (see avlidis   E.G.   et   al.,   2016 for   the   description   of   the  
# methodology and Yusupova et al., 2017 for the data details).")
# 
# cat("Price-to-Income Ratio")
# 
# 
# "Current status (2018Q3):"
# 
# cat("The regional real house price to real personal disposable income ratio is currently at 3.39.")
# 
# 
# cat("Exuberance:Note in the graph below  that the statistic (BSADF) employed to determine whether the ratio of Real House Price to Real 
# Personal Disposable Income is in an exuberantphase is below the explosive threshold, i.e. no 
# exuberance.Probability  of  exuberance:The  probability  of  entering  in  an  exuberant  phase  
# within  the  next quarteris   1.3%   (seePavlidis   E.G.   et   al.,   2016for   the   description   
#                                       of   the   methodology andYusupova et al., 2017 for the data details).")
