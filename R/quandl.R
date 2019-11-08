library(Quandl)
Quandl.api_key("sMcvTx8cd-zzewgSdERq") 

Quandl(c("UKONS/NRJR_Q", "BOE/IUMTLMV"), collapse = "quarterly", 
       start_date = "2016-01-01")




