
# DMA ---------------------------------------------------------------------

library(eDMA)

# vDelta forgetting factor this is lambda in KK
# alpha also the forgeting factor
# vkeep keep constant and AR1 lag always
# vBeta another
DMA(HP ~ LAG(HP,1) + LAG(explanatory, 1), data = ., 
    vDelta = 0.95, dAlpha = 0.95, vKeep = c(1,2))
DMA(HP ~ LAG(explanatory, 1), data = ., data = ., 
    vDelta = 0.95, dAlpha = 0.95, vKeep = c(1,2))



# BVAR --------------------------------------------------------------------

# install.packages("remotes")
# remotes::install_github("kthohr/BMR")

library(BMR)

data(BMRVARData)
bvar_data <- data.matrix(USMacroData[,2:4])

coef_prior <- c(0.9,0.9,0.9)
bvar_obj <- new(bvarm)

bvar_obj$build(bvar_data,TRUE,4)
bvar_obj$prior(coef_prior)
bvar_obj$gibbs(10000)

x <- BMR::forecast(bvar_obj, shocks = TRUE,
                   var_names = colnames(bvar_data),
                   back_data = 10, save = FALSE)

# Onyl regress with regional house prices

# ARDL --------------------------------------------------------------------

# install.packages("dLagM")
library(dLagM)

data(warming)

data(M1Germany)
data = M1Germany[1:144,]
finiteDLMauto(logprice ~ interest + logm1,data = data.frame(data), 
              y = NULL, x = NULL, k.order = 8, model.type = "dlm", 
              error.type = "BIC", trace = TRUE)

data(warming)
model.ardl = ardlDlm(x = warming$NoMotorVehicles,y = warming$Warming, p = 1 , q = 1 )
forecast(model = model.ardl , x = c(95, 98), h = 2 , interval = FALSE)

# hp ~ 1 + lag(hp) + explanatory per time
# use lag with criteria of both hp and explenatory BIC

# TVP-AR ------------------------------------------------------------------

# hp ~ 1 + lag(hp,1) + all predictors for each region

