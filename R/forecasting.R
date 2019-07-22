
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

# Onyl regress with regional house prices

# ARDL --------------------------------------------------------------------

# hp ~ 1 + lag(hp) + explanatory per time
# use lag with criteria of both hp and explenatory BIC

# TVP-AR ------------------------------------------------------------------

# hp ~ 1 + lag(hp,1) + all predictors for each region

