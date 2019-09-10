source('do-not-run/phillips_sadf_gsadf.R')


n <- NROW(rhpi)
mw <- floor((0.01 + 1.8 / sqrt(n)) * n)
R <- 2000
bsadfc <- bsadfd <- matrix(NA, n, R)
gsadfc  <-  vector()
for (i in 1:R) {
  print(i)
  y <- cumsum(rnorm(n + 1))
  aux <- gsadf_slr(yy = y[-1], xx = y[-(n + 1)], wins = mw)
  bsadfc[, i] <- cummax(aux$tstat_M[, 1])
  gsadfc[i] <- aux$gsadf
}
# 95% GSADF critical value
gsadf95 <- quantile(gsadfc, .95)
print(paste("GSADF 95% Critical Value:", gsadf95, sep = " "))
# 95% BSADF critical value sequence
bsadf95 <- apply(bsadfc, 1, quantile, .95)
plot.ts(bsadf95[bsadf95 != -999], ylim = c(-.5, 3))


cv_custom <- 
  structure(
    list(
      gsadf_cv = quantile(gsadfc, c(.9, .95, .99)),
      bsadf_cv = apply(bsadfc, 1, quantile, c(.9, .95, .99)) %>% 
        t() %>% 
        "["(-c(1:(mw)), )
    ),
    method = "Monte Carlo",
    iter = "1000",
    minw = mw,
    class = "cv",
    opt_badf = "fixed"
  )

saveRDS(cv_custom, file = "do-not-run/cv_custom.Rds")


# Compare bsadf-stats -----------------------------------------------------


