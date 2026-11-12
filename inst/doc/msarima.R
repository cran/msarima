## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 4
)

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(msarima)

## ----data, fig.alt = "Line chart of vehicles per hour over the first four weeks. A daily cycle of two peaks and a night trough repeats, with lower peaks on the last two days of every week."----
head(MAD_m40, 12)

plot(window(MAD_m40, end = 4), ylab = "vehicles per hour",
     main = "M-40, first four weeks (time in weeks)")

## ----acf, fig.height = 3.5, fig.alt = "Correlogram up to lag 200. The autocorrelations stay well above the confidence band and peak at the multiples of 24, in orange, and at 168, in blue, decaying very slowly."----
sacf(MAD_m40, lag.max = 200)

## ----pacf, fig.height = 3.5, fig.alt = "Partial correlogram up to lag 200. Only the first lags and those around the multiples of 24 and 168 stand out of the confidence band."----
spacf(MAD_m40, lag.max = 200)

## ----ggacf, fig.height = 3.5, eval = requireNamespace("ggplot2", quietly = TRUE), fig.alt = "The same correlogram in ggplot2, with a legend mapping orange to period 24 and blue to period 168."----
ggsacf(MAD_m40, lag.max = 200)

## ----fit----------------------------------------------------------------------
fit <- msarima(MAD_m40,
               order = c(1, 0, 2),
               seasonal = list(c(0, 1, 1), c(0, 1, 1)))
fit

## ----summary------------------------------------------------------------------
summary(fit)

## ----nused--------------------------------------------------------------------
c(nobs = fit$nobs, n.used = fit$n.used, leading_NA = sum(is.na(residuals(fit))))

## ----fixed--------------------------------------------------------------------
fit_fixed <- msarima(MAD_m40,
                     order = c(1, 0, 2),
                     seasonal = list(c(0, 1, 1), c(0, 1, 1)),
                     fixed = c(NA, NA, NA, -0.6, NA, NA))
c(free = AIC(fit), fixed = AIC(fit_fixed))

## ----resid, fig.height = 3.5, fig.alt = "Correlogram of the residuals up to lag 200. The spikes are much smaller than before and mostly inside the confidence band."----
sacf(fit, lag.max = 200, main = "Residuals")

## ----portm--------------------------------------------------------------------
msportmanteau(fit, nlag = 5, lags_short = 8)
msportmanteau(fit, type = "Monti", nlag = 5, lags_short = 8)

## ----forecast, fig.alt = "Line chart of the last two weeks of data followed by 48 hours of forecasts, which repeat the daily shape, inside shaded 80 and 95 per cent prediction bands."----
fc <- forecast(fit, h = 48)
plot(fc, include = 336, main = "Two days ahead", ylab = "vehicles per hour")

## ----sim----------------------------------------------------------------------
set.seed(1)
y <- msarima.sim(nsim = 500,
                 order = c(1, 0, 1),
                 seasonal = list(c(0, 0, 1), c(0, 0, 1)),
                 seasonal.period = c(7, 30),
                 ar = 0.6, ma = c(0.4, 0.3, 0.2),
                 sigma2 = 0.5)

## ----sim2---------------------------------------------------------------------
y_sim <- simulate(fit, nsim = 168, seed = 1)

