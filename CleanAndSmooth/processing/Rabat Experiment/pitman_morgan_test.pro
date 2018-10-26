FUNCTION pitman_morgan_test, mod1_y, mod2_y, obs_y
;check NaN
indFinX = WHERE(FINITE(mod1_y))
indFinY = WHERE(FINITE(obs_y))
indFinXAndY = cgSetIntersection(indFinX, indFinY, count=countFin)
mod1_y = mod1_y[indFinXAndY]
mod2_y = mod2_y[indFinXAndY]
obs_y = obs_y[indFinXAndY]

x = mod1_y - obs_y
y =  mod2_y- obs_y
;that X = Y_mod_with_VGT – Y_obs and Y = Y_mod_with_PV – Y_obs
conf_level=0.95
;translated from pitman.morgan.test in R
ratio = 1.0
;alpha = 1 - conf.level
alpha = 1 - conf_level
;n <- length(x)
n =  N_ELEMENTS(x)
df = n - 2
;r <- cor(x, y)
r = CORRELATE(x, y)
;Var1 <- var(x)
Var1 = VARIANCE(x)
;Var2 <- var(y)
Var2 = VARIANCE(y)
;w <- var(x)/var(y)
w = VARIANCE(x)/VARIANCE(y)
;stat.t <- ((w - ratio) * sqrt(n - 2))/sqrt(4 * (1 - r^2) * w * ratio)
stat_t = ((w - ratio) * sqrt(n - 2))/sqrt(4 * (1 - r^2) * w * ratio)

;if (alternative == "two.sided") {
;k <- qt(1 - alpha/2, df = n - 2)
k = - T_CVF(1 - alpha/2, df)
;K <- 1 + (2 * (1 - r^2) * k^2)/(n - 2)
K = 1 + (2 * (1 - r^2) * k^2)/FLOAT(n - 2)
;low <- w * (K - sqrt(K^2 - 1))
low = w * (K - sqrt(K^2 - 1))
;up <- w * (K + sqrt(K^2 - 1))
up = w * (K + sqrt(K^2 - 1))
;pval <- 2 * (1 - pt(abs(stat.t), df = df))
pval = 2 * (1 - T_PDF(abs(stat_t), df))
;}
;estimate <- c(Var1, Var2)
;cint <- c(low, up)
;tstat <- stat.t
;method <- c("Paired Pitman-Morgan test")
;names(estimate) <- c("variance of x", "variance of y")
;names(tstat) <- "t"
;names(df) <- "df"
;names(ratio) <- c("ratio of variances")
;attr(cint, "conf.level") <- conf.level
;rval <- list(statistic = tstat, parameter = df, p.value = pval,
;conf.int = cint, estimate = estimate, null.value = ratio,
;alternative = alternative, method = method, data.name = dname)
;class(rval) <- "htest"
;return(rval)

;small p (<0.05) means that the hypo "same variances, same RMSE" can be rejected.
RETURN, pval


END