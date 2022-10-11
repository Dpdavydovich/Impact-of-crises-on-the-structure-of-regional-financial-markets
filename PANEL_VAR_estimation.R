# https://bdemeshev.github.io/r_cycle/cycle_files/09_panel_regression.html
library(panelvar)
library("tseries")
library("plm")
library("texreg")

PANELDATA=na.omit(PANELDATA)



PANELDATA$dln_qvrp=c(0,diff(PANELDATA$lnqvrp))

paneldata <- pdata.frame(PANELDATA,
                 index = c("id", "period"),
                 row.names = TRUE)


#panel=ts(paneldata, start = c(2007, 1), frequency = 4)

adf.test(paneldata[, "real_rate"])

adf.test(paneldata[, "lnrts"])

adf.test(paneldata[, "lnqvrp"])

adf.test(paneldata[, "dln_qvrp"])

adf.test(paneldata[, "md"])

adf.test(paneldata[, "cv"])

adf.test(paneldata[, "prod"])



panelvar=pvargmm(
  dependent_vars=c("prod"),
  lags=2,
  predet_vars = c("lnrts", "dln_qvrp","md", "cv"),
  exog_vars = c("real_rate", "qcurr"),
  transformation = "fd",
  data = paneldata,
  panel_identifier = c("id", "period"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 5,
  max_instr_predet_vars = 5,
  min_instr_dependent_vars = 1L,
  min_instr_predet_vars = 1L,
  collapse = FALSE
  )

saveRDS(panelvar,"PanelVAR.RDS")
PanelVAR
extract(panelvar)
coef(panelvar)

#######

panelvarmdprod=pvargmm(
  dependent_vars=c("md","prod"),
  lags=2,
  predet_vars = c("lnrts", "dln_qvrp", "cv"),
  exog_vars = c("real_rate", "qcurr"),
  transformation = "fd",
  data = paneldata,
  panel_identifier = c("id", "period"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 5,
  max_instr_predet_vars = 5,
  min_instr_dependent_vars = 1L,
  min_instr_predet_vars = 1L,
  collapse = FALSE
)
summary(panelvarmd)
summary(panelvarmdprod)

panelvarmdprod=pvargmm(
  dependent_vars=c("md","prod","cv","dln_qvrp"),
  lags=2,
  predet_vars = c("lnrts"),
  exog_vars = c("real_rate", "qcurr"),
  transformation = "fd",
  data = paneldata,
  panel_identifier = c("id", "period"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 5,
  max_instr_predet_vars = 5,
  min_instr_dependent_vars = 1L,
  min_instr_predet_vars = 1L,
  collapse = FALSE
)

summary(panelvarmdprod)
