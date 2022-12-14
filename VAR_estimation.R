library (vars)
install.packages("panelvar")
library(panelvar)
install.packages("xlsx")
library("xlsx")

install.packages("tseries")
library("tseries")

#-------------------------------------------------------------------------------
#--- Initial settings

#sFolder = "d:/KKHolodilin/VShE/Code/VKR/"
sInFile = "Data_fo.xlsx"
sOutFile = ""

#-------------------------------------------------------------------------------
#--- Load data

DATA_fo = read.xlsx(paste(sFolder, sInFile, sep=""), detectDates = T)

#-------------------------------------------------------------------------------
#--- ?????????? ?????????? ?????????? ? ?????? ?????????

DATA_fo$log_RTS=log(DATA_fo$RTS)
DATA_fo$log_RUB_EUR=log(DATA_fo$RUB_EUR)
DATA_fo$log_RUB_USD=log(DATA_fo$RUB_USD)
DATA_fo$dreal_rate=c(0,diff(DATA_fo$real_rate))

DATA_fo$d_cfo_VRP=c(0,diff(DATA_fo$cfo_VRP))
DATA_fo$d2_cfo_VRP=c(0,diff(DATA_fo$d_cfo_VRP))
DATA_fo$d_log_cfo_VRP=c(0,diff(DATA_fo$log_cfo_VRP))
DATA_fo$d2_log_cfo_VRP=c(0,diff(DATA_fo$d_log_cfo_VRP))

DATA_fo$d_szfo_VRP=c(0,diff(DATA_fo$szfo_VRP))
DATA_fo$d2_szfo_VRP=c(0,diff(DATA_fo$d_szfo_VRP))
DATA_fo$d_log_szfo_VRP=c(0,diff(DATA_fo$log_szfo_VRP))
DATA_fo$d2_log_szfo_VRP=c(0,diff(DATA_fo$d_log_szfo_VRP))

DATA_fo$d_yufo_VRP=c(0,diff(DATA_fo$yufo_VRP))
DATA_fo$d2_yufo_VRP=c(0,diff(DATA_fo$d_yufo_VRP))
DATA_fo$d_log_yufo_VRP=c(0,diff(DATA_fo$log_yufo_VRP))
DATA_fo$d2_log_yufo_VRP=c(0,diff(DATA_fo$d_log_yufo_VRP))

DATA_fo$d_pfo_VRP=c(0,diff(DATA_fo$pfo_VRP))
DATA_fo$d2_pfo_VRP=c(0,diff(DATA_fo$d_pfo_VRP))
DATA_fo$d_log_pfo_VRP=c(0,diff(DATA_fo$log_pfo_VRP))
DATA_fo$d2_log_pfo_VRP=c(0,diff(DATA_fo$d_log_pfo_VRP))

DATA_fo$d_ufo_VRP=c(0,diff(DATA_fo$ufo_VRP))
DATA_fo$d2_ufo_VRP=c(0,diff(DATA_fo$d_ufo_VRP))
DATA_fo$d_log_ufo_VRP=c(0,diff(DATA_fo$log_ufo_VRP))
DATA_fo$d2_log_ufo_VRP=c(0,diff(DATA_fo$d_log_ufo_VRP))

DATA_fo$d_skfo_VRP=c(0,diff(DATA_fo$skfo_VRP))
DATA_fo$d2_skfo_VRP=c(0,diff(DATA_fo$d_skfo_VRP))
DATA_fo$d_log_skfo_VRP=c(0,diff(DATA_fo$log_skfo_VRP))
DATA_fo$d2_log_skfo_VRP=c(0,diff(DATA_fo$d_log_skfo_VRP))

DATA_fo$d_dfo_VRP=c(0,diff(DATA_fo$dfo_VRP))
DATA_fo$d2_dfo_VRP=c(0,diff(DATA_fo$d_dfo_VRP))
DATA_fo$d_log_dfo_VRP=c(0,diff(DATA_fo$log_dfo_VRP))
DATA_fo$d2_log_dfo_VRP=c(0,diff(DATA_fo$d_log_dfo_VRP))

DATA_fo$d_sfo_VRP=c(0,diff(DATA_fo$sfo_VRP))
DATA_fo$d2_sfo_VRP=c(0,diff(DATA_fo$d_sfo_VRP))
DATA_fo$d_log_sfo_VRP=c(0,diff(DATA_fo$log_sfo_VRP))
DATA_fo$d2_log_sfo_VRP=c(0,diff(DATA_fo$d_log_sfo_VRP))

DATA_fo <- ts(DATA_fo, start = c(2007, 1), frequency = 4)

#???????? ??????????? ????????
DATA_fo=na.omit(DATA_fo)

#???????? ?? ??????????????
#????? ??? ???? ?? ??????????

adf.test(DATA_fo[, "real_rate"])
adf.test(diff(DATA_fo[, "real_rate"]))
adf.test(diff(DATA_fo[, "log_RTS"]))
adf.test(diff(DATA_fo[, "log_RUB_EUR"]))
adf.test(diff(DATA_fo[, "log_RUB_USD"]))

#???
adf.test(DATA_fo[, "log_cfo_VRP"])
adf.test(DATA_fo[, "d_log_cfo_VRP"])
adf.test(DATA_fo[, "d2_log_cfo_VRP"])
adf.test(d_cfo_VRP)
adf.test(d2_cfo_VRP)
adf.test(DATA_fo[, "cfo_md"])
adf.test(DATA_fo[, "cfo_cv"])
adf.test(DATA_fo[, "cfo_prod"])

#????
adf.test(DATA_fo[, "log_szfo_VRP"])
adf.test(DATA_fo[, "d_log_szfo_VRP"])
DATA_fo[, "d_log_szfo_VRP"]
adf.test(DATA_fo[, "szfo_md"])
adf.test(DATA_fo[, "szfo_cv"])
adf.test(DATA_fo[, "szfo_prod"])

#???
adf.test(DATA_fo[, "log_yufo_VRP"])
adf.test(DATA_fo[, "yufo_md"])
adf.test(DATA_fo[, "yufo_cv"])
adf.test(DATA_fo[, "yufo_prod"])

#???
adf.test(DATA_fo[, "log_pfo_VRP"])
adf.test(DATA_fo[, "pfo_md"])
adf.test(DATA_fo[, "pfo_cv"])
adf.test(DATA_fo[, "pfo_prod"])

#???
adf.test(DATA_fo[, "log_ufo_VRP"])
adf.test(DATA_fo[, "ufo_md"])
adf.test(DATA_fo[, "ufo_cv"])
adf.test(DATA_fo[, "ufo_prod"])

#?-???
adf.test(DATA_fo[, "log_skfo_VRP"])
adf.test(DATA_fo[, "skfo_md"])
adf.test(DATA_fo[, "skfo_cv"])
adf.test(DATA_fo[, "skfo_prod"])

#???
adf.test(DATA_fo[, "log_dfo_VRP"])
adf.test(DATA_fo[, "dfo_md"])
adf.test(DATA_fo[, "dfo_cv"])
adf.test(DATA_fo[, "dfo_prod"])

#???
adf.test(DATA_fo[, "log_sfo_VRP"])
adf.test(DATA_fo[, "sfo_md"])
adf.test(DATA_fo[, "sfo_cv"])
adf.test(DATA_fo[, "sfo_prod"])



#???: ?????? VAR ?? ????????????? ???????????
#4 ????
svEndog = c("cfo_md","cfo_cv","cfo_prod","d2_log_cfo_VRP","dreal_rate")
#svExogen = DATA_fo[c("d2_log_cfo_VRP","real_rate"]

varcfo <- VAR(DATA_fo[, svEndog], p=4, type = c("const"))
summary(varcfo)

#2 ????
varcfo2 <- VAR(DATA_fo[, svEndog], p=2, type = c("const"))
summary(varcfo2)


#???: ?????? VAR ?? ????????????? ???????????
svEndog = c("sfo_md","sfo_cv","sfo_prod","d2_log_sfo_VRP","dreal_rate")
#4 ????
varsfo <- VAR(DATA_fo[, svEndog], p=4, type = c("const"))
summary(varsfo)


#????
svEndog = c("szfo_md","szfo_cv","szfo_prod","d2_log_szfo_VRP","dreal_rate")
#4 ????
varszfo <- VAR(DATA_fo[, svEndog], p=4, type = c("const"))
summary(varszfo)


#???
svEndog = c("yufo_md","yufo_cv","yufo_prod","d2_log_yufo_VRP","dreal_rate")
#4 ????
varyufo <- VAR(DATA_fo[, svEndog], p=4, type = c("const"))
summary(varyufo)


#???
svEndog = c("pfo_md","pfo_cv","pfo_prod","d2_log_pfo_VRP","dreal_rate")
#4 ????
varpfo <- VAR(DATA_fo[, svEndog], p=4, type = c("const"))
summary(varpfo)


#???
svEndog = c("ufo_md","ufo_cv","ufo_prod","d2_log_ufo_VRP","dreal_rate")
#4 ????
varufo <- VAR(DATA_fo[, svEndog], p=4, type = c("const"))
summary(varufo)


#????
svEndog = c("skfo_md","skfo_cv","skfo_prod","d2_log_skfo_VRP","dreal_rate")
#4 ????
varskfo <- VAR(DATA_fo[, svEndog], p=4, type = c("const"))
summary(varcfo)


#???
svEndog = c("dfo_md","dfo_cv","dfo_prod","d2_log_dfo_VRP","dreal_rate")
#4 ????
vardfo <- VAR(DATA_fo[, svEndog], p=4, type = c("const"))
summary(vardfo)



#-------------------------------------------------------------------------------
#--- Impulse responses

feir_cfo <- irf(varcfo, impulse = "dreal_rate", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_cfo)
feir_cfo <- irf(varcfo, impulse = "d2_log_cfo_VRP", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_cfo)


feir_szfo <- irf(varszfo, impulse = "dreal_rate", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_szfo)
feir_szfo <- irf(varszfo, impulse = "d2_log_szfo_VRP", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_szfo)

feir_yufo <- irf(varyufo, impulse = "dreal_rate", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_yufo)
feir_yufo <- irf(varyufo, impulse = "d2_log_yufo_VRP", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_yufo)

feir_pfo <- irf(varpfo, impulse = "dreal_rate", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_pfo)
feir_pfo <- irf(varpfo, impulse = "d2_log_pfo_VRP", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_pfo)

feir_ufo <- irf(varufo, impulse = "dreal_rate", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_ufo)
feir_ufo <- irf(varufo, impulse = "d2_log_ufo_VRP", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_ufo)

feir_skfo <- irf(varskfo, impulse = "dreal_rate", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_skfo)
feir_skfo <- irf(varskfo, impulse = "d2_log_skfo_VRP", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_skfo)

feir_dfo <- irf(vardfo, impulse = "dreal_rate", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_dfo)
feir_dfo <- irf(vardfo, impulse = "d2_log_dfo_VRP", n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir_dfo)
