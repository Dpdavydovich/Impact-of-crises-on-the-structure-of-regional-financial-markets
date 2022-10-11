library (vars)

#добавление забытых логарифмов переменных
DATA_fo$log_RTS=log(DATA_fo$RTS)
DATA_fo$log_RUB_EUR=log(DATA_fo$RUB_EUR)
DATA_fo$log_RUB_USD=log(DATA_fo$RUB_USD)

DATA_fo <- ts(DATA_fo, start = c(2007, 1), frequency = 4)

#удаление пропущенных переменных
DATA_fo=na.omit(DATA_fo)

#наиболее полная модель
svEndog = c("cfo_md","cfo_cv","cfo_prod","log_RTS","log_cfo_VRP","log_RUB_EUR","log_RUB_USD","proc_stavka")
varcfo <- VAR(DATA_fo[, svEndog], p=4, type = c("const"))
summary(varcfo)

svEndog = c("cfo_md","cfo_cv","cfo_prod","log_RUB_USD")
varcfo <- VAR(DATA_fo[, svEndog], p=4, type = c("const"))
summary(varcfo)

#исключение "log_RTS","log_cfo_VRP","log_RUB_EUR"
svEndog = c("cfo_md","cfo_cv","cfo_prod","log_RUB_USD","proc_stavka")
varcfo <- VAR(DATA_fo[, svEndog], p=4, type = c("const"))
summary(varcfo)

#оценка только с переменными "cfo_md","cfo_cv","cfo_prod"
svEndog = c("cfo_md","cfo_cv","cfo_prod")
varcfo <- VAR(DATA_fo[, svEndog], p=4, type = c("const"))
summary(varcfo)


#оценка только с переменными "cfo_md","cfo_cv","cfo_prod" и с экзогенной проц. ставкой
svExog = c("proc_stavka")
svEndog = c("cfo_md","cfo_cv","cfo_prod")
varcfo <- VAR(DATA_fo[, svEndog], p=4, type = c("const"),exogen = DATA_fo[,svExog])
summary(varcfo)
