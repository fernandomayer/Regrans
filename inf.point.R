dados <- read.csv2("FLA.csv")

plot(log(LA) ~ log(LC), data=dados)

par(mfrow=c(1,2))
hist(log(dados$LA))
hist(log(dados$LC))
par(mfrow=c(1,1))

dados2 <- dados[order(dados$LC),]
dados2[,1] <- log10(dados2[,1])
dados2[,2] <- log10(dados2[,2])


saida <- data.frame(SQResL = numeric(0), aL = numeric(0), bL =
                    numeric(0), nL = numeric(0), SQResR = numeric(0), aR
                    = numeric(0), bR = numeric(0), nR = numeric(0))

for(i in 3:(nrow(dados2)-2)){
    regL <- lm(LA[1:i] ~ LC[1:i], data = dados2)
    regR <- lm(LA[(i+1):(nrow(dados2)-2)] ~ LC[(i+1):(nrow(dados2)-2)],
                    data = dados2)
    SQResL <- sum(residuals(regL)^2)
    SQResR <- sum(residuals(regR)^2)
    saida <- rbind(saida, data.frame(SQResL = SQResL, aL =
                    coef(regL)[1], bL = coef(regL)[2], nL =
                    length(dados2$LA[1:i]), SQResR = SQResR, aR =
                    coef(regR)[1], bR = coef(regR)[2], nR =
                    length(dados2$LA[(i+1):(nrow(dados2)-2)])))
}
row.names(saida) <- NULL
