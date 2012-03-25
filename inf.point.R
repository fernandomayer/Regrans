#######################################################################
# Regrans: a analitic package to segmented regressions
#######################################################################

# Loading Deep-sea crabs database for testing analisys
dados <- read.csv2("FLA.csv")

# Graphic to data visualization - AW - Abdome Width vs CW - Carapace
# Width
plot(log(AW) ~ log(CW), data = dados)

# Log of variables target (AW and CW) histograms
par(mfrow=c(1,2))
hist(log(dados$AW), main = "Abdome Width", xlab = "AW", ylab =
     "Frequency", col = "gray")
hist(log(dados$CW), main = "Carapace Width", xlab = "CW", ylab =
     "Frequency", col = "gray")
par(mfrow=c(1,1))

# New object with logaritmized variables and classified by Carapace
# Width
dados2 <- dados[order(dados$CW),]
dados2[,1] <- log10(dados2[,1])
dados2[,2] <- log10(dados2[,2])

## Calls a new object to receive data from looping
saida <- data.frame(SQResL = numeric(0), aL = numeric(0), bL =
                    numeric(0), nL = numeric(0), SQResR = numeric(0), aR
                    = numeric(0), bR = numeric(0), nR = numeric(0))

## Looping to estimate the iteractive linear models (regressions)
for(i in 5:(nrow(dados2)-5)){
    regL <- lm(AW[1:i] ~ CW[1:i], data = dados2)
    regR <- lm(AW[(i+1):(nrow(dados2))] ~ CW[(i+1):(nrow(dados2))],
                    data = dados2)
    SQResL <- sum(residuals(regL)^2)
    SQResR <- sum(residuals(regR)^2)
    saida <- rbind(saida, data.frame(SQResL = SQResL, aL =
                    coef(regL)[1], bL = coef(regL)[2], nL =
                    length(dados2$CW[1:i]), SQResR = SQResR, aR =
                    coef(regR)[1], bR = coef(regR)[2], nR =
                    length(dados2$CW[(i+1):(nrow(dados2))])))
}

# Remove names of rows!
row.names(saida) <- NULL

