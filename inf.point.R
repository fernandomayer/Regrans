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
dados2$lCW <- log10(dados2$CW)
dados2$lAW <- log10(dados2$AW)

par(mfrow=c(1,2))
plot(AW ~ CW, data = dados2)
plot(lAW ~ lCW, data = dados2)
par(mfrow=c(1,1))

## Linear model (regression) for total data
mod.total <- lm(lAW ~ lCW, data = dados2)
coef(mod.total)

## Calls a new object to receive data from looping
saida <- data.frame(SSResL = numeric(0), aL = numeric(0),
                    bL = numeric(0), nL = numeric(0),
                    SSResR = numeric(0), aR = numeric(0),
                    bR = numeric(0), nR = numeric(0))

## Define n.min = the minimum number of points for a regression
n.min <- 5

## Looping to estimate the iteractive linear models (regressions)
for(i in n.min:(nrow(dados2)-n.min)){
    regL <- lm(lAW[1:i] ~ lCW[1:i], data = dados2)
    regR <- lm(lAW[(i+1):(nrow(dados2))] ~ lCW[(i+1):(nrow(dados2))],
               data = dados2)
    SSResL <- sum(residuals(regL)^2)
    SSResR <- sum(residuals(regR)^2)
    saida <- rbind(saida,
                   data.frame(SSResL = SSResL,
                              aL = coef(regL)[1],
                              bL = coef(regL)[2],
                              nL = length(residuals(regL)),
                              SSResR = SSResR,
                              aR = coef(regR)[1],
                              bR = coef(regR)[2],
                              nR =length(residuals(regR))))
}
# Remove rownames
row.names(saida) <- NULL

## Calls a new column with Sums of Sum Squared Residuals of right and
## left models.
saida$sumSSResLR <- saida$SSResL + saida$SSResR

## Shows a line from saida's object with the minimum sumSSRes of right
## and left models; And apresent the sum of squared residuals of total
## model
saida[saida$sumSSResLR == min(saida$sumSSResLR),]

sum.total.model <- sum(residuals(mod.total)^2)
sum.total.model

### Graphics for the piecewise regression
require(lattice)

xyplot(AW ~ CW, data = dados, xlab = "Carapace Width (cm)", ylab =
       "Abdome Width (cm)", col = "black", panel =
       function(x, y, ...) {

       }
       )

