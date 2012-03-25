# R example for piecewise regression 
# We use the raw materials data set from class 


# y = cost here
# x1 = shipment size

# Entering the data:

y <- c(2.57,4.40,4.52,1.39,4.75,3.55,2.49,4.27,4.42,4.10,2.21,2.90)

x1 <- c(650,340,400,800,300,570,720,480,425,460,675,600)

# Original scatter plot of Y against x1:

plot(x1, y)

# Defining indicator x2:

x2 <- as.numeric(x1 > 500)

# Defining x2star:

x2star <- (x1 - 500)*x2

# Fitting the piecewise regression function with a 
# changepoint at X = 500:                          

raw.piece.reg <- lm(y ~ x1 + x2star)
# Recall x2star = (x1 - 500)*x2 here!

summary(raw.piece.reg)

# Plotting the estimated piecewise regression curve on top of the data:

myplotgrid <- seq(min(x1),max(x1),length=400)
my.predicted.values <- predict(raw.piece.reg, newdata=data.frame(x1 = myplotgrid, 
x2star = (myplotgrid - 500)*as.numeric(myplotgrid > 500)))

plot(x1, y)
lines(myplotgrid, my.predicted.values)

###############################################

# Allowing for a discontinuity in the regression function:

# Defining another indicator x3:

x3 <- as.numeric(x1 > 500)

# Fitting a discontinuous piecewise regression function with a
# changepoint at X = 500:          

raw.piece.reg.disc <- lm(y ~ x1 + x2star + x3)
# Recall x2star = (x1 - 500)*x2 here!

summary(raw.piece.reg.disc)
                 
# Based on the output, beta_3 may be zero (P-value = 0.3645),
# and the continuous piecewise regression model may be fine.

# Plotting the discontinuous piecewise regression function:

myplotgrid2 <- seq(min(x1),max(x1),length=400)
my.predicted.values2 <- predict(raw.piece.reg.disc, newdata=data.frame(x1 = myplotgrid2, 
x2star = (myplotgrid2 - 500)*as.numeric(myplotgrid2 > 500), x3 = as.numeric(myplotgrid2 > 500)))

plot(x1, y)
points(myplotgrid2, my.predicted.values2, pch=20, cex=0.8)

###############################################

# When the appropriate changepoint is not known: 

# This is an R function to pick the optimal changepoint for a 
# continuous piecewise linear regression, based on a SSE criterion:

# Copy this function into R:

####
#
pick.changepoint <- function(response, predictor, min.changept, max.changept, gridlength){
my.changept.choices <- pretty(c(min.changept, max.changept), n=gridlength)
results <- matrix(0, nrow=length(my.changept.choices), ncol=2)
for (i in 1:length(my.changept.choices))
  {
  x2star <- (predictor - my.changept.choices[i])*as.numeric(predictor > my.changept.choices[i]) 
  fit <- lm(response ~ predictor + x2star)
  results[i,1] <- my.changept.choices[i]
  results[i,2] <- anova(fit)["Residuals", "Sum Sq"]
  }
print(paste("Optimal changepoint:", results[results[,2]==min(results[,2]),1] ))
return(results)
}
#
####

# Run the function on the raw materials data set:

pick.changepoint(response=y, predictor=x1, min.changept=400, max.changept=600, gridlength=50)

# For this data set, using a changepoint of X = 480 produces the smallest SSE.
