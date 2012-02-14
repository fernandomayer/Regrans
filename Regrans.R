x <- data.frame(X = seq(1,100,1), Y = rnorm(100, 2050, 3.5))

mod.left <- 0
mod.right <- 0

for(i in nrow(x)) {
	left <- x[1:i,]
	right <- x[i+1:nrow(x),]
	nam.left <- paste("mod.left", i, sep = ".")
	nam.right <- paste("mod.right", i, sep = ".") 
	print(assign(nam.left, 1:i)) <- with(left, lm(Y ~ X))
	print(assign(nam.right, 1:i)) <- with(right, lm(Y ~ X))
}

