# read in the data
water = read.table('AgricultureWater.txt',header=T)


# Polynomial regression
library(splines)


## Splines (and basis function expansions)
# To use splines, you will need the `splines` library in R.  
# All basis function expansions are done within R formulas (so I hope you are comfortable with them by now).  
# For example, to fit a B-spline you would specify the formula `lm(y~bs(x, df=))` where `x` is the 
# variable you want a spline in.

# - `poly(var_name, degree=)` - creates a `degree` polynomial in `var_name`
# - `bs(var_name, df=, degree=, knots=)` - creates a `degree` B-spline in the variable `var_name` with `df` degrees of freedom.  Altneratively, you can use the `knots` argument to specify where the knots are put.  If you only give the `df` then it will put the knots equally spaced.
# - `ns(var_name, df=, degree=, knots=)` - create a `degree` natural spline in the variable `var_name` with `df` degrees of freedom or `knots`.
# - `I()` - this is for special use within formulas where you can put any function you want.  For example, if I put `y~I(x>10)` it will set up a step function at the point 10.  Likewise, `y~I(x^2)` will put a squared term for `x` (although `poly()` above is a better way to do polynomial regression).  So, anytime you alter a variable within a formula, make sure you use `I()`.

model1 = lm(swc ~ poly(cwsi, degree = 2),data=water)
model2 = lm(swc ~ ns(water$cwsi, knots=c(0.5, 0.6, 0.7, 0.8)), data=water)

plot(water$cwsi, water$swc)
lines(water$cwsi[order(water$cwsi)], model1$fitted.values[order(water$cwsi)],col='orange')
lines(water$cwsi[order(water$cwsi)], model2$fitted.values[order(water$cwsi)],col='seagreen')

summary(model2)
