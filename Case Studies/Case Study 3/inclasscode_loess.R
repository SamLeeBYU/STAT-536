# read in the data
water = read.table('AgricultureWater.txt',header=T)

# ts = sample(1:nrow(water),20)
# train = water[-ts,]
# test = water[ts,]
# 
# plot(train$cwsi,train$swc,ylim=c(22,30))
# points(test$cwsi,test$swc,pch=19)
# 
# 
# # Local Regression
# 
# # Because local regression is typically a bit more stable than kernel smoothing, 
# # only local regression is highlighted here
# # Of course, you can try kernel smoothing if you want!
# 
# # loess(formula=, data=, degree=, span=) - 
# # this uses a tricubic weighting function with smoothing span span to fit a local 
# # regression of degree degree. If span<1 then only a fraction of the data is weighted. 
# # IMPORTANT: this function can only handle up to 4 numeric predictors. For higher dimensions, 
# # local regression usually doesn’t work well due to the curse of dimensionality.
# 
# 
# 
# # first, the default settings of quadratic (degree=2) and span=0.75
# model = loess(swc~cwsi,data=train, span=median(span.min))
# oo = order(train$cwsi)
# lines(train$cwsi[oo],model$fitted[oo],col=2)
# 
# # Now increase the flexibility by using more local weights with span=0.15
# model2 = loess(swc~cwsi,data=train,span=.15)
# lines(train$cwsi[oo],model2$fitted[oo],col=3)

s = seq(from=0.1, to=1, length.out = 1000)
#Take k.spans random # of samples from s to use for span parameter
#This is how Random Grid Search CV works
k.spans = 100
spans = sample(s, k.spans)

k.folds <- nrow(water)
k.prop <- nrow(water)/k.folds
k.index <- ((1:k.folds)*k.prop) |> round()
k.index.lag <- dplyr::lag(k.index, default = 1)
water.sample = sample(1:nrow(water),)

predictions = matrix(nrow=k.folds, ncol=k.spans)

rmse.spans <- matrix(nrow = k.folds, ncol=k.spans)
for(i in 1:k.spans){
  span = spans[i]
  for(fold in 1:k.folds){
    
    ts = water.sample[k.index.lag[fold]:k.index[fold]]
    test = water[ts,]
    train = water[-ts,]
    
    train.model <- loess(swc~cwsi,data=train, span=span, degree=1)
    #Compute RMSE
    rmse.spans[fold, i] = sqrt((mean((test$swc-predict(train.model, test)), na.rm = T)^2))
  }
}
#See which span performed the best on the k-fold CV
span.min = median(spans[which(colMeans(rmse.spans) == min(colMeans(rmse.spans)))])

ts = sample(1:nrow(water),20)
train = water[-ts,]
test = water[ts,]

plot(train$cwsi,train$swc,ylim=c(22,30))
points(test$cwsi,test$swc,pch=19)

model.1 = loess(swc~cwsi,data=train, span=span.min, degree=1)
model.2 = loess(swc~cwsi,data=train, span=span.min, degree=2)
oo = order(train$cwsi)
lines(train$cwsi[oo],model.1$fitted[oo],col=3)
lines(train$cwsi[oo],model.2$fitted[oo],col=2)

sqrt(mean((test$swc-predict(model.2, test))^2))
sqrt(mean((test$swc-predict(model.1, test))^2))
