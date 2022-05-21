# First very simple model based on average market odds for away home or draw by categorical regression
Data <- readr::read_csv("https://www.football-data.co.uk/new/POL.csv")

Data$Res <- sapply(
  Data$Res,
  FUN = function(x) {
    switch(
      x,
      "H" = 1, # home win coded as 1
      "D" = 0, # draw win coded as 0
      "A" = -1 # away win coded as -1
    )
  }
)

# nnet is a library for neural network categorical regression 
# (i.e logistic regression for many classes) is a simple instance 
#of a neural network with sigmoid activating function

library(nnet)

Model <- multinom(
  formula = Res ~ AvgH + AvgD + AvgA, # this is not intended to make sense just a presentation
  data = Data, # next arguments will be passed to nnet function as a part of ... 
  #they will specify the parameters for numerical method used to estimate regression
  abstol = .Machine$double.eps,
  reltol = .Machine$double.eps,
  maxit = 10000
)

# Model Parameters:

summary(Model)

# Not the greatest fit

# make predictions:

pred <- predict(Model, Data[c("AvgH", "AvgD", "AvgA")])

# Resubstitution error:

mean(pred != Data$Res)

# Very bad more than 50%