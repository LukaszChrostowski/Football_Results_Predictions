library(e1071)

fn <- function(k) {
  A <- gknn(formula = Wynik ~ ., 
            data = cbind("Wynik" = as.factor(dd1y), 
                         dd1), 
            k = k)
  mean(predict(A, rbind(dd1, dd2))[-(1:length(dd1y))] != dd2y)
}
sapply(1:20, fn)
