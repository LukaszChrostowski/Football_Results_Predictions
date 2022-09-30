formula <- Wynik ~ Gospodarz + Gość + `Posiadanie piłki Gospodarz` + `Sytuacje bramkowe Gospodarz` + `Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz` + `Rzuty rożne Gospodarz` + `Spalone Gospodarz` + `Interwencje bramkarzy Gospodarz` + `Faule Gospodarz` + `Żółte kartki Gospodarz` + `Czerwone kartki Gospodarz` + `Posiadanie piłki Gość` + `Sytuacje bramkowe Gość` + `Strzały na bramkę Gość` + `Strzały niecelne Gość` + `Rzuty rożne Gość` + `Spalone Gość` + `Interwencje bramkarzy Gość` + `Faule Gość` + `Żółte kartki Gość` + `Czerwone kartki Gość` + `Gole Gospodarz` + `Gole Gość`
formula1 <- Wynik ~ `Posiadanie piłki Gospodarz` + `Sytuacje bramkowe Gospodarz` + `Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz` + `Rzuty rożne Gospodarz` + `Spalone Gospodarz` + `Interwencje bramkarzy Gospodarz` + `Faule Gospodarz` + `Żółte kartki Gospodarz` + `Czerwone kartki Gospodarz` + `Posiadanie piłki Gość` + `Sytuacje bramkowe Gość` + `Strzały na bramkę Gość` + `Strzały niecelne Gość` + `Rzuty rożne Gość` + `Spalone Gość` + `Interwencje bramkarzy Gość` + `Faule Gość` + `Żółte kartki Gość` + `Czerwone kartki Gość` + `Gole Gospodarz` + `Gole Gość`

df2 <- df1
df2$Wynik <- ifelse(df1$Wynik == "H", 1, 0)

A1 <- glm(formula = formula, data = df2, family = binomial())
A11 <- glm(formula = formula1, data = df2, family = binomial())

df3 <- df1
df3$Wynik <- ifelse(df1$Wynik == "A", 1, 0)

A2 <- glm(formula = formula, data = df3, family = binomial())
A22 <- glm(formula = formula1, data = df3, family = binomial())


predict(A1, type = "response")
cc1 <- ifelse(predict(A1, type = "response") > .5, "H", ifelse(predict(A2, type = "response"), "A", "D"))
cc2 <- ifelse(predict(A11, type = "response") > .5, "H", ifelse(predict(A22, type = "response"), "A", "D"))
mean(cc1 != df1$Wynik)
mean(cc2 != df1$Wynik)

