patients_task <- patients
patients_task$Глюкоза[c(3, 15, 45)] <- NA

str(patients$Возраст)
str(patients$Глюкоза)

patients$Пол <- factor(patients$Пол, levels = c("м", "ж"))
levels(patients$Пол)

patients$возраст_группа_2 <- ifelse(patients$Возраст <= 60, "Молодые", "Старшие")

patients[patients$Возраст > 75, ]

head(patients[, c("Лейкоциты", "Глюкоза")])
summary(patients[, c("Лейкоциты", "Глюкоза")])

aggregate(Глюкоза ~ Пол, data = patients, mean)

aggregate(Лейкоциты ~ Пол + возраст_группа_2, data = patients, mean)

aggregate(Глюкоза ~ Пол, data = patients, 
          FUN = function(x) c(Mean = mean(x), SD = sd(x), N = length(x)))

boxplot(Глюкоза ~ Пол, data = patients,
        main = "Распределение глюкозы по полу",
        xlab = "Пол", ylab = "Глюкоза")

t.test(Лейкоциты ~ Пол, data = patients)

sum(is.na(patients_task))

which(is.na(patients_task$Глюкоза))

patients_no_na <- na.omit(patients_task)
dim(patients_task); dim(patients_no_na)

patients_task$Глюкоза[is.na(patients_task$Глюкоза)] <- 
  median(patients_task$Глюкоза, na.rm = TRUE)

aggregate(Лейкоциты ~ Пол, patients_task, mean, na.rm = TRUE)
aggregate(Лейкоциты ~ Пол, patients_no_na, mean)

final_result <- aggregate(Гемоглобин ~ возраст_группа_2, 
                          data = patients, 
                          FUN = function(x) c(Mean = mean(x), SD = sd(x)))
colnames(final_result) <- c("Группа", "Среднее", "Стд.откл.")

write.csv(final_result, "анализ_гемоглобина.csv", row.names = FALSE)
file.exists("анализ_гемоглобина.csv")
