

install.packages('AER')

install.packages('margins')

library(AER)


library(tidyverse)
library(stargazer)
data("SmokeBan")
SmokeBan <-
  SmokeBan %>% mutate(smoker = ifelse(smoker == 'yes', 1, 0))
reg_mod <- lm(smoker ~ ., data = SmokeBan)
SmokeBan$y_pred <- predict(reg_mod, SmokeBan) %>% round(2)

logit_mod <-
  glm(
    smoker ~ age + ban + gender + education + afam + hispanic,
    data = SmokeBan,
    family = "binomial"
  )
stargazer(
  logit_mod,
  type = 'text' ,
  no.space = T,
  covariate.labels = c(
    "Age",
    "Workplace Smoking Ban",
    "Female",
    "High school",
    "Some college",
    "College",
    "Master",
    "African-American",
    "Hispanic",
    "Constant"
  )
)

library(margins)
cplot(logit_mod,
      x = "ban",
      what = "effect",
      ylim = c(-1, 0))
plot(margins(logit_mod, variables = c('ban', 'gender')),
     labels = c("Workplace Smoking Ban", "Female"))

data("DoctorVisits")
DoctorVisits <- DoctorVisits %>% mutate(age = age * 100)
hist(DoctorVisits$visits)

poi_mod <-
  glm(
    visits ~ gender + age + income + illness + reduced + health,
    family = "poisson",
    data = DoctorVisits
  )

stargazer(poi_mod,
          type = 'text',
          no.space = T)



"""# Poisson Regression"""

data("DoctorVisits")
DoctorVisits <- DoctorVisits %>% mutate(age = age * 100)
hist(DoctorVisits$visits)

poi_mod <-
  glm(
    visits ~ gender + age + income + illness + reduced + health,
    family = "poisson",
    data = DoctorVisits
  )

stargazer(poi_mod,
          type = 'text',
          no.space = T)

"""# Poisson and Negative Binomial regression"""

fHH1 <-
  read_csv("https://raw.githubusercontent.com/proback/BeyondMLR/master/data/fHH1.csv")

modela <- glm(total ~ age, family = poisson, data = fHH1)
confint(modela)
exp(confint(modela))

model0 <- glm(total ~ 1, family = poisson, data = fHH1)

fHH1 <- fHH1 %>% mutate(age2 = age * age)
modela2 <- glm(total ~ age + age2, family = poisson,
               data = fHH1)
modela2 %>% stargazer(type = 'text', no.space = T)

modela2L <- glm(total ~ age + age2 + location,
                family = poisson, data = fHH1)

modela2L %>% stargazer(type = 'text', no.space = T)

# Account for over-dispersion with negative binomial model
modelinb <- glm.nb(total ~ age + age2 + location, data = fHH1)
modelinb %>% stargazer(type = 'text', no.space = T)