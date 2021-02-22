
# load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)

# print data --------------------------------------------------------------
iris
iris <- as_tibble(iris) # so it prints a little nicer
iris


# question 1 --------------------------------------------------------------

i1<-rename(iris, sepal_length = Sepal.Length, sepal_width = Sepal.Width, petal_length = Petal.Length, petal_width = Petal.Width, species = Species)
i1

# question 2 --------------------------------------------------------------
select(i1, sepal_length,sepal_width,petal_length,petal_width)
i1
i2<-mutate(i1, sl = sepal_length* 10, sw = sepal_width*10,pl = petal_length*10, pw = petal_width*10  )
i2

# question 3 --------------------------------------------------------------

i3 <- mutate(i2 , sepal_area = sl*sw, petal_area=pl*pw)
i4 <-select(i3,sepal_area,petal_area, species)

# question 4 --------------------------------------------------------------
i5 <- select(i3,sl)
i5

 summarize(i5,sampl_size = n(),
          maximum = max(sl),
          minimum = min(sl),
          iqr = IQR(sl),
          med = median(sl),
          range=maximum-minimum,
          q1 = quantile(sl, probs = 0.25),
          q3 = quantile(sl,probs = 0.75))

# question 5 --------------------------------------------------------------
i6 <- select(i3,sw)
summarize(i6,sample_size = n(),
          Mean = mean(sw),
          SD = sd(sw),
          sem = SD / sqrt(sample_size),
          ci_upper_limit = Mean + 1.96 * sem,
          ci_lower_limit = Mean - 1.96 * sem,
          variance = var(sw))

# question 6 --------------------------------------------------------------

ggplot(data = i3) +
  geom_jitter(mapping = aes(x = species, y = pw))          

# question 7 --------------------------------------------------------------

pw_summary <-
  summarize(
    i3, 
    mean_pw= mean(pw),
    sem = sd(pw) / sqrt(n()),
    ci_upper_limit = mean_pw + 1.96 * sem,
    ci_lower_limit = mean_pw - 1.96 * sem
  )
pw_summary

# question 8 --------------------------------------------------------------

ggplot(data = i3) +
  geom_point(mapping = aes(x = pl, y = pw, color = species))

