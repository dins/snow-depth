library("readr")
library("dplyr")
library("ggplot2")

Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Tilastoitava lumensyvyys mitataan aamulla klo 8 paikallista aikaa (kesäaikaan klo 9). 
# Arvo -1 = ei lunta. Arvo 0 = havaintoasemalla ei ole lunta, mutta sen ympäristössä aukealla on.

d <- readr::read_csv("kaisaniemi.csv", na="", col_type = c("Lumensyvyys (cm)" = "i")) %>% 
  setNames(c("year", "month", "day", "_clock", "tzone", "snow_depth", "temp")) %>%
  filter(!is.na(snow_depth)) %>%
  mutate(t = ISOdate(year, month, day), 
         f_month = as.factor(month),
         decade = as.numeric(t - ISOdate(2000, 1, 1), units="days")/365.25) %>%
  select(year, f_month, t, decade, snow_depth, temp)  %>%
  mutate(snow_depth = ifelse(snow_depth < 0, 0, snow_depth) ) %>%
  mutate(snow = snow_depth > 0)

xmas <- filter(d, format(t, "%d.%m.") == "24.12.")
p1 <- ggplot(xmas, aes(x=t, y=snow_depth)) + geom_smooth()
print(p1)
p2 <- ggplot(xmas, aes(x=t, y=snow_depth)) + 
  geom_point(aes(shape=snow)) + 
  scale_shape_manual(values=c(19, 1)) + 
  geom_smooth(method="lm") + 
  labs(title = "Snow depth in Kaisaniemi on 24.12. each year")
print(p2)

counts <- xmas %>% group_by(snow) %>% summarise(n=n())
print(counts)

num_snow = counts$n[counts$snow == TRUE]
num_no_snow = counts$n[counts$snow == FALSE]
prior_snow = 1
prior_no_snow = 1
shape_snow = num_snow + prior_snow
shape_no_snow = num_no_snow + prior_no_snow

posterior_samples <- data.frame(p=rbeta(10000, shape_snow, shape_no_snow))
p3 <- ggplot(posterior_samples, aes(x=p)) + 
  stat_function(fun = dbeta, args = list(shape1 = shape_snow, shape2 = shape_no_snow)) +
  geom_vline(aes(xintercept=qbeta(0.5, shape_snow, shape_no_snow)), linetype="dashed") +
  labs(title = "Propability (p) of snow on 24.12.")
print(p3)
