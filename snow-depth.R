library("readr")
library("dplyr")
library("ggplot2")
library("rstanarm")
library("bayesplot")

Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Tilastoitava lumensyvyys mitataan aamulla klo 8 paikallista aikaa (kesäaikaan klo 9). 
# Arvo -1 = ei lunta. Arvo 0 = havaintoasemalla ei ole lunta, mutta sen ympäristössä aukealla on.

d <- readr::read_csv("kaisaniemi.csv", na="", col_type = c("Lumensyvyys (cm)" = "i")) %>% 
  setNames(c("year", "month", "day", "_clock", "tzone", "snow_depth", "temp")) %>%
  filter(!is.na(snow_depth)) %>%
  mutate(t = ISOdate(year, month, day), 
         f_month = as.factor(month),
         decade = as.numeric(t - ISOdate(1990, 1, 1), units="days")/365.25) %>%
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
  labs(title = "Distribution of propability (p) of snow on 24.12. in Kaisaniemi")
print(p3)

scaled_year = scale(xmas$year)

stan_data <- with(xmas, list(N=length(snow), decade=as.vector(scaled_year), snow=as.numeric(snow)))


compile_and_fit_model <- function(model_code, data, vars_of_interest) {
  stan_start_time <- Sys.time()
  model <- stan_model(model_code = model_code) # compilation takes time if model is changed
  fit <- sampling(model, data = data)
  message("Compilation and fitting in secs ", difftime(Sys.time(), stan_start_time, units="secs"))
  
  posterior <- as.matrix(fit)
  print(traceplot(fit))
  print(fit)
  
  # Print posterior distributions for interesting variables
  posterior <- as.matrix(fit)
  for (var_name in vars_of_interest)
  {
    plot <- mcmc_areas(posterior, pars = c(var_name), prob = 0.95)  + 
      ggtitle(paste("Variable ", var_name, " posterior distributions with median and 95% interval"))
    print(plot)  
  }
  
  message("Total duration in secs ", difftime(Sys.time(), stan_start_time, units="secs"))
  return(fit)
}

logistic_reggr_code <- "
data {
  int<lower=0> N;
  vector[N] decade;
  int<lower=0,upper=1> snow[N];
}
parameters {
  real alpha;
  real beta;
}
model {
  snow ~ bernoulli_logit(alpha + beta * decade);
}
"

fit <- compile_and_fit_model(
  model_code = logistic_reggr_code, 
  data = stan_data, 
  vars_of_interest = c("alpha", "beta"))

scaled_2019 <- scale(2019, attr(scaled_year, "scaled:center"), attr(scaled_year, "scaled:scale"))
log_regr_post_draws <- extract(fit)

predictions <- inv.logit(log_regr_post_draws$alpha + log_regr_post_draws$beta * scaled_2019[1])
hist(predictions)
  
  

