library(ggplot2)
library(targets)
library(magrittr)
tar_load(fig1.coefs)
fig1.coefs %>% 
  ggplot(aes(predictor, coef, color = extrap_richness_response_group)) +
  geom_point() 

tar_load(data)
tar_load(data_CDI)

coefs <- fig1.coefs %>% data.table::data.table()
# The first model only used the coefs in the all category
ranges <- coefs[extrap_richness_response_group == "all"]
data
# The function 

richness <- data %>% 
  # select the correct predictor
  select(ranges$extrap_richness_response_group) %>% 
  # give each combination an unique id
  mutate(Site_Code = site_name) %>% 
  # transform to long format for calculation
  pivot_longer(!Site_Code, names_to = "predictor") %>% 
  # join the coefs
  right_join(Spp.Rich.coeff) %>% 
  # get the product of value and its coef
  mutate(prod = value * coef) %>% 
  # group by the combination of predicators
  group_by(Site_Code)  %>% 
  # get the product for each combination and round to integer
  summarise(richness = round(sum(prod), digits = 0)) %>%
  # remove negative values
  mutate(richness = if_else(richness < 0, 0, richness))
return(richness)


library(sensobol)
library(data.table)
N <- 2 ^ 9
params <- ranges$predictor

matrices <- c("A", "B", "AB", "BA")
first <- total <- "azzini"
order <- "second"
R <- 10 ^ 3
type <- "percent"
conf <- 0.95
# times <- seq(0, 150, 1)

# Construct the sampling matrix 
# Assume that there are all normal distributions 
mat <- sobol_matrices(matrices = matrices, N = N, params = params, 
                      order = order, type = "LHS")

mat[,"LM"] <- rnorm(mat[, "LM"],0.543, 1.273 )
for( i in 1:10){
  mat[, i] <- rnorm(mat[, i], 
                    mean = ranges$confint_lower_boundary[i],
                    sd = ranges$confint_upper_boundary[i])
}

# Simplify The function 
dat <- data.table(data) %>% 
  melt.data.table(variable.name = "params")

model_richness_fun <- function(dt, dat){
  apply(dt, 1, function(x){
    dt <- as.data.table(x, keep.rownames = TRUE)
    dt <- merge.data.table(dat, dt, by.x = "params", by.y = "rn")
    dt[, prod:=value*x]
    richness <- sum(dt$prod)
    ifelse(richness < 0, 0, richness)
    
  }, simplify = TRUE)
}
# computing the output from all sampled variance
y <- model_richness_fun(mat, dat)

plot_uncertainty(Y = y, N = N) + 
  labs(y = "Counts", x = "$y$")


quantile(y, probs = c(0.01, 0.025, 0.5, 0.975, 0.99, 1))
plot_scatter(data = mat, N = N, Y = y, params = params, method = "bin")
# Error  Computation failed in `stat_binhex()`:
# Solution: install hexbin pkg

plot_multiscatter(data = mat, N = N, Y = y, params = params, smpl = 2^11)

ind <- sobol_indices(matrices = mat, Y = y, N = N, params = params,
                      first = first, total = total, order = order, 
                     boot = TRUE, R = R,
                      parallel = "no", type = type, conf = conf)

ind.dummy <- sobol_dummy(Y = y, N = N, params = params, boot = TRUE,
                         R = R)
plot(ind, dummy = ind.dummy)

