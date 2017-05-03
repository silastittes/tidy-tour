## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.lp = '')

## ---- echo=FALSE, results='hide'-----------------------------------------
suppressMessages(library(tidyverse))
suppressMessages(library(RCurl))
suppressMessages(library(xtable))

## ---- echo = FALSE-------------------------------------------------------
means <- seq(1, 20, length.out = 2)
vals <- as_vector(means %>% map(~ rnorm(n = 1000, mean = .x)))
par(mar = c(5,5.5,4,2)*1.1)
plot(density(vals, bw = 2),
     lwd = 3,
     axes = F, main = "", 
     xlab = "R skillz", cex.lab = 2.5)
axis(side = 1, at = means, labels = F)
axis(side = 2, labels = F)
text(10.5, 0.04, "AM I HERE?", col = "red", cex = 2)
arrows(x0 = 10, y0 = 0.03, x1 = 10, y1 = 0, col = "red", lwd = 3)

## ---- eval=FALSE---------------------------------------------------------
## data <- read.csv("datafile")
## data1 <- data[,c(1,2,3)]
## data2 <- subset(data1, column1 == "test")
## data3 <- ...

## ------------------------------------------------------------------------
iris[,3:5] %>% 
  head(5)

## ---- eval=FALSE---------------------------------------------------------
## subject %>% (then)
##   action1 %>% (then)
##   action2 %>% (then)
##   action3

## ------------------------------------------------------------------------
iris[,3:5] %>% 
  tail(5) %>% 
  head(1)

## ------------------------------------------------------------------------
samps <- 5
ducks <- data.frame(duck = rnorm(samps),
                    goose = c(rnorm(samps - 1), NA),
                    idx = 1:samps)

## ---- results='asis', echo=FALSE-----------------------------------------
print(xtable(ducks), comment = FALSE)

## ------------------------------------------------------------------------
ducks_td <- ducks %>% 
  gather(key = "bird_type", value = "temp", -idx) %>%
  drop_na()

## ---- results='asis', echo=FALSE-----------------------------------------
print(xtable(ducks_td), comment = F)

## ------------------------------------------------------------------------
ducks_wd <- ducks_td %>% 
  spread(bird_type, temp)

## ---- results='asis', echo=FALSE-----------------------------------------
print(xtable(ducks_wd), comment = FALSE)

## ------------------------------------------------------------------------
data.frame(VADeaths) %>% 
  gather(key = "municipal.sex", value = "deaths_K") %>%
  head(5)

## ------------------------------------------------------------------------
data.frame(VADeaths) %>% 
  gather(key = "municipal.sex", value = "deaths_K") %>%
  separate(col = municipal.sex, 
           into = c("munic","sx"), 
           sep = "\\.") %>%
  head(5)

## ------------------------------------------------------------------------
#by index, name, or mixture of two
iris %>% 
  select(1, Petal.Length) %>% 
  head(5)

## ------------------------------------------------------------------------
#desc() for descending order
iris %>% 
  arrange(Species, Sepal.Width, desc(Sepal.Length)) %>% 
  select(Sepal.Width, Sepal.Length, Species) %>%
  head(5)

## ------------------------------------------------------------------------
#notice all columns would be returned w/o pipe to select()
iris %>% 
  filter(Species == "setosa", Sepal.Length < 6) %>%
  select(Sepal.Length, Species) %>%
  head(5)

## ------------------------------------------------------------------------
iris %>% 
  mutate(Sepal.Area = Sepal.Length * Sepal.Width) %>%
  select(Sepal.Area, Species) %>%
  head(5)

## ------------------------------------------------------------------------
#note: group_by() can be used with multiple variables
iris %>% 
  group_by(Species) %>%
  summarise(mean_SL = mean(Sepal.Length),
            sd_SL = sd(Sepal.Length),
            q25 = quantile(Sepal.Length, 0.25),
            q75 = quantile(Sepal.Length, 0.75))

## ------------------------------------------------------------------------
#also see new modelr package for more like this
iris %>%
  sample_n(2) %>% 
  select(Petal.Length, Species)

## ------------------------------------------------------------------------
iris %>%
  sample_frac(0.02) %>% 
  select(Petal.Length, Species)

## ------------------------------------------------------------------------
#surprisingly hard in base R
iris %>% 
  group_by(Species) %>%
  mutate(counts = n()) %>%
  select(Species, counts) %>%
  sample_n(2)

## ------------------------------------------------------------------------
death_sum <- data.frame(VADeaths) %>% 
  gather(key = "municipal.sex", value = "deaths_K") %>%
  separate(col = municipal.sex, 
           into = c("munic","sx"), sep = "\\.") %>%
  group_by(munic, sx) %>% 
  summarise(mean_death = mean(deaths_K), 
            sd_death = sd(deaths_K))

## ---- results='asis', echo = FALSE---------------------------------------
print(xtable(death_sum), comment = F)

## ---- eval=FALSE---------------------------------------------------------
## #default:
## ggplot(data = <DATA>) +
##   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
## 
## #alternative 1:
## <DATA> %>%
##   ggplot( ) +
##   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
## 
## #alternative 2 (my usual preference):
## <DATA> %>%
##   ggplot(mapping = aes(<MAPPINGS>)) +
##   <GEOM_FUNCTION>()

## ------------------------------------------------------------------------
p <- mtcars %>% 
  ggplot(aes(x = drat, y = wt)) +
  geom_point() 

## ---- echo=FALSE---------------------------------------------------------
p

## ------------------------------------------------------------------------
p <- mtcars %>% 
  ggplot(aes(x = drat, y = wt)) +
  geom_point() +
  theme_gray(base_size = 30)

## ---- echo=FALSE---------------------------------------------------------
p

## ------------------------------------------------------------------------
p <- mtcars %>% 
  ggplot(aes(x = drat, y = wt, size = hp)) +
  geom_point(aes(colour = factor(am)), alpha = 0.5) +
  xlab("Rear axle ratio") +
  ylab("Weight") +
  theme_classic(base_size = 20)

## ---- echo=FALSE---------------------------------------------------------
p

## ------------------------------------------------------------------------
p <- iris %>% 
  ggplot(aes(x = Species, 
             y = Petal.Width, 
             fill = Species)) +
  geom_violin(alpha = 0.3) +
  theme_minimal(base_size = 30)

## ---- echo=FALSE---------------------------------------------------------
p

## ------------------------------------------------------------------------
p <- mtcars %>%
  ggplot(aes(x = hp, y = mpg)) +
  geom_point() +
  facet_wrap(~ vs) +
  theme_light(base_size = 30)

## ---- echo=FALSE---------------------------------------------------------
p

## ------------------------------------------------------------------------
p <- mtcars %>%
  mutate(tran = ifelse(am == 0, 
                  "automatic", 
                  "manual")) %>%
  ggplot(aes(x = hp, y = mpg)) +
  geom_point() +
  facet_wrap(~ tran) +
  theme_light(base_size = 30)

## ---- echo=FALSE---------------------------------------------------------
p

## ------------------------------------------------------------------------
p <- data.frame(VADeaths) %>% 
  mutate(age_group = rownames(VADeaths)) %>%
  gather("pop_group", "death_rate", -age_group) %>% 
  separate(pop_group, c("municipal","sex"), 
           sep = "\\.") %>%
  ggplot(aes(x = sex, y = death_rate)) + #new stuff below
  geom_jitter(width = 0.1) +
  facet_grid(~municipal) +
  geom_pointrange(stat = "summary",
              fun.ymin = function(z) quantile(z,0.25),
              fun.ymax = function(z) quantile(z,0.75),
              fun.y = mean, colour = "dodgerblue", 
              alpha = 0.5, lwd = 2) +
  theme_classic(base_size = 30)

## ---- echo=FALSE---------------------------------------------------------
p

## ------------------------------------------------------------------------
#for example
vec <- 1:4

store <- rep(NA, length(vec))
for(i in vec){
  store[i] <- i*2
}
store

## ------------------------------------------------------------------------
vec <- 1:4

vec %>% 
  map_dbl(function(x) x * 2)

# or
vec %>% 
  map_dbl(~ .x * 2)

## ------------------------------------------------------------------------
my_list <- list(1, TRUE, rnorm(2), "lists!!")
#access with double brackets [[]]
my_list[[3]]

## ------------------------------------------------------------------------
#I() is the identity function
#same as function(x) x
my_list %>% 
  map(I)

## ------------------------------------------------------------------------
mtcars %>%
  map_dbl(mean) %>%
  head(5)

## ------------------------------------------------------------------------
mtcars %>%
  map_lgl(is.numeric) %>%
  head(5)

## ------------------------------------------------------------------------
mtcars %>%
  map_int(length) %>%
  head(5)

## ------------------------------------------------------------------------
1:4 %>%
  map_df( ~ data.frame(rbind(rnorm(3))))

## ------------------------------------------------------------------------
df_list <- list(mtcars = mtcars, 
                 iris = iris, 
                 VADeaths = VADeaths)

#the nrow function isn't important, 
#the ability to utilize any function here is.

