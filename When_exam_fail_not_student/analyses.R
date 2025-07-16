library(ggplot2)

a <- 1
b <- 0
p <- 0.3
n <- 37
mi <- p*a + (1 - p)*b
var <- p*(1 - p)
std <- sqrt(var)/sqrt(n)

p2 <- 0.5
mi2 <- p2*a + (1 - p2)*b
var2 <- p2*(1 - p2)
std2 <- sqrt(var2)/sqrt(n)

p3 <- 0.7
mi3 <- p3*a + (1 - p3)*b
var3 <- p3*(1 - p3)
std3 <- sqrt(var3)/sqrt(n)

sample <- rnorm(200, mi, std)
data <- data.frame(x = 40 * sample)
ggplot(data, aes(x)) +
  geom_histogram() +
  theme_bw()

grades <- cut(sample, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1)*40)
levels(grades) <- c("2", "3", "3.5", "4", "4.5", "5")

data <- data.frame(x = grades)

ggplot(data, aes(x)) +
  geom_bar() +
  theme_bw()

data_lev <- data.frame(x = sample)
ggplot(data_lev, aes(x)) +
  stat_function(fun = function(x) {dnorm(x, mi, std)}) +
  xlim(0,1) +
  theme_bw()

grade <- c("2", "3", "3.5", "4", "4.5", "5")
prob1 <- c(pnorm(0.5, mi, std),
          pnorm(0.6, mi, std) - pnorm(0.5, mi, std),
          pnorm(0.7, mi, std) - pnorm(0.6, mi, std),
          pnorm(0.8, mi, std) - pnorm(0.7, mi, std),
          pnorm(0.9, mi, std) - pnorm(0.8, mi, std),
          1 - pnorm(0.9, mi, std))


theoretical_dist <- data.frame(x = grade, y = prob1, t = 'Teoretycznie')

ggplot(theoretical_dist, aes(x, y)) +
  geom_col() +
  theme_bw()


prob_emp <- c(0.66, 0.16, 0.09, 0.06, 0.03, 0.01)
empirical_dist <- data.frame(x = grade, y = prob_emp, t = 'Real exam')

dist_stack <- rbind(theoretical_dist, empirical_dist)

ggplot(dist_stack, aes(x, y, fill = t)) +
  geom_col(position = "dodge") +
  theme_bw() +
  labs(fill = "")

chisq.test(213*round(prob1, 2), 213*prob_emp)

ggplot(data_lev, aes(x)) +
  stat_function(fun = function(x) {0.5*dnorm(x, mi, std) + 
      0.35*dnorm(x, mi2, std2) + 
      0.15*dnorm(x, mi3, std3)}) +
  xlim(0,1) +
  theme_bw()

mixture_model <- function(x, mi, mi2, mi3, std, std2, std3)
{
  0.5*pnorm(x, mi, std) +
    0.35*pnorm(x, mi2, std2) + 
    0.15*pnorm(x, mi3, std3)
}

prob_theo <- c(mixture_model(0.5, mi, mi2, mi3, std, std2, std3),
           mixture_model(0.6, mi, mi2, mi3, std, std2, std3) - mixture_model(0.5, mi, mi2, mi3, std, std2, std3),
           mixture_model(0.7, mi, mi2, mi3, std, std2, std3) - mixture_model(0.6, mi, mi2, mi3, std, std2, std3),
           mixture_model(0.8, mi, mi2, mi3, std, std2, std3) - mixture_model(0.7, mi, mi2, mi3, std, std2, std3),
           mixture_model(0.9, mi, mi2, mi3, std, std2, std3) - mixture_model(0.8, mi, mi2, mi3, std, std2, std3),
           1 - mixture_model(0.9, mi, mi2, mi3, std, std2, std3))

prob_theo_2 <- ceiling(prob_theo*210)/sum(ceiling(prob_theo*210))

theoretical_dist <- data.frame(x = grade, y = prob_theo_2, t = 'Theoretical exam')

dist_stack <- rbind(theoretical_dist, empirical_dist)

ggplot(dist_stack, aes(x, y, fill = t)) +
  geom_col(position = "dodge") +
  theme_bw() +
  labs(fill = "", x = "grade", y = "frequency")

mixture_model_2 <- function(x, mi, mi2, mi3, std, std2, std3)
{
  0.7*pnorm(x, mi2, std2) + 
    0.3*pnorm(x, mi3, std3)
}

prob_theo_fixed <- c(mixture_model_2(0.5, mi, mi2, mi3, std, std2, std3),
               mixture_model_2(0.6, mi, mi2, mi3, std, std2, std3) - mixture_model_2(0.5, mi, mi2, mi3, std, std2, std3),
               mixture_model_2(0.7, mi, mi2, mi3, std, std2, std3) - mixture_model_2(0.6, mi, mi2, mi3, std, std2, std3),
               mixture_model_2(0.8, mi, mi2, mi3, std, std2, std3) - mixture_model_2(0.7, mi, mi2, mi3, std, std2, std3),
               mixture_model_2(0.9, mi, mi2, mi3, std, std2, std3) - mixture_model_2(0.8, mi, mi2, mi3, std, std2, std3),
               1 - mixture_model_2(0.9, mi, mi2, mi3, std, std2, std3))

prob_theo_fixed_2 <- ceiling(prob_theo_fixed*210)/sum(ceiling(prob_theo_fixed*210))

theoretical_dist_fixed <- data.frame(x = grade, y = prob_theo_fixed_2, t = 'Valid exam?')

dist_stack <- rbind(theoretical_dist, empirical_dist, theoretical_dist_fixed)

ggplot(dist_stack, aes(x, y, fill = t)) +
  geom_col(position = "dodge") +
  theme_bw() +
  labs(fill = "", x = "grade", y = "frequency")

ggplot(data_lev, aes(x)) +
  stat_function(fun = function(x) {0.7*dnorm(x, mi2, std2) + 
      0.3*dnorm(x, mi3, std3)}) +
  xlim(0,1) +
  theme_bw()

library(dplyr)
dist_stack %>%
  group_by(t = "Real exam") %>%
  ggplot(aes(x, y, fill = t)) +
  geom_col(position = "dodge") +
  theme_bw() +
  labs(fill = "", x = "grade", y = "frequency") +
  theme(legend.position = 'none')
