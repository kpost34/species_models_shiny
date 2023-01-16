#Created by Keith Post on 1/14/23
#Backbone code for Species Models App: Species-Area Curves

pacman::p_load(here,tidyverse,sars,cowplot)
source(here("backbone_and_functions","species_models_func_02.R"))

###### Species-Area Curves===========================================================================
#### Defining the relationship
### Power law
## Equation and parameters
# S = cA^z
# S: number of species in a patch of size A
# c & z: fitted constants

## Create DF and scalars
# UI inputs
a_start<-10^-5
a_end<-10^10
c<-2.5
z<-0.35

# Build reactive DF
sar_aDF<-tibble(a=seq(a_start,a_end,length.out=20),
                log_a=log10(a))

## Make plots
# Plot with geom_function
sar_aDF %>%
  ggplot(aes(x=a)) +
  ggtitle("Power Law: Linear Scale") +
  geom_function(fun=~c*.x^z,color="darkgreen") +
  # scale_x_continuous(expand=c(0,0),limits=c(0,NA)) +
  # scale_y_continuous(expand=c(0,0),limits=c(0,NA)) +
  labs(x="Area (ha)",
       y="No. of Species") +
  theme_bw()


# Plot with custom function
sar_aDF %>%
  plot_power_mod(c=c,z=z,col="darkgreen")



## Often plotted as log-log relation to make curve linear
# log(S) = b + zlog(A)
# b: intercept (equal to log c)
# z: slope

## Make plots
# Plot with geom_function
sar_aDF %>% 
  ggplot(aes(x=log_a)) +
  ggtitle("Power Law: Log-log scale") +
  geom_function(fun = ~log10(c) + z*.x,color="darkgreen") +
  # scale_x_continuous(expand=c(0,0),limits=c(0,NA)) +
  # scale_y_continuous(expand=c(0,0),limits=c(0,NA)) +
  labs(x="log10(Area (ha))",
       y="log10(No. of Species)") +
  theme_bw()

# Plot with custom function
sar_aDF %>%
  plot_powerlog_mod(c=c,z=z,col="darkgreen")
  



 ### Semi-log model
 ## Equation and parameters
 # S = log(cA^z) = log(c) + zlog(A)

## Make plots
# Plot with geom_function
sar_aDF %>%
  ggplot(aes(x=log_a)) +
  ggtitle("Semi-log model: log Area-linear species") +
  geom_function(fun=~log10(c) + z*.x) +
  # scale_x_continuous(expand=c(0,0),limits=c(0,NA)) +
  # scale_y_continuous(expand=c(0,0),limits=c(0,NA)) +
  labs(x="log10(Area (ha))",
       y="No. of Species") +
  theme_bw()

# Plot with custom function



### Working with example data
## Load data sets from sars package (except for cole_sim)
sars_dfs_names<-c("aegean","aegean2","galap","niering")
data(list=sars_dfs_names)

## Combine data (except for cole_sim) into one tibble
mget(sars_dfs_names) %>% 
  bind_rows(.id="name") -> sars_dfs


## Plot data
# All datasets
sars_dfs %>%
  ggplot(aes(x=a,y=s,color=name)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_viridis_d() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x="log10(Area)",
       y="log10(No. of Species") +
  theme_bw()


# aegean only
#linear
sars_dfs %>%
  filter(name=="aegean") %>%
  plot_linear_sars(col="darkblue",reg=TRUE)

#log-log
sars_dfs %>%
  filter(name=="aegean") %>%
  plot_log_sars(col="darkblue",reg=TRUE)

#semi-log
sars_dfs %>%
  filter(name=="aegean") %>%
  plot_semilog_sars(col="darkblue",reg=TRUE)

#all plots
sars_dfs %>%
  filter(name=="aegean") %>%
  plot_sars_grid(col="darkgreen")



galap %>%
  mutate(across(a:s,log10,.names="log_{.col}")) -> galap_logDF


data(niering)

niering %>%
  ggplot() +
  geom_point(aes(x=a,y=s)) +
  theme_bw()


data(aegean)

aegean %>%
  ggplot() +
  geom_point(aes(x=a,y=s),color="darkgreen") +
  theme_bw()

aegean



#### App ideas
# 1) a) Provide inputs for a (min and max), c, and z and user can draw graph and tinker with
  #parameters
# 1) b) Outputs could include 1) linear scale, 2) log-log scale, and 3) semilog model
#2) Using real data (from sars package): 
  #a) radio button--choose data set
  #b) plot data--linear, log-log, semilog
  #c) tinker with model over it (just to get a feel for it)
#3) linear model for log-log of power law and semi-log model
#4) compare results


#### ===============================================================================================
### NEXT


### DONE



#### LAST COMMIT
# continued building out backbone code and developing functions for s-a curves




