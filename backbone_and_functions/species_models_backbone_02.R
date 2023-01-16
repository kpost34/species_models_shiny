#Created by Keith Post on 1/14/23
#Backbone code for Species Models App: Species-Area Curves

pacman::p_load(here,tidyverse,sars)
source(here("backbone_and_functions","species_models_func_01.R"))

###### Species-Area Curves===========================================================================
#### Defining the relationship
### Power law
## Equation and parameters
# S = cA^z
# S: number of species in a patch of size A
# c & z: fitted constants

# Create DF and scalars
sar_aDF<-tibble(A = 10^(-5:10))
                  
c<-2.5
z<-0.35

# Plot with geom_function
sar_aDF %>%
  ggplot(aes(x=A)) +
  ggtitle("Power Law: Linear Scale") +
  geom_function(fun=~c*.x^z,color="darkgreen") +
  scale_x_continuous(expand=c(0,0),limits=c(0,NA)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,NA)) +
  labs(x="Area (ha)",
       y="No. of Species") +
  theme_bw()


## Often plotted as log-log relation to make curve linear
# log(S) = b + zlog(A)
# b: intercept (equal to log c)
# z: slope

# Create DF 
sar_aDF %>%
  mutate(log_A=log10(A)) -> sar_logaDF



# Plot
sar_logaDF %>% 
  ggplot(aes(x=log_A)) +
  ggtitle("Power Law: Log-log scale") +
  geom_function(fun = ~log10(c) + z*.x,color="darkgreen") +
  # scale_x_continuous(expand=c(0,0),limits=c(0,NA)) +
  # scale_y_continuous(expand=c(0,0),limits=c(0,NA)) +
  labs(x="log10(Area (ha))",
       y="log10(No. of Species)") +
  theme_bw()
  



 ### Semi-log model
 ## Equation and parameters
 # S = log(cA^z) = log(c) + zlog(A)
sar_logaDF %>%
  ggplot(aes(x=log_A)) +
  ggtitle("Semi-log model: log Area-linear species") +
  geom_function(fun=~log10(c) + z*.x) +
  # scale_x_continuous(expand=c(0,0),limits=c(0,NA)) +
  # scale_y_continuous(expand=c(0,0),limits=c(0,NA)) +
  labs(x="log10(Area (ha))",
       y="No. of Species") +
  theme_bw()



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


sars_dfs %>%
  filter(name=="aegean") %>%
  ggplot(aes(x=a,y=s)) +
  geom_point() +
  theme_bw()

#log-log
sars_dfs %>%
  filter(name=="aegean") %>%
  ggplot(aes(x=log10(a),y=log10(s))) +
  geom_point() +
  theme_bw() 

#semi-log
sars_dfs %>%
  filter(name=="aegean") %>%
  ggplot(aes(x=log10(a),y=s)) +
  geom_point() +
  theme_bw()


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
# began creating backbone code and functions for species-area curves




