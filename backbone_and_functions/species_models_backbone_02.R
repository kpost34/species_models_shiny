#Created by Keith Post on 1/14/23
#Backbone code for Species Models App: Species-Area Curves

pacman::p_load(here,tidyverse,sars,cowplot,nlstools)
source(here("backbone_and_functions","species_models_func_02.R"))

####### Species-Area Curves=========================================================================
##### Generating curves from inputs-----------------------------------------------------------------
#### Power law
### Equation and parameters
# S = cA^z
# S: number of species in a patch of size A
# c & z: fitted constants

### Create DF and scalars
## UI inputs
a_start<-10^-5
a_end<-10^10
c<-2.5
z<-0.35

## Build reactive DF
sar_aDF<-tibble(a=seq(a_start,a_end,length.out=20),
                log_a=log10(a))

### Make plots
## Plot with geom_function
sar_aDF %>%
  ggplot(aes(x=a)) +
  ggtitle("Power Law: Linear Scale") +
  geom_function(fun=~c*.x^z,color="darkgreen") +
  scale_x_continuous(expand=c(0,0),limits=c(0,NA)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,NA)) +
  labs(x="Area (ha)",
       y="No. of Species") +
  theme_bw()


## Plot with custom function
sar_aDF %>%
  plot_power_mod(c=c,z=z,col="darkgreen")



### Often plotted as log-log relation to make curve linear
# log(S) = b + zlog(A)
# b: intercept (equal to log c)
# z: slope

### Make plots
## Plot with geom_function
sar_aDF %>% 
  ggplot(aes(x=log_a)) +
  ggtitle("Power Law: Log-log scale") +
  geom_function(fun = ~log10(c) + z*.x,color="darkgreen") +
  # scale_x_continuous(expand=c(0,0),limits=c(0,NA)) +
  # scale_y_continuous(expand=c(0,0),limits=c(0,NA)) +
  labs(x="log10(Area (ha))",
       y="log10(No. of Species)") +
  theme_bw()

## Plot with custom function
sar_aDF %>%
  plot_powerlog_mod(c=c,z=z,col="darkgreen")
  



#### Semi-log model
### Equation and parameters
# S = log(cA^z) = log(c) + zlog(A)

### Make plots
## Plot with geom_function
sar_aDF %>%
  ggplot(aes(x=log_a)) +
  ggtitle("Semi-log model: log Area-linear species") +
  geom_function(fun=~log10(c) + z*.x,color="darkgreen") +
  labs(x="log10(Area)",
       y="No. of Species") +
  theme_bw()


## Plot with custom function
sar_aDF %>%
  plot_semilog_mod(c=c,z=z,col="darkgreen")





##### Working with example data---------------------------------------------------------------------
#### Load data sets from sars package (except for cole_sim)
sars_dfs_names<-c("aegean","aegean2","galap","niering")
data(list=sars_dfs_names)

#combine data (except for cole_sim) into one tibble
mget(sars_dfs_names) %>% 
  bind_rows(.id="name") -> sars_dfs


#### Plot data
### All datasets
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


### aegean only
## Power law: linear
# Create model 
power_nls2<-nls(s~c*a^z,
    data=sars_dfs %>%
      filter(name=="aegean") %>%
      select(-name),
    start=list(c=c,z=z))


# Plot data and model
#hard code
sars_dfs %>%
  filter(name=="aegean") %>%
  mutate(lwr=confint2(power_nls)[1,1]*a^confint2(power_nls)[2,1],
         upr=confint2(power_nls)[1,2]*a^confint2(power_nls)[2,2]) %>% 
  ggplot(aes(x=a,y=s)) +
  ggtitle("Power Law: Linear") +
  geom_point(color="black") +
  labs(x="Area (hectares)",
       y="No. of species") +
  theme_bw() +
  geom_function(fun=~coef(power_nls)[1]*.x^coef(power_nls)[2],color="darkgreen",
                linewidth=1.2) +
  geom_ribbon(aes(ymin=lwr,ymax=upr),fill="gray50",alpha=0.2)
  
#from function
sars_dfs %>%
  filter(name=="aegean") %>%
  plot_power_sars(col="darkgreen",reg=TRUE,mod=power_nls,col_reg="darkgreen")



## Power law: log-log
# Create model
powerlog_lm<-sars_dfs %>%
  filter(name=="aegean") %>%
  mutate(log_a=log10(a),
         log_s=log10(s)) %>%
  lm(formula=log_s~log_a)

#plot model from predict(lm object)
sars_dfs %>%
  filter(name=="aegean") %>%
  mutate(log_a=log10(a),
         pred_log_s=predict(powerlog_lm)) %>%
  ggplot() +
  geom_line(aes(x=log_a,y=pred_log_s)) +
  theme_bw()

# Plot data and model (using function)
sars_dfs %>%
  filter(name=="aegean") %>%
  plot_powerlog_sars(reg=TRUE,col_reg="darkgreen")



## Semi-log model
# Create model
semilog_lm<-sars_dfs %>%
  filter(name=="aegean") %>%
  mutate(log_a=log10(a)) %>%
  lm(formula=s~log_a)

#plot model from predict(lm object)
sars_dfs %>%
  filter(name=="aegean") %>%
  mutate(log_a=log10(a),
         pred_s=predict(semilog_lm)) %>%
  ggplot() +
  geom_line(aes(x=log_a,y=pred_s)) +
  theme_bw()

# Plot data and model (using function)
sars_dfs %>%
  filter(name=="aegean") %>%
  plot_semilog_sars(reg=TRUE,col_reg="darkgreen")

#all plots
sars_dfs %>%
  filter(name=="aegean") %>%
  plot_sars_grid(reg=TRUE,mod=power_nls,col_reg="darkgreen")


### aegean2
## Create model
power_nls_aegean2<-nls(s~c*a^z,
    data=sars_dfs %>%
      filter(name=="aegean2") %>%
      select(-name),
    start=list(c=c,z=z))

## Output all plots
sars_dfs %>%
  filter(name=="aegean2") %>%
  plot_sars_grid(reg=TRUE,mod=power_nls_aegean2,col_reg="darkblue")


### galap
## Create model
power_nls_galap<-nls(s~c*a^z,
    data=sars_dfs %>%
      filter(name=="galap") %>%
      select(-name),
    start=list(c=c,z=z))

## Output all plots
sars_dfs %>%
  filter(name=="galap") %>%
  plot_sars_grid(reg=TRUE,mod=power_nls_galap,col_reg="darkred")


### niering
## Create model
c<-1
z<-0.4
power_nls_niering<-nls(s~c*a^z,
    data=sars_dfs %>%
      filter(name=="niering") %>%
      select(-name),
    start=list(c=c,z=z))


## Output all plots
sars_dfs %>%
  filter(name=="niering") %>%
  plot_sars_grid(reg=TRUE,mod=power_nls_niering,col_reg="purple")



#### Compare models
### aegean
## Plot
sars_dfs %>%
  filter(name=="aegean") %>%
  plot_sars_grid(reg=TRUE,mod=power_nls,col_reg="darkgreen")

## Pull model info
tidy(powerlog_lm); tidy(semilog_lm)
glance(powerlog_lm); glance(semilog_lm)
glance(powerlog_lm) %>%
  select(r.squared,rse=sigma,AIC)
glance(semilog_lm) %>%
  select(r.squared,rse=sigma,AIC)
#compare R2 (since only one pred var) and sigma (=residual standard error)
#here: semilog slightly better R2 but greater sigma


#### App ideas
# 1) a) Provide inputs for a (min and max), c, and z and user can draw graph and tinker with
  #parameters
# 1) b) Outputs could include 1) linear scale, 2) log-log scale, and 3) semilog model
#2) Using real data (from sars package): 
  #a) radio button--choose data set
  #b) plot data--linear, log-log, semilog
  #c) tinker with model over it (just to get a feel for it)
#3) create linear models for log-log of power law and semi-log model
#4) compare results


#### ===============================================================================================
### NEXT


### DONE




#### LAST COMMIT





