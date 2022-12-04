#Created by Keith Post on 11/6/22
#Backbone code for Species Models App

pacman::p_load(here,tidyverse,sars)

aegean

#power function
#S=cA^z

#log-log axes
#log(S)=log(cA^z) = log(c) + zlog(A)


#semi-log model
#S=log(cA^z)

#semi-log axes
#S=log(c) + zlog(A)


#### First Stab=====================================================================================
### Data wrangling
aegean %>%
  mutate(across(a:s,log,.names="log_{.col}")) -> aegeanDF


### ggplots
## Linear scale
aegeanDF %>%
  ggplot(aes(x=a,y=s)) +
  geom_point() +
  theme_bw()

## log-log scale
aegeanDF %>%
  ggplot(aes(x=log_a,y=log_s)) +
  geom_point() +
  theme_bw()

## Semi-log scale
aegeanDF %>%
  ggplot(aes(x=log_a,y=s)) +
  geom_point() +
  theme_bw()

### sars package
## Info
sars_models() #model list
display_sars_models() #model info


## Fit model
fit<-sar_loga(aegean,grid_start="partial")

fitC<-sar_multi(data=galap,obj=c("power","loga","monod"))

plot.multi(fitC)


aegean %>%
  sar_multi() -> aegean_sars


  plot.multi(aegean_sars)



  

  
  
  
  
  
  
  #1) Sample data sets (more than 1) or upload data
  
  
  #2) Plot data--multiple scales: 1) linear, 2) semi-log, 3) log-log, 4) others
  
  
  #3) Attempt to fit multiple models (checkbox)
  #a) visually
  
  #b) tabular output
  
  
  #4) extrapolation
  
  
  #5) thresholds
  
  
  
  
  



