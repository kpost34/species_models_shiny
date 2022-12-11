#Created by Keith Post on 12/3/22
#Backbone code for Species Models App

pacman::p_load(here,tidyverse)




##### Island Biogeography===========================================================================
#### Formulae
### Concept/Background
# species equilibrium occurs when colonization and extinction rates are equal
# C(s) = E(s)
# C: colonization rate per unit time
# E: extinction rate per unit time

### Express each rate as an equation (basic, linear model)
## Colonization
# C(s) = c(p-s); colonization depends on # of species remaining in pool and per species colonization rate
# s: number of species
# p: total number of species in species pool
# c: mean rate of colonization averaged over species in the species pool

## Extinction
# E(s) = hs
# h: mean rate of extinction averaged over species in the species pool

## Equilibrium equation
# s* = cp/(c + h)


### Plot example
## params: h = 0.1, c = 0.1, p = 100
s<-seq(0,100,1)
c<-0.1
h<-0.1
p<-100
#calculate colonization and extinction rates
C<-c*(p-s)
E<-h*s

## Create tibble of s, C, and E
island1<-tibble(s,C,E) %>%
  pivot_longer(cols=!s,names_to="param",values_to="rate")

## Plot rate functions
island1 %>%
  ggplot(aes(x=s,y=rate,group=param,color=param)) +
  geom_line() +
  scale_x_continuous(name="Species",breaks=seq(0,100,20)) +
  scale_y_continuous(name="Colonization rate",breaks=seq(0,10,2),
                     sec.axis=sec_axis(~.x,name="Extinction rate",breaks=seq(0,10,2))) +
  scale_color_viridis_d(end=0.7) +
  theme_bw() +
  theme(legend.position="none")

## Intersection point
(c*p)/(c + h) #50


## Plot s over time
# General form: S(t+1) = S(t) + C(t) - E(t)
# S(t+1) = S(t) + c(p-s) - hs

# Richness
s<-c(0,rep(NA,49))

for(i in 1:50){
  s[i+1]<-s[i] + c*(p-s[i]) - h*s[i]
}

# Immigration
i<-c(0,rep)

s_tTab<-tibble(t=0:50,
               s)

s_tTab %>%
  ggplot() +
  geom_point(aes(x=t,y=s)) +
  theme_bw()

### Second modeling approach taking distance (C) and area (E) into account
## New formulas
# C(s) = c(p-s)*exp(-phi*d)
# E(s) = s*exp^(-ep*a)

# s* = (cp*exp(ep*a))/(c*exp(ep*a) + exp(phi*d))

## params: 
# d: distance between island and mainland
# a: area of island
# phi: fit parameter governing distance decay of colonization rate
# ep (epsilon): fit parameter governing effect of area on extinction

phi<-.0001
ep<-.001
p<-100
a<-2500

#d<-c(100,2000,5000)

s<-seq(0,100,length.out=100) %>% rep(.,3)
d<-rep(c(100,2000,10000),each=100)
a<-rep(5000,300)

island2<-tibble(s,d,a)

island2 %>%
  split(.$d) %>%
  map(function(x) {
    c*(p-x$s)*exp(-phi*x$d)
  }) -> island2_list


island2 %>%
  mutate(C=c*(p-s)*exp(-phi*d),
         E=s*exp(-ep*a),
         d=as.factor(d)) %>% 
  ggplot(aes(x=s,y=C,group=d,color=d)) +
  geom_line() +
  geom_line(aes(y=E,group=a),color="black") +
  theme_bw()





## Plot s over time
#General form: S(t+1) = S(t) + C(t) - E(t)
#S(t+1) = S(t) + c(p-s)*exp(-phi*d) - s*exp^(-ep*a)
s<-c(0,rep(NA,99))

for(i in 1:100){
  s[i+1]<-s[i] + c*(p-s[i])*exp(-phi*d)-s*exp(ep*a)
}

s_tTab<-tibble(t=0:100,
               s)

s_tTab %>%
  ggplot() +
  geom_point(aes(x=t,y=s)) +
  theme_bw()




















frates.simberloff <- irregular_multiple_datasets(list = simberloff, 
                                                vectorlist = list(3:17, 3:18, 3:17, 3:19, 3:17, 3:16), 
                                                c = 0.001, e = 0.001, jacobian = T)
rates.islands <- irregular_multiple_datasets(list = simberloff, 
                                             vectorlist = list(3:17, 3:18, 3:17, 3:19, 3:17, 3:16), 
                                             c = 0.001, e = 0.001, column = "Island", n = 5, jacobian = T)
rates.taxonomy <- irregular_multiple_datasets(list = simberloff, 
                                              vectorlist = list(3:17, 3:18, 3:17, 3:19, 3:17, 3:16), 
                                              c = 0.0001, e = 0.0001, column = "Tax. Unit 1", n = 20, jacobian = T)


rates.islands %>%
  select(Group,c,e) %>%
  mutate(across(!Group,~signif(.x,3))) -> island_rates

I0<-log(1)
b<-.1
curve(exp(I0-b*x),0,50,
      xlab="No. of Species (R)",ylab="Rate (I or E)")


#### Compute colonization and extinction rates
## Convert presence-absence matrices into # colonized per time interval
simberloff[[1]] %>% 
  #use rowwise to differentiate "Gen. Sp" in multiple orders
  rowwise() %>%
  mutate(Taxa=paste0(`Tax. Unit 1`[Taxa=="Gen. Sp."],Taxa)) %>% 
  #retain taxa and all post-defaunation measures
  select(-c(PRE,`Tax. Unit 1`:Island)) %>%
  #set initial values to 0
  mutate(`0`=0,.before="44") %>%
  #pivot twice as a way to transpose data
  pivot_longer(cols=!Taxa,names_to="day",values_to="p_a") %>% 
  pivot_wider(names_from="Taxa",values_from="p_a") %>% 
  #'duplicate' data to assign whether a species underwent an extinction or colonization event
  mutate(across(!day,~case_when(
    (.x-lag(.x))==-1 ~ "E",
    (.x-lag(.x))==0  ~ "U",
    (.x-lag(.x))==1  ~ "C"),
    .names="change{.col}")) %>%
  #rowwise for c_across()
  rowwise() %>%
  #compute richness and # colonized and went extinct at each time measurement
  mutate(richness=sum(c_across(!c(day,starts_with("change")))),
    colonize=sum(c_across(starts_with("change"))=="C"),
    extinct=sum(c_across(starts_with("change"))=="E")) %>%
  ungroup() %>%
  #retain day and summary values only
  select(day,richness:extinct) %>%
  mutate(day=as.integer(day)) %>%
  #convert colonize and extinct to rates
  mutate(day_change=day-lag(day),
    colonize_change=colonize/day_change,
    extinct_change=extinct/day_change) -> e1_sum


simberloff[[6]] %>% 
  #use rowwise to differentiate "Gen. Sp" in multiple orders
  rowwise() %>%
  mutate(Taxa=paste0(`Tax. Unit 1`[Taxa=="Gen. Sp."],`Tax. Unit 2`[Taxa=="Gen. Sp."],Taxa)) %>% 
  #retain taxa and all post-defaunation measures
  select(-c(PRE,`Tax. Unit 1`:Island)) %>%
  #set initial values to 0
  mutate(`0`=0,.before="21") %>%
  #pivot twice as a way to transpose data
  pivot_longer(cols=!Taxa,names_to="day",values_to="p_a") %>% 
  pivot_wider(names_from="Taxa",values_from="p_a") %>% 
  #'duplicate' data to assign whether a species underwent an extinction or colonization event
  mutate(across(!day,~case_when(
    (.x-lag(.x))==-1 ~ "E",
    (.x-lag(.x))==0  ~ "U",
    (.x-lag(.x))==1  ~ "C"),
    .names="change{.col}")) %>%
  #rowwise for c_across()
  rowwise() %>%
  #compute richness and # colonized and went extinct at each time measurement
  mutate(richness=sum(c_across(!c(day,starts_with("change")))),
         colonize=sum(c_across(starts_with("change"))=="C"),
         extinct=sum(c_across(starts_with("change"))=="E")) %>%
  ungroup() %>%
  #retain day and summary values only
  select(day,richness:extinct) %>%``
  mutate(day=as.integer(day)) %>%
  #convert colonize and extinct to rates
  mutate(day_change=day-lag(day),
         colonize_change=colonize/day_change,
         extinct_change=extinct/day_change) -> e3_sum






e1_sum %>%
  select(richness,colonize_change,extinct_change) %>%
  rename_with(.cols=ends_with("change"),~str_remove(.x,"_change$")) %>%
  pivot_longer(cols=!richness,
               names_to="parameter",
               values_to="rate") -> e1_s_rate

e2_sum %>%
  select(richness,colonize_change,extinct_change) %>%
  rename_with(.cols=ends_with("change"),~str_remove(.x,"_change$")) %>%
  pivot_longer(cols=!richness,
               names_to="parameter",
               values_to="rate") -> e2_s_rate


e1_sum %>%
  ggplot(aes(x=day,y=richness)) +
  geom_point()

e1_s_rate %>%
  filter(parameter=="colonize") %>%
  ggplot(aes(x=richness,y=rate,color=parameter)) +
  geom_point() +
  scale_color_viridis_d(end=0.8) +
  theme_bw()

e2_s_rate %>%
  ggplot(aes(x=richness,y=rate,color=parameter)) +
  geom_point() +
  scale_color_viridis_d(end=0.8) +
  theme_bw()
  
  



## Plot the data (not necessary but helpful)


## Develop functions/models that fit data



## Use these functions in the shiny app











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
  
  
  
  
  



