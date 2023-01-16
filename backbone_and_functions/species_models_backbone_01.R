#Created by Keith Post on 12/3/22
#Backbone code for Species Models App: Theory of Island Biogeography

pacman::p_load(here,tidyverse,plotly,ggforce)
source(here("backbone_and_functions","species_models_func_01.R"))

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

# S vs t
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



### Plots with one d value
d<-100
a<-5000

## Rate plot-Island 1
# Create DF
tibble(island=rep("1",300),
       s=seq(0,p,length.out=300),
      Colonization=c*(p-s)*exp(-phi*d),
      Extinction=s*exp(-ep*a)) %>%
  pivot_longer(Colonization:last_col(),names_to="rate",values_to="value") %>%
  mutate(across(c(s,value),~signif(.x,3))) -> island3DF

# Equilibrium DF
s_eqDF_3<-tibble(island="1",
               s_eq=(c*p*exp(ep*a))/(c*exp(ep*a) + exp(phi*d)),
               rate_eq=s_eq*exp(-ep*a)) %>%
  mutate(across(!island,~signif(.x,3)))


## Rate plot-Island 2
d<-50
a<-1000

# Create DF
tibble(island=rep("2",300),
       s=seq(0,p,length.out=300),
      Colonization=c*(p-s)*exp(-phi*d),
      Extinction=s*exp(-ep*a)) %>%
  pivot_longer(Colonization:last_col(),names_to="rate",values_to="value") %>%
  mutate(across(c(s,value),~signif(.x,3))) -> island4DF


# Equilibrium DF
s_eqDF_4<-tibble(island="2",
               s_eq=(c*p*exp(ep*a))/(c*exp(ep*a) + exp(phi*d)),
               rate_eq=s_eq*exp(-ep*a)) %>%
  mutate(across(!island,~signif(.x,3)))

  



# Develop plot
ndf<-2

island3DF %>%
  ggplot(aes(x=s,y=value)) +
  geom_line(aes(group=rate,linetype=rate,color=island,
                text=paste0("Island ",island,
                            "\n",rate,": ",value," spp/t",
                            "\n","Species: ",s))) +
  scale_color_manual(values=c("1"="red4"),guide="none") +
  scale_linetype_manual(values=c("Colonization"="solid",
                                 "Extinction"="dashed")) +
  geom_point(data=s_eqDF_3,
             aes(x=s_eq,y=rate_eq,
                 text=paste0("Island ",island,
                            "\n","rate*: ",rate_eq, " spp/time",
                            "\n", "s*: ",s_eq))) +
  geom_point(data=s_eqDF_3,
           aes(x=s_eq,y=0,
               text=paste0("Island ",island,
                          "\n", "s*: ",s_eq)),
           size=1) +
  geom_segment(data=s_eqDF_3,
               aes(x=s_eq,xend=s_eq,y=0,yend=rate_eq),
               linetype="dotted",color="purple") +
  labs(x="Species richness of island",
       y="Colonization/extinction rate (spp/time)") +
  theme_bw() -> island3_plot


if(ndf==2) {
  island3_plot +
    geom_line(data=island4DF,
              aes(x=s,y=value,group=rate,linetype=rate,color=island,
                  text=paste0("Island ",island,
                  "\n",rate,": ",value," spp/t",
                  "\n","Species: ",s))) +
    scale_color_manual(values=c("1"="red4","2"="blue4")) +
    geom_point(data=s_eqDF_4,
               aes(x=s_eq,y=rate_eq,
                   text=paste0("Island ",island,
                               "\n","rate*: ",rate_eq, " spp/time",
                               "\n", "s*: ",s_eq)),
               color="black") +
    geom_point(data=s_eqDF_4,
               aes(x=s_eq,y=0,
                   text=paste0("Island ",island,
                               "\n", "s*: ",s_eq)),
                   color="black") +
    geom_segment(data=s_eqDF_4,
                 aes(x=s_eq,xend=s_eq,y=0,yend=rate_eq),
                     linetype="dotted",color="darkgreen") -> island34_plot
    } else if(ndf==1) {
      island3_plot -> island34_plot
    }

  island34_plot


island34_plot %>%
  ggplotly(tooltip="text") %>%
    layout(margin=list(b=120),
      #put legend below plot
      legend=list(orientation="h",xanchor="center",yanchor="bottom",
                  x=0,y=-0.3))


## Plot s over time (with d=100)
#General form: S(t+1) = S(t) + C(t) - E(t)
#S(t+1) = S(t) + c(p-s)*exp(-phi*d) - s*exp^(-ep*a)
# Build DF
s<-c(0,rep(NA,99))

for(i in 1:100){
  s[i+1]<-s[i] + c*(p-s[i])*exp(-phi*d)-s[i]*exp(-ep*a)
}


s_tTab<-tibble(island=rep("island 1",101),
               t=0:100,
               s)


d<-100
a<-5000

s<-c(0,rep(NA,99))

for(i in 1:100){
  s[i+1]<-s[i] + c*(p-s[i])*exp(-phi*d)-s[i]*exp(-ep*a)
}

s_tTab2<-tibble(island=rep("island 2",101),
                t=0:100,
                s)

sec_isle<-"yes"

# Plot output
s_tTab %>%
  {if(sec_isle=="yes") bind_rows(.,s_tTab2) else .} %>%
  ggplot() +
  geom_point(aes(x=t,y=s)) +
  theme_bw()



### Polygon plot
## Simple schematic
# Set objects
reps<-104

d1<-10000
a1<-10000

d2<-100
a2<-100

p<-100

mainDF<-tibble(
  mainx=c(0,20,
          rep(c(19.75,20),50),
          0,0),
  mainy=c(0,
          seq(0,100,length.out=101),
          100,0),
  p=rep(p,reps),
  island1=rep("large, far",reps),
  d1=rep(d1,reps),
  a1=rep(a1,reps),
  island2=rep("sm, near",reps),
  d2=rep(d2,reps),
  a2=rep(a2,reps)
  ) %>%
  mutate(across(starts_with("a"),~log2(.x)),
         across(starts_with("d"),~sqrt(.x)))


xref<-20

yref1<-70
yref2<-30


# Plot
mainDF %>%
  ggplot() +
  #wavy vertical line to designate mainland boundary with text
  geom_polygon(aes(x=mainx,y=mainy),
               fill="burlywood",alpha=0.8) +
  annotate("text",x=10,y=50,label="Mainland") +
  #island 1 shape and text
  geom_circle(data=. %>% filter(mainx==xref,mainy==yref1),
              aes(x0=mainx+d1+a1,y0=yref1,r=a1),
              fill="green1",alpha=0.3) +
  geom_text(data=. %>% filter(mainx==xref,mainy==yref1),
            aes(x=mainx+d1+a1,y=yref1+3,
                label=paste0(island1,
                             "\n","a = ",round(2^a1,0))),
            size=3.5) +
  #line segment indicating distance between island 1 and mainland
  geom_segment(data=. %>% filter(mainx==xref,mainy==yref1),
               aes(x=mainx,xend=.95*(mainx+d1),y=yref1,yend=yref1),
               arrow=arrow(length=unit(0.2,"cm"))) +
  geom_text(data=. %>% filter(mainx==xref,mainy==yref1),
            aes(x=mainx+.5*d1,y=yref1+3,
                label=round(d1^2,0)),
            hjust=0.5) +
  #island 2 shape and text
  geom_circle(data = . %>% filter(mainx==xref,mainy==yref2),
              aes(x0=mainx+d2+a2,y0=yref2,r=a2),
              fill="green1",alpha=0.3) +
  geom_text(data= . %>% filter(mainx==xref,mainy==yref2),
            aes(x=mainx+d2+a2,y=yref2,
                label=paste0(island2,
                             "\n","a = ",round(2^a2,0))),
            size=3.5) +
  #island 2 distance to mainland
  geom_segment(data=. %>% filter(mainx==xref,mainy==yref2),
               aes(x=mainx,xend=.95*(mainx+d2),y=yref2,yend=yref2),
               arrow=arrow(length=unit(0.2,"cm"))) +
  geom_text(data=. %>% filter(mainx==xref,mainy==yref2),
            aes(x=mainx+.5*a2,y=yref2+3,
                label=round(d2^2,0)),
          hjust=0.5) +
  xlim(c(0,150)) +
  ylim(c(0,100)) +
  theme_void() +
  labs(caption=paste("Note: island areas log2-transformed",
                     "\ndistances sqrt-transformed")) +
  theme(plot.caption=element_text(face="italic",hjust=0,size=9))


## Plot simple schematic via functions
mainDF<-build_schematic_df(nm=c("island 1","island 2"),a1=1000,d1=1000,sec_isle=TRUE,
                           a2=2000,d2=500)

mainDF %>%
  make_island_schematic(sec_isle=TRUE)

