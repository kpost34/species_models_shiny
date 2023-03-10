#Created by Keith Post on 12/18/22
#Code for functions used in Species Models App for Island Biogeography mini-app


### Function to easily create multiple line breaks--------------------------------------------------
linebreaks <- function(n){HTML(strrep(br(), n))}


#### Create Function to Build DF of s, C, and E=====================================================
### Custom version
build_rate_df<-function(island,s_len,c,p,phi,d,ep,a){
  tibble(island=rep(paste(island),s_len),
         s=seq(0,p,length.out=s_len),
         Colonization=c*(p-s)*exp(-phi*d),
         Extinction=s*exp(-ep*a)) %>%
    pivot_longer(Colonization:last_col(),names_to="rate",values_to="value") %>%
    mutate(across(c(s,value),~signif(.x,3))) 
}

### Static version
build_rate_static_df<-function(island,d, a,rate){
  tibble(island=rep(paste(island),300),
         s=seq(0,100,length.out=300)) %>%
    #pipe depends on rate selected-[NOTE: two islands could share the same C or E if parameters for
      #them are the same]
    {if(rate=="Colonization") mutate(.,Colonization=0.6*(100-s)*exp(-.0002*d)) else .} %>%
    {if(rate=="Extinction") mutate(.,Extinction=s*exp(-.0004*a)) else .} %>%
    {if(rate=="Both") mutate(.,Colonization=0.6*(100-s)*exp(-.0002*d),
                               Extinction=s*exp(-.0004*a)) else .} %>%
    pivot_longer(!c(island,s),names_to="rate",values_to="value") %>%
    mutate(across(c(s,value),~signif(.x,3))) 
}


#### Create Function to Build DF of s* and rate*====================================================
build_eq_df<-function(island,c=0.6,p=100,ep=.0004,a,phi=.0002,d){
  tibble(island=paste(island),
               s_eq=(c*p*exp(ep*a))/(c*exp(ep*a) + exp(phi*d)),
               rate_eq=s_eq*exp(-ep*a)) %>%
  mutate(across(!island,~signif(.x,3)))
}



#### Create Function to Build Rate Plot=============================================================
### Custom version
build_rate_plot<-function(data1a,data1b,sec_isle="no",data2a=NA,data2b=NA){
  #island 1: rate plot
  data1a %>%
    ggplot(aes(x=s,y=value)) +
    geom_line(aes(group=rate,linetype=rate,color=island,
                  #text is for renderPlotly()/ggplotly() tooltip
                  text=paste0(island,
                              "\n",rate,": ",value," spp/t",
                              "\n","Species: ",s)),
              color="red4") +
    #set name=NULL to de-clutter legend 
    scale_color_manual(name=NULL,values=c("Island 1"="red4"),guide="none") +
    scale_linetype_manual(name=NULL,values=c("Colonization"="solid",
                                   "Extinction"="dashed")) +
    #island 1: eq points and segment
    geom_point(data=data1b,
               aes(x=s_eq,y=rate_eq,
                   text=paste0(island,
                              "\n","rate*: ",rate_eq," spp/t",
                              "\n", "s*: ",s_eq, " species")),
               color="brown") +
    geom_point(data=data1b,
               aes(x=s_eq,y=0,
               text=paste0(island,
                          "\n", "s*: ",s_eq, " species")),
               size=1,color="brown") +
    geom_segment(data=data1b,
                 aes(x=s_eq,xend=s_eq,y=0,yend=rate_eq),
                 linetype="dotted",color="purple") +
    labs(title="Island species richness and colonization/extinction rates",
         x="Species richness of island",
         y="Rate (spp/time)") +
    theme_bw() +
    theme(legend.position="bottom") -> rate1_p
  
if(sec_isle=="yes") {
  #island 2: rate plot
  rate1_p +
    geom_line(data=data2a,
              aes(x=s,y=value,group=rate,linetype=rate,color=island,
                  text=paste0(island,
                  "\n",rate,": ",value," spp/t",
                  "\n","Species: ",s))) +
    scale_color_manual(name=NULL,values=c("Island 1"="red4","Island 2"="blue4")) +
    geom_point(data=data2b,
               aes(x=s_eq,y=rate_eq,
                   text=paste0(island,
                               "\n","rate*: ",rate_eq, " spp/t",
                               "\n", "s*: ",s_eq, " species")),
               color="black") +
    #island 2: eq points and segment
    geom_point(data=data2b,
               aes(x=s_eq,y=0,
                   text=paste0(island,
                               "\n", "s*: ",s_eq, " species")),
                   size=1,color="black") +
    geom_segment(data=data2b,
                 aes(x=s_eq,xend=s_eq,y=0,yend=rate_eq),
                     linetype="dotted",color="darkgreen") -> rate12_p
    } else if(sec_isle=="no") {
        rate1_p -> rate12_p
    }

rate12_p %>%
  #tooltip set to "text" to pull in custom text associated with geoms above
  ggplotly(tooltip="text") %>%
    layout(margin=list(b=110),
           font="Helvetica",
      #put legend below plot
      legend=list(orientation="h",xanchor="center",yanchor="bottom",
                  x=0.5,y=-0.35))

}


### Static version
build_rate_static_plot<-function(rate_data,eq_data){
  #set order of islands based on how rate_data is built and so that "gray30" matches with "Both" 
    #(not present in all scenarios)
  rate_data %>%
    pull(island) %>%
    unique() -> nm
  
  col<-c("red4","blue4","gray30")
  names(col)<-nm
  
  #rate plot for scenarios
  rate_data %>%
    ggplot(aes(x=s,y=value)) +
    geom_line(aes(group=interaction(island,rate),linetype=rate,color=island,
                  #text is for tooltip display
                  text=paste0(island,
                              "\n",rate,": ",value," spp/t",
                              "\n","Species: ",s))) +
    scale_color_manual(name=NULL,values=col) +
    scale_linetype_manual(name=NULL,values=c("Colonization"="solid",
                                             "Extinction"="dashed")) +
    #equilibrium points and segment
    geom_point(data=eq_data,
               aes(x=s_eq,y=rate_eq,
                   text=paste0(island,
                              "\n","rate*: ",rate_eq," spp/t",
                              "\n", "s*: ",s_eq, " species")),
               color="black") +
    geom_point(data=eq_data,
               aes(x=s_eq,y=0,
                   text=paste0(island,
                               "\n", "s*: ",s_eq, " species")),
                   size=1,color="black") +
    geom_segment(data=eq_data,
                 aes(x=s_eq,xend=s_eq,y=0,yend=rate_eq),
                    color="gray80", linetype="dotted") +
    labs(title="Island species richness and colonization/extinction rates",
         x="Species richness of island",
         y="Rate (spp/time)") +
    theme_bw() -> rate_static_p
  
  rate_static_p %>%
    ggplotly(tooltip="text") %>%
    layout(margin=list(b=120),
           font="Helvetica",
      #put legend below plot
      legend=list(orientation="h",xanchor="center",yanchor="bottom",
                  x=0.5,y=-0.5)) %>%
    config(displayModeBar=FALSE)
}



#### Create Function to Build Spp v Time DF=========================================================
### Custom mode
build_svt_df<-function(isle, t, c, p, phi, d, ep, a) {
  s<-c(0,rep(NA,t-1))
    
  for(i in 1:t){
    s[i+1]<-s[i] + c*(p-s[i])*
    exp(-phi*d)-s[i]*exp(-ep*a)
  }
     
  tibble(island=rep(paste("Island",isle),t+1),
         t=0:t,
         s=signif(s,3))
}


### Static mode
build_svt_static_df<-function(isle, d, a) {
  s<-c(0,rep(NA,49))
    
  for(i in 1:50){
    s[i+1]<-s[i] + 0.6*(100-s[i])*
    exp(-.0002*d)-s[i]*exp(-.0004*a)
  }
     
  tibble(island=rep(isle,51),
         t=0:50,
         s=signif(s,3))
}

#### Create Function to Build Spp v Time Plot=======================================================
### Custom mode
build_svt_plot<-function(data1,sec_isle,data2){
  #if sec_isle is "yes" then DF adds in island 2 data
  data1 %>%
    {if(sec_isle=="yes") bind_rows(.,data2) else .} %>%
    ggplot() +
    geom_point(aes(x=t,y=s,shape=island,color=island,
                   #text is for tooltip
                   text=paste0(island,
                               "\nSpecies: ",s,
                               "\nTime: ",t))) +
    scale_color_manual(name=NULL,values=c("Island 1"="red4","Island 2"="blue4")) +
    scale_shape_manual(name=NULL,values=c("Island 1"=16,"Island 2"=18)) +
    labs(title="Island species richness over time",
         x="Time",
         y="Species richness of island") +
    theme_bw() +
    theme(legend.position="bottom")-> p
    
  p %>%
    ggplotly(tooltip="text") %>%
    layout(margin=list(b=110),
           font="Helvetica",
      #put legend below plot
      legend=list(orientation="h",xanchor="center",yanchor="bottom",
                  x=0.5,y=-0.35))
}


### Static mode
build_svt_static_plot<-function(data){
  #naming order matches how data are built
  data %>%
    pull(island) %>%
    unique() -> nm
  
  col<-c("red4","blue4")
  names(col)<-nm
  
  shp<-c(16,18)
  names(shp)<-nm
  
  data %>%
    ggplot() +
    geom_point(aes(x=t,y=s,shape=island,color=island,
                   text=paste0(island,
                               "\nSpecies: ",s,
                               "\nTime: ",t))) +
    scale_color_manual(name=NULL,values=col) +
    scale_shape_manual(name=NULL,values=shp) +
    labs(title="Island species richness over time",
         x="Time",
         y="Richness") +
    theme_bw() +
    theme(legend.position="bottom") -> p
    
  p %>%
    ggplotly(tooltip="text") %>%
    layout(margin=list(b=120),
           font="Helvetica",
      #put legend below plot
      legend=list(orientation="h",xanchor="center",yanchor="bottom",
                  x=0.5,y=-0.35)) %>%
    config(displayModeBar=FALSE)
}


#### Create Function to Build Schematic of Islands and Mainland=====================================
### Develop DF
build_schematic_df<-function(nm,a1,d1,sec_isle="no",a2,d2){
  nm_vec<-nm
  reps<-104
  
  tibble(
    mainx=c(0,20,
            rep(c(19.75,20),50),
            0,0),
    mainy=c(0,
            seq(0,100,length.out=reps-3),
            100,0),
    island1=rep(nm_vec[1],reps),
    a1=rep(a1,reps),
    d1=rep(d1,reps),
  ) %>%
    {if(sec_isle=="yes") bind_cols(.,
      tibble(
        island2=rep(nm_vec[2],reps),
        a2=rep(a2,reps),
        d2=rep(d2,reps),
      )
    ) else .} %>%
  mutate(across(starts_with("a"),~log2(.x)),
         across(starts_with("d"),~sqrt(.x)))
}


### Plot (draw) polygons
make_island_schematic<-function(data,sec_isle="no"){
# Set ref values
xref<-20

yref1<-70
yref2<-30


# Plot
data %>%
  ggplot() +
  #wavy vertical line to designate mainland boundary with text
  geom_polygon(aes(x=mainx,y=mainy),
               fill="burlywood",color="black",alpha=0.8) +
  annotate("text",x=10,y=50,label="Mainland",size=6,fontface=2) +
  #island 1 shape and text
  geom_circle(data=. %>% filter(mainx==xref,mainy==yref1),
              aes(x0=mainx+d1+a1,y0=yref1,r=a1,linewidth=0.5),
              fill="green1",color="darkred",alpha=0.3) +
  geom_text(data=. %>% filter(mainx==xref,mainy==yref1),
            aes(x=mainx+d1+a1,y=yref1,
                label=paste0(island1,
                             "\n","a = ",round(2^a1,0))),
            size=4.5) +
  #line segment indicating distance between island 1 and mainland
  geom_segment(data=. %>% filter(mainx==xref,mainy==yref1),
               aes(x=mainx,xend=.95*(mainx+d1),y=yref1,yend=yref1),
               arrow=arrow(length=unit(0.2,"cm")),
               color="darkred") +
  geom_text(data=. %>% filter(mainx==xref,mainy==yref1),
            aes(x=mainx+.2*d1,y=yref1+3,
                label=paste0("d = ",round(d1^2,0))),
            hjust=0.5,size=4.5) +
  xlim(c(0,150)) +
  ylim(c(0,100)) +
  theme_void() +
  labs(title="         Mainland-Island Schematic",
       caption=paste("Notes:",
                     "\n*island areas are log2-transformed",
                     "\n*distances are square root-transformed")) + 
  theme(plot.title=element_text(size=18),
        plot.caption=element_text(face="italic",hjust=0,size=13)) -> is1_plot

if(sec_isle=="no"){
  is1_plot -> is_plot
}

#second island
else if(sec_isle=="yes"){
  is1_plot +
  #island 2 shape and text
  geom_circle(data = . %>% filter(mainx==xref,mainy==yref2),
              aes(x0=mainx+d2+a2,y0=yref2,r=a2,linewidth=0.5),
              fill="green1",color="darkblue",alpha=0.3) +
  geom_text(data= . %>% filter(mainx==xref,mainy==yref2),
            aes(x=mainx+d2+a2,y=yref2,
                label=paste0(island2,
                             "\n","a = ",round(2^a2,0))),
            size=4.5) +
  #island 2 distance to mainland
  geom_segment(data=. %>% filter(mainx==xref,mainy==yref2),
               aes(x=mainx,xend=.95*(mainx+d2),y=yref2,yend=yref2),
               arrow=arrow(length=unit(0.2,"cm")),
               color="darkblue") +
  geom_text(data=. %>% filter(mainx==xref,mainy==yref2),
            aes(x=mainx+.2*d2,y=yref2+3,
                label=paste0("d = ",round(d2^2,0))),
          hjust=0.5,size=4.5) -> is_plot
}

return(is_plot)
}












