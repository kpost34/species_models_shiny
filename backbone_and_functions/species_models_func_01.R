#Created by Keith Post on 12/18/22
#Code for functions used in Species Models App for Island Biogeography mini-app


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
  data1a %>%
    ggplot(aes(x=s,y=value)) +
    geom_line(aes(group=rate,linetype=rate,color=island,
                  text=paste0(island,
                              "\n",rate,": ",value," spp/t",
                              "\n","Species: ",s)),
              color="red4") +
    scale_color_manual(name=NULL,values=c("Island 1"="red4"),guide="none") +
    scale_linetype_manual(name=NULL,values=c("Colonization"="solid",
                                   "Extinction"="dashed")) +
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
    labs(x="Species richness of island",
         y="Colonization/extinction rate (spp/time)") +
    theme_bw() +
    theme(legend.position="bottom") -> rate1_p
  
if(sec_isle=="yes") {
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
  ggplotly(tooltip="text") %>%
    layout(margin=list(b=110),
      #put legend below plot
      legend=list(orientation="h",xanchor="center",yanchor="bottom",
                  x=0.5,y=-0.35))

}


### Static version
build_rate_static_plot<-function(rate_data,eq_data){
  rate_data %>%
    pull(island) %>%
    unique() %>%
    sort() %>%
    rev() -> nm
  
  col<-c("red4","blue4","gray30")
  names(col)<-nm
  
  rate_data %>%
    ggplot(aes(x=s,y=value)) +
    geom_line(aes(group=interaction(island,rate),linetype=rate,color=island,
                  text=paste0(island,
                              "\n",rate,": ",value," spp/t",
                              "\n","Species: ",s))) +
    scale_color_manual(name=NULL,values=col) +
    scale_linetype_manual(name=NULL,values=c("Colonization"="solid",
                                             "Extinction"="dashed")) +
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
    labs(x="Species richness of island",
         y="Colonization/extinction rate (spp/time)") +
    theme_bw() -> rate_static_p
  
  rate_static_p %>%
    ggplotly(tooltip="text") %>%
    layout(margin=list(b=150),
      #put legend below plot
      legend=list(orientation="h",xanchor="center",yanchor="bottom",
                  x=0.5,y=-0.5))
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
  data1 %>%
    {if(sec_isle=="yes") bind_rows(.,data2) else .} %>%
    ggplot() +
    geom_point(aes(x=t,y=s,shape=island,color=island,
                   text=paste0(island,
                               "\nSpecies: ",s,
                               "\nTime: ",t))) +
    scale_color_manual(name=NULL,values=c("Island 1"="red4","Island 2"="blue4")) +
    scale_shape_manual(name=NULL,values=c("Island 1"=16,"Island 2"=18)) +
    labs(x="Time",
         y="Species richness of island") +
    theme_bw() +
    theme(legend.position="bottom")-> p
    
  p %>%
    ggplotly(tooltip="text") %>%
    layout(margin=list(b=110),
      #put legend below plot
      legend=list(orientation="h",xanchor="center",yanchor="bottom",
                  x=0.5,y=-0.35))
}


### Static mode
build_svt_static_plot<-function(data){
  data %>%
    pull(island) %>%
    unique() %>%
    sort() %>%
    rev() -> nm
  
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
    labs(x="Time",
         y="Species richness of island") +
    theme_bw() +
    theme(legend.position="bottom") -> p
    
  p %>%
    ggplotly(tooltip="text") %>%
    layout(margin=list(b=110),
      #put legend below plot
      legend=list(orientation="h",xanchor="center",yanchor="bottom",
                  x=0.5,y=-0.35))
}


#### Create Function to Build Schematic of Islands and Mainland=====================================
### Develop DF
build_schematic_df<-function(nm,a1,d1,sec_isle=FALSE,a2,d2){
  reps<-104
  
  tibble(
    mainx=c(0,20,
            rep(c(19.75,20),50),
            0,0),
    mainy=c(0,
            seq(0,100,length.out=reps-3),
            100,0),
    island1=rep(nm[1],reps),
    a1=rep(a1,reps),
    d1=rep(d1,reps),
  ) %>%
    {if(sec_isle) bind_cols(.,
      tibble(
        island2=rep(nm[2],reps),
        a2=rep(a2,reps),
        d2=rep(d2,reps),
      )
    ) else .} %>%
  mutate(across(starts_with("a"),~log2(.x)),
         across(starts_with("d"),~sqrt(.x)))
}


### Plot (draw) polygons
make_island_schematic<-function(data,sec_isle=FALSE){
xref<-20

yref1<-70
yref2<-30


# Plot
data %>%
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
            aes(x=mainx+d1+a1,y=yref1,
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
            hjust=0.5) -> is1_plot

if(!sec_isle){
  is1_plot +
    xlim(c(0,150)) +
    ylim(c(0,100)) +
    theme_void() +
    labs(caption=paste("island areas log2-transformed",
                       "\ndistances sqrt-transformed")) +
    theme(plot.caption=element_text(face="italic",hjust=0,size=9)) ->is_plot
}


else if(sec_isle){
  is1_plot +
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
  labs(caption=paste("island areas log2-transformed",
                     "\ndistances sqrt-transformed")) +
  theme(plot.caption=element_text(face="italic",hjust=0,size=9)) -> is_plot
}

return(is_plot)
}












