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
    layout(margin=list(b=110),
      #put legend below plot
      legend=list(orientation="h",xanchor="center",yanchor="bottom",
                  x=0.5,y=-0.35))
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












