#Created by Keith Post on 12/11/22







#--------------------------------------------------------------------------------------------------#
##### Define Server Function========================================================================
#--------------------------------------------------------------------------------------------------#
server<-function(input,output,session){
  

  ##### Island Biogeography=ib)=====================================================================
  #### UI--------------------------------------------------------------------------------------------
  
  
  #### Back-end--------------------------------------------------------------------------------------
  ### Rate plot
  ## Island 1
  # Create reactive object of C & E vs s
  rate1DF_ib<-reactive({
    tibble(s1=seq(0,input$sld_p1_ib,length.out=300),
           C1=input$sld_c1_ib*(input$sld_p1_ib-s1)*exp(-input$sld_phi1_ib*input$num_d1_ib),
           E1=s1*exp(-input$sld_ep1_ib*input$num_a1_ib)) %>%
      mutate(across(everything(),~round(.x,2)))
  })
  
  # Create reactive object of s*
  rate_eq1DF<-reactive({
    tibble(s_eq1=
    (input$sld_c1_ib*input$sld_p1_ib*exp(input$sld_ep1_ib*input$num_a1_ib))/
      ((input$sld_c1_ib*exp(input$sld_ep1_ib*input$num_a1_ib))+(exp(input$sld_phi1_ib*input$num_d1_ib)))) %>%
      mutate(rate_eq1=s_eq1*exp(-input$sld_ep1_ib*input$num_a1_ib),
             across(everything(),~signif(.x,3)))
  })
  
  
  ## Island 2
  # Create reactive object of C & E vs s
  rate2DF_ib<-reactive({
  tibble(s2=seq(0,input$sld_p1_ib,length.out=250),
         C2=input$sld_c2_ib*(input$sld_p1_ib-s2)*exp(-input$sld_phi2_ib*input$num_d2_ib),
         E2=s2*exp(-input$sld_ep2_ib*input$num_a2_ib)) %>%
    mutate(across(everything(),~round(.x,2)))
  })
  
    
  # Create reactive object of s*
  rate_eq2DF<-reactive({
    tibble(s_eq2=
    (input$sld_c2_ib*input$sld_p1_ib*exp(input$sld_ep2_ib*input$num_a2_ib))/
      ((input$sld_c2_ib*exp(input$sld_ep2_ib*input$num_a2_ib))+(exp(input$sld_phi2_ib*input$num_d2_ib)))) %>%
      mutate(rate_eq2=s_eq2*exp(-input$sld_ep2_ib*input$num_a2_ib),
             across(everything(),~signif(.x,3)))
  })
  
  
  ## Render plots 
  output$plotly_rate_ib<-renderPlotly({
    rate1DF_ib() %>%
      ggplot(aes(x=s1,y=C1)) +
      geom_line(aes(label=C1),color="red4") +
      geom_line(aes(y=E1,label=E1),color="red") +
      geom_point(data=rate_eq1DF(),
                 aes(x=s_eq1,y=rate_eq1,label=s_eq1),color="black") +
      geom_segment(data=rate_eq1DF(),
                   aes(x=s_eq1,xend=s_eq1,y=0,yend=rate_eq1,label=rate_eq1),
                   color="purple",linetype="dotted") +
      labs(x="Species richness of island(s)",y="Colonization/extinction rate (sp/time)") +
      theme_bw() -> p_rate1_ib
    
    if(input$rad_is2_ib=="yes") {
      p_rate1_ib +
        geom_line(data=rate2DF_ib(),aes(x=s2,y=C2,label=C2),color="blue4",linetype="dashed") +
        geom_line(data=rate2DF_ib(),aes(x=s2,y=E2,label=E2),color="blue",linetype="dashed") +
        geom_point(data=rate_eq2DF(),
                  aes(x=s_eq2,y=rate_eq2,label=s_eq2),color="black") +
        geom_segment(data=rate_eq2DF(),
                   aes(x=s_eq2,xend=s_eq2,y=0,yend=rate_eq2,label=rate_eq2),
                   color="darkgreen",linetype="dotted") -> p_rate_ib
    }
    
    else if(input$rad_is2_ib=="no") {
      p_rate1_ib -> p_rate_ib
    }
  
    p_rate_ib %>%
      ggplotly(tooltip="label")
  })
  
  
  ### Species over time plot
  ## Reactive objects
  # Island 1
  spp1DF<-reactive({
    s<-c(0,rep(NA,input$sld_t_ib-1))
    
    for(i in 1:input$sld_t_ib){
      s[i+1]<-s[i] + input$sld_c1_ib*(input$sld_p1_ib-s[i])*
        exp(-input$sld_phi1_ib*input$num_d1_ib)-s[i]*exp(-input$sld_ep1_ib*input$num_a1_ib)
    }
    tibble(t=0:input$sld_t_ib,
           s=signif(s,3))
  })
  
  
  ## Render plot
  # Island 1
  output$plotly_spp_ib<-renderPlotly({
    spp1DF() %>%
      ggplot() +
      geom_point(aes(x=t,y=s)) +
      labs(x="Time",
           y="Species richness of island(s)") +
      theme_bw() -> p_spp1_ib

  p_spp1_ib %>%
    ggplotly()

  })
  

  
  
  ##### Create second navbarMenu (Species-Area Curves=sa)===========================================

  
  ##### Create third navbarMenu (Rarefaction=rf)====================================================
        
               
               
               



}




