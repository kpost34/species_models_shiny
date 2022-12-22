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
    build_rate_df(island=1,s_len=300,c=input$sld_c1_ib, p=input$sld_p_ib, phi=input$sld_phi1_ib,
                  d=input$num_d1_ib, ep=input$sld_ep1_ib, a=input$num_a1_ib)
  })
  
  
  # Create reactive object of s*
  rate_eq1DF_ib<-reactive({
    build_eq_df(island=1,c=input$sld_c1_ib, p=input$sld_p_ib, phi=input$sld_phi1_ib,
                d=input$num_d1_ib, ep=input$sld_ep1_ib, a=input$num_a1_ib)
  })
  
  
  
  ## Island 2
  # Create reactive object of C & E vs s
  rate2DF_ib<-reactive({
    build_rate_df(island=2,s_len=300,c=input$sld_c2_ib, p=input$sld_p_ib, phi=input$sld_phi2_ib,
                d=input$num_d2_ib, ep=input$sld_ep2_ib, a=input$num_a2_ib)
  })
  
  
  # Create reactive object of s*
  rate_eq2DF_ib<-reactive({
    build_eq_df(island=2,c=input$sld_c2_ib, p=input$sld_p_ib, phi=input$sld_phi2_ib,
                d=input$num_d2_ib, ep=input$sld_ep2_ib, a=input$num_a2_ib)
  })
  
  

  ## Render plot
  output$plotly_rate_ib<-renderPlotly({
    build_rate_plot(data1a=rate1DF_ib(),data1b=rate_eq1DF_ib(),sec_isle=input$rad_is2_ib,
                    data2a=rate2DF_ib(),data2b=rate_eq2DF_ib())
  })
            
    
  
  ### Species over time plot
  ## Island 1
  # Reactive object
  # spp1DF<-reactive({
  #   s<-c(0,rep(NA,input$sld_t_ib-1))
  #   
  #   for(i in 1:input$sld_t_ib){
  #     s[i+1]<-s[i] + input$sld_c1_ib*(input$sld_p_ib-s[i])*
  #       exp(-input$sld_phi1_ib*input$num_d1_ib)-s[i]*exp(-input$sld_ep1_ib*input$num_a1_ib)
  #   }
  #   tibble(t=0:input$sld_t_ib,
  #          s=signif(s,3))
  # })
  
  # Reactive object
  spp1tDF_ib<-reactive({
    build_svt_df(isle=1, t=input$sld_t_ib, c=input$sld_c1_ib, p=input$sld_p_ib, 
                 phi=input$sld_phi1_ib, d=input$num_d1_ib, ep=input$sld_ep1_ib, a=input$num_a1_ib)
  })
  
  

  # Render plot
  output$plotly_spp_ib<-renderPlotly({
    spp1tDF_ib() %>%
      ggplot() +
      geom_point(aes(x=t,y=s)) +
      labs(x="Time",
           y="Species richness of island") +
      theme_bw() -> p_spp1_ib

  p_spp1_ib %>%
    ggplotly()

  })
  

  
  ## Island 2
  # Reactive object
  spp2tDF_ib<-reactive({
    build_svt_df(isle=2,t=input$sld_t_ib, c=input$sld_c2_ib, p=input$sld_p_ib,
                 phi=input$sld_phi2_ib, d=input$num_d2_ib, ep=input$sld_ep2_ib, a=input$num_a2_ib)
  })
  
  
  # Render plot
  output$plotly_spp_ib<-renderPlotly({
    build_svt_plot(data1=spp1tDF_ib(), sec_isle=input$rad_is2_ib, data2=spp2tDF_ib())
  })
  
  
  
  
  
  ##### Create second navbarMenu (Species-Area Curves=sa)===========================================

  
  ##### Create third navbarMenu (Rarefaction=rf)====================================================
        
               
               
               



}




