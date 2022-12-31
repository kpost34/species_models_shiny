#Created by Keith Post on 12/11/22





#--------------------------------------------------------------------------------------------------#
##### Define Server Function========================================================================
#--------------------------------------------------------------------------------------------------#
server<-function(input,output,session){
  

  ##### Island Biogeography=ib======================================================================
  #### UI--------------------------------------------------------------------------------------------
  ### Load IB mini-app with sidebar hidden
  hide("Sidebar")
  
  ### Show sidebar if any scenario is selected
  observeEvent(input$rad_scenario_ib,{
    show("Sidebar")
  })
  
  ### Dynamically display sidebar and main panel
  observeEvent(input$rad_scenario_ib,{
    updateTabsetPanel(inputId="input_tabset_ib",selected=input$rad_scenario_ib)
  })
  
  observeEvent(input$rad_scenario_ib, {
    updateTabsetPanel(inputId="out_tabset_ib",selected=input$rad_scenario_ib)
  })
  
  ##### Back-end------------------------------------------------------------------------------------
  #### Scenario 1: large vs small
  ### Rate plot
  ## Create rate DF
  sc1_rateDF_ib <- bind_rows(
    build_rate_static_df(island="small island",d=1000,a=1000,rate="Extinction"),
    build_rate_static_df(island="large island",d=1000,a=5000,rate="Extinction"),
    #note: area does not affect colonization rate
    build_rate_static_df(island="both islands",d=1000,a=1000,rate="Colonization")
  ) 
  
  
  ## Create eq rate/s* DF
  sc1_rate_eqDF_ib<-bind_rows(
    build_eq_df(island="small island",a=1000,d=1000),
    build_eq_df(island="large island",a=5000,d=1000)
  )



  ## Render plot
  output$plotly_sc1_rate_ib<-renderPlotly({
    build_rate_static_plot(sc1_rateDF_ib,eq_data=sc1_rate_eqDF_ib)
  })
  
  
  ### Species vs time plot
  ## Create DF
  sc1_spp1tDF_ib<-bind_rows(
    build_svt_static_df(isle="small island", d=1000,a=1000),
    build_svt_static_df(isle="large island", d=1000,a=5000)
  )

  ## Render plot
  output$plotly_sc1_spp_ib<-renderPlotly({
    build_svt_static_plot(data=sc1_spp1tDF_ib)
  })
  
  
  
  #### Scenario 2: near vs distant
  ## Create rate DF
  sc2_rateDF_ib <- bind_rows(
    build_rate_static_df(island="near island",d=1000,a=1000,rate="Colonization"),
    build_rate_static_df(island="distant island",d=5000,a=1000,rate="Colonization"),
    #note: distance does not affect extinction rate
    build_rate_static_df(island="both islands",d=1000,a=1000,rate="Extinction")
  )
  
  ## Create eq rate/s* DF
  sc2_rate_eqDF_ib<-bind_rows(
    build_eq_df(island="near island",a=1000,d=1000),
    build_eq_df(island="distant island",a=1000,d=5000)
  )
  
  ## Render plot
  output$plotly_sc2_rate_ib<-renderPlotly({
    build_rate_static_plot(sc2_rateDF_ib,eq_data=sc2_rate_eqDF_ib)
  })
  
  
  ### Species vs time plot
  ## Create DF
  sc2_spp1tDF_ib<-bind_rows(
    build_svt_static_df(isle="near island", d=1000,a=1000),
    build_svt_static_df(isle="distant island", d=5000,a=1000)
  )

  ## Render plot
  output$plotly_sc2_spp_ib<-renderPlotly({
    build_svt_static_plot(data=sc2_spp1tDF_ib)
  })
  
  
  #### Scenario 3: large, near vs small, distant
  ## Create rate DF
  sc3_rateDF_ib <- bind_rows(
    build_rate_static_df(island="large, near island",d=1000,a=5000,rate="Both"),
    build_rate_static_df(island="small, distant island",d=5000,a=1000,rate="Both")
  )
  
  ## Create eq rate/s* DF
  sc3_rate_eqDF_ib<-bind_rows(
    build_eq_df(island="large, near island",a=5000,d=1000),
    build_eq_df(island="small, distant island",a=1000,d=5000)
  )
  
  ## Render plot
  output$plotly_sc3_rate_ib<-renderPlotly({
    build_rate_static_plot(sc3_rateDF_ib,eq_data=sc3_rate_eqDF_ib)
  })
  
  
  ### Species vs time plot
  ## Create DF
  sc3_spp1tDF_ib<-bind_rows(
    build_svt_static_df(isle="large, near island", d=1000,a=5000),
    build_svt_static_df(isle="small, distant island", d=5000,a=1000)
  )

  ## Render plot
  output$plotly_sc3_spp_ib<-renderPlotly({
    build_svt_static_plot(data=sc3_spp1tDF_ib)
  })
  
  
  #### Scenario 4: large, distant vs small, near
  ## Create rate DF
  sc4_rateDF_ib <- bind_rows(
    build_rate_static_df(island="large, distant island",d=5000,a=5000,rate="Both"),
    build_rate_static_df(island="small, near island",d=1000,a=1000,rate="Both")
  )
  
  ## Create eq rate/s* DF
  sc4_rate_eqDF_ib<-bind_rows(
    build_eq_df(island="large, distant island",a=5000,d=5000),
    build_eq_df(island="small, near island",a=1000,d=1000)
  )
  
  ## Render plot
  output$plotly_sc4_rate_ib<-renderPlotly({
    build_rate_static_plot(sc4_rateDF_ib,eq_data=sc4_rate_eqDF_ib)
  })
  
  
  ### Species vs time plot
  ## Create DF
  sc4_spp1tDF_ib<-bind_rows(
    build_svt_static_df(isle="large, distant island", d=5000,a=5000),
    build_svt_static_df(isle="small, near island", d=1000,a=1000)
  )

  ## Render plot
  output$plotly_sc4_spp_ib<-renderPlotly({
    build_svt_static_plot(data=sc4_spp1tDF_ib)
  })
  
  
  
  #### Custom specifications
  ### Rate plot
  ## Island 1
  # Create reactive object of C & E vs s
  rate1DF_ib<-reactive({
    build_rate_df(island="Island 1",s_len=300,c=input$sld_c1_ib, p=input$sld_p_ib, phi=input$sld_phi1_ib,
                  d=input$num_d1_ib, ep=input$sld_ep1_ib, a=input$num_a1_ib)
  })
  
  
  # Create reactive object of s*
  rate_eq1DF_ib<-reactive({
    build_eq_df(island="Island 1",c=input$sld_c1_ib, p=input$sld_p_ib, phi=input$sld_phi1_ib,
                d=input$num_d1_ib, ep=input$sld_ep1_ib, a=input$num_a1_ib)
  })
  
  
  
  ## Island 2
  # Create reactive object of C & E vs s
  rate2DF_ib<-reactive({
    build_rate_df(island="Island 2",s_len=300,c=input$sld_c2_ib, p=input$sld_p_ib, phi=input$sld_phi2_ib,
                d=input$num_d2_ib, ep=input$sld_ep2_ib, a=input$num_a2_ib)
  })
  
  
  # Create reactive object of s*
  rate_eq2DF_ib<-reactive({
    build_eq_df(island="Island 2",c=input$sld_c2_ib, p=input$sld_p_ib, phi=input$sld_phi2_ib,
                d=input$num_d2_ib, ep=input$sld_ep2_ib, a=input$num_a2_ib)
  })
  
  

  ## Render plot
  output$plotly_rate_ib<-renderPlotly({
    build_rate_plot(data1a=rate1DF_ib(),data1b=rate_eq1DF_ib(),sec_isle=input$rad_is2_ib,
                    data2a=rate2DF_ib(),data2b=rate_eq2DF_ib())
  })
            
    
  
  ### Species over time plot
  ## Reactive object
  spp1tDF_ib<-reactive({
    build_svt_df(isle=1, t=input$sld_t_ib, c=input$sld_c1_ib, p=input$sld_p_ib, 
                 phi=input$sld_phi1_ib, d=input$num_d1_ib, ep=input$sld_ep1_ib, a=input$num_a1_ib)
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
  
  
  ## Render plot
  # output$plotly_spp_ib<-renderPlotly({
  #   spp1tDF_ib() %>%
  #     ggplot() +
  #     geom_point(aes(x=t,y=s)) +
  #     labs(x="Time",
  #          y="Species richness of island") +
  #     theme_bw() -> p_spp1_ib
  # 
  # p_spp1_ib %>%
  #   ggplotly()
  # 
  # })
  
  
  
  ##### Create second navbarMenu (Species-Area Curves=sa)===========================================

  
  ##### Create third navbarMenu (Rarefaction=rf)====================================================
        
               
               
               



}




