#Created by Keith Post on 12/11/22





#--------------------------------------------------------------------------------------------------#
##### Define Server Function========================================================================
#--------------------------------------------------------------------------------------------------#
server<-function(input,output,session){
  

  ##### Island Biogeography=ib)=====================================================================
  #### UI--------------------------------------------------------------------------------------------
  ### Scenarios 1-4
  # observeEvent(input$rad_scenario_ib,{
  #   if(input$rad_scenario_ib %in% scenarios_ib){
  #     hideTab(inputId="app_tabset_ib")
  #   }
  # })
  
  observeEvent(input$rad_scenario_ib, {
    updateTabsetPanel("out_tabset_ib",selected=input$rad_scenario_ib)
  })
# Scenario 1: large vs small islands

# Scenario 2: near vs distant islands

# Scenario 3: large, near vs small, distant islands

# Scenario 4: large, distant vs small, near islands
  
  ##### Back-end-------------------------------------------------------------------------------------
  #### Scenario 1: large vs small
  ### Rate plot
  ## Create rate DFs
  sc1_s_rateDF_ib<-build_rate_df(island="small",s_len=300,c=0.6,p=100,phi=.0002,d=1000,ep=.0004,a=1000)
  sc1_l_rateDF_ib<-build_rate_df(island="large",s_len=300,c=0.6,p=100,phi=.0002,d=1000,ep=.0004,a=5000)
  
  
  ## Create eq rate/s* DFs
  sc1_s_rate_eqDF_ib<-build_eq_df(island="small",c=0.6, p=100, phi=.0002,d=10000, ep=.0004, a=1000)
  sc1_l_rate_eqDF_ib<-build_eq_df(island="large",c=0.6, p=100, phi=.0002,d=10000, ep=.0004, a=5000)

  ## Render plot
  output$plotly_sc1_rate_ib<-renderPlotly({
    build_rate_plot(data1a=sc1_s_rateDF_ib,data1b=sc1_s_rate_eqDF_ib,sec_isle="yes",
                    data2a=sc1_l_rateDF_ib,data2b=sc1_l_rate_eqDF_ib)
  })
  
  ### Species v time plot
  
  
  #### Scenario 2: near vs distant
  
  
  
  #### Scenario 3: large, near vs small, distant
  
  
  
  #### Scenario 4: large, distant vs small, near
  
  
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
  
  

  ## Render plot
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




