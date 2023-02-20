#Created by Keith Post on 12/11/22





#--------------------------------------------------------------------------------------------------#
##### Define Server Function========================================================================
#--------------------------------------------------------------------------------------------------#
server<-function(input,output,session){
  

  ##### Island Biogeography (=ib)===================================================================
  #### UI-------------------------------------------------------------------------------------------
  ### Load IB mini-app with sidebar hidden
  hide("sidebar_ib")
  
  ### Show sidebar if custom or specific scenario is selected and hide otherwise
  observeEvent(input$rad_scenario1_ib,{
    if(input$rad_scenario1_ib=="custom"){
      show("sidebar_ib")
    }
  })
  
  observeEvent(input$rad_scenario2_ib,{
    show("sidebar_ib")
  })
  
  observeEvent(input$rad_scenario1_ib,{
    if(input$rad_scenario1_ib=="scenario"){
      hide("sidebar_ib")
    }
  })
  
  
  ### Dynamically display second set of radio buttons
  output$ui_rad_scenarios2_ib<-renderUI({
    req(input$rad_scenario1_ib=="scenario")
    radioButtons(inputId="rad_scenario2_ib",choices=scenarios2_ib,selected=character(0),
                 inline=TRUE,label="Select a scenario")
    })
  
  
  ### Dynamically display sidebar and main panel
  observeEvent(input$rad_scenario1_ib,{
    updateTabsetPanel(inputId="input_tabset_ib",selected=input$rad_scenario1_ib)
  })
  
  observeEvent(input$rad_scenario1_ib, {
    updateTabsetPanel(inputId="out_tabset_ib",selected=input$rad_scenario1_ib)
  })
  
  
  ### Reset button
  #resets all values in sidebar (custom UI panel)
  observeEvent(input$reset_ib, {
    reset("sidebar_ib")
  })
  
  
  ### Toggle plot outputs of custom settings
  observeEvent(input$chk_schematicOut_ib,{
    toggle("plot_cust_schematic_ib")
  },ignoreInit=TRUE)

  observeEvent(input$chk_rateOut_ib,{
    toggle("plotly_cust_rate_ib")
  },ignoreInit=TRUE)
  
  observeEvent(input$chk_svtOut_ib,{
    toggle("plotly_cust_sppt_ib")
  },ignoreInit=TRUE)

  


  
  ##### Back-end------------------------------------------------------------------------------------
  #### Scenarios
  ### Set up switches for text sidebar, schematic DF, rate DF, eq rate DF, & spp v time DF to create reactive objects
  ## Text sidebar
  #display different text in sidebar when sc1-4 is selected
  sc1_4_text_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
      "lvs"=sc1_text_ib,
      "nvd"=sc2_text_ib,
      "lnvsd"=sc3_text_ib,
      "ldvsn"=sc4_text_ib)
  })
  
  
  ## Schematic DF
  #build reactive DF for schematic plot depending on sc1-4
  sc1_4_schematicDF_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
      "lvs"=build_schematic_df(nm=c("large","small"),a1=5000,d1=1000,sec_isle="yes",a2=1000,d2=1000),
      "nvd"=build_schematic_df(nm=c("near","distant"),a1=1000,d1=1000,sec_isle="yes",a2=1000,d2=5000),
      "lnvsd"=build_schematic_df(nm=c("large, near","small, distant"),
                                 a1=5000,d1=1000,sec_isle="yes",a2=1000,d2=5000),
      "ldvsn"=build_schematic_df(nm=c("large, distant","small, near"),
                                 a1=5000,d=5000,sec_isle="yes",a2=1000,d2=1000)
    )
  })
  
  ## Rate DF
  #build reactive DF for rate plot depending on sc1-4 
  sc1_4_rateDF_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
           "lvs"=sc1_rateDF_ib,
           "nvd"=sc2_rateDF_ib,
           "lnvsd"=sc3_rateDF_ib,
           "ldvsn"=sc4_rateDF_ib)
  })
  
  
  ## Eq Rate DF
  #same as directly above except for equilibrium points of rate plot
  sc1_4_rate_eqDF_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
           "lvs"=sc1_rate_eqDF_ib,
           "nvd"=sc2_rate_eqDF_ib,
           "lnvsd"=sc3_rate_eqDF_ib,
           "ldvsn"=sc4_rate_eqDF_ib)
  })
  
  
  ## Spp vs Time DF
  #build spp v time DF reactive object for sc1-4 depending on selection
  sc1_4_spptDF_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
           "lvs"=sc1_spptDF_ib,
           "nvd"=sc2_spptDF_ib,
           "lnvsd"=sc3_spptDF_ib,
           "ldvsn"=sc4_spptDF_ib)
  })
  
  
  ### Render functions to output text and plots
  ## Text
  output$text_sc1_4_text_ib<-renderText({
    sc1_4_text_ib()
  })
  
  
  ## Schematic Plot
  output$plot_sc1_4_schematic_ib<-renderPlot({
    req(input$rad_scenario2_ib)
    make_island_schematic(sc1_4_schematicDF_ib(),sec_isle="yes")
  })
  
  
  ## Rate Plotly Plot
  output$plotly_sc1_4_rate_ib<-renderPlotly({
    req(input$rad_scenario2_ib)
    build_rate_static_plot(rate_data=sc1_4_rateDF_ib(),eq_data=sc1_4_rate_eqDF_ib())
  })
  
  
  ## Spp v Time Plotly Plot
  output$plotly_sc1_4_sppt_ib<-renderPlotly({
    req(input$rad_scenario2_ib)
    build_svt_static_plot(data=sc1_4_spptDF_ib())
  })
  


  #### Custom specifications
  ### Schematic plot
  ## Create reactive object of schematic island DF
  schematicDF_ib<-reactive({
    build_schematic_df(nm=c("island 1","island 2"),a1=input$num_a1_ib,d1=input$num_d1_ib,
                       sec_isle=input$rad_is2_ib,a2=input$num_a2_ib,d2=input$num_d2_ib)
  })
  
  
  ## Render plotly plot
  output$plot_cust_schematic_ib<-renderPlot({
    make_island_schematic(schematicDF_ib(),sec_isle=input$rad_is2_ib)
  })
  
  
  ### Rate plot
  ## Island 1
  # Create reactive object of rate DF
  rate1DF_ib<-reactive({
    build_rate_df(island="Island 1",s_len=300,c=input$sld_c1_ib, p=input$sld_p_ib, phi=input$sld_phi1_ib,
                  d=input$num_d1_ib, ep=input$sld_ep1_ib, a=input$num_a1_ib)
  })


  # Create reactive object of eq rate DF
  rate_eq1DF_ib<-reactive({
    build_eq_df(island="Island 1",c=input$sld_c1_ib, p=input$sld_p_ib, phi=input$sld_phi1_ib,
                d=input$num_d1_ib, ep=input$sld_ep1_ib, a=input$num_a1_ib)
  })



  ## Island 2
  # Create separate reactive object of rate DF (for island 2)
  rate2DF_ib<-reactive({
    build_rate_df(island="Island 2",s_len=300,c=input$sld_c2_ib, p=input$sld_p_ib, phi=input$sld_phi2_ib,
                d=input$num_d2_ib, ep=input$sld_ep2_ib, a=input$num_a2_ib)
  })


  # Create reactive object of eq rate DF (for island 2)
  rate_eq2DF_ib<-reactive({
    build_eq_df(island="Island 2",c=input$sld_c2_ib, p=input$sld_p_ib, phi=input$sld_phi2_ib,
                d=input$num_d2_ib, ep=input$sld_ep2_ib, a=input$num_a2_ib)
  })



  ## Render plotly plot
  output$plotly_cust_rate_ib<-renderPlotly({
    build_rate_plot(data1a=rate1DF_ib(),data1b=rate_eq1DF_ib(),sec_isle=input$rad_is2_ib,
                    data2a=rate2DF_ib(),data2b=rate_eq2DF_ib())
  })



  ### Species over time plot
  ## Island 1
  # Reactive object
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


  ## Render plotly plot
  output$plotly_cust_sppt_ib<-renderPlotly({
    build_svt_plot(data1=spp1tDF_ib(), sec_isle=input$rad_is2_ib, data2=spp2tDF_ib())
  })
  
  

  
  
  
  ##### Species-Area Curves (=sa)===================================================================
  #### UI-------------------------------------------------------------------------------------------
  ### Dynamically display mainPanel tab based on sidebarPanel tab
  observeEvent(input$input_tabset_sa,{
    updateTabsetPanel(inputId="out_tabset_sa",selected=input$input_tabset_sa)
  })
  
  
  
  ### Reset button
  #resets all values in sidebar
  observeEvent(input$reset_sa, {
    reset("drawMod_tab_sa")
  })
  
  
  ### Toggle plot outputs of custom settings
  observeEvent(input$chk_plline_sa,{
    toggle("plot_draw_plline_sa")
  },ignoreInit=TRUE)

  observeEvent(input$chk_pllog_ib,{
    toggle("plot_draw_pllog_sa")
  },ignoreInit=TRUE)
  
  observeEvent(input$chk_semilog_sa,{
    toggle("plot_draw_semilog_sa")
  },ignoreInit=TRUE)
  
  
  #### Back-end-------------------------------------------------------------------------------------
  ### Curve drawing
  ## Create reactive df
  curve_drawDF_sa<-reactive({
    tibble(a=seq(10^input$sld_a_sa[1],10^input$sld_a_sa[2],length.out=20),
           log_a=log10(a)
    )
  })
  
  ## Power law
  #linear scale
  output$plot_draw_plline_sa<-renderPlot({
    curve_drawDF_sa() %>%
      plot_power_mod(c=input$sld_c_sa,z=input$sld_z_sa,col="darkgreen") 
  })
  
  
  #log-log scale
  output$plot_draw_pllog_sa<-renderPlot({
    curve_drawDF_sa() %>%
      plot_powerlog_mod(c=input$sld_c_sa,z=input$sld_z_sa,col="darkgreen")
  })
  
  
  ## Semilog model
  output$plot_draw_semilog_sa<-renderPlot({
    curve_drawDF_sa() %>%
      plot_semilog_mod(c=input$sld_c_sa,z=input$sld_z_sa,col="darkgreen")
  })
  
  
  ### Model fitting
  ## Create reactive df
  model_fitDF_sa<-reactive({
    switch(input$rad_dataset_sa,
      "aegean"=aegean,
      "aegean2"=aegean2,
      "galap"=galap,
      "niering"=niering)
  })
  
  ## Create models
  # Power law: linear (nls)
  # pl_nls_mod<-reactive({
  #   nls(s~c*a^z,
  #     data=model_fitDF_sa(),
  #     start=list(c=5,z=.35)
  #   )
  # })
  
  # Power law: log (lm)
  pl_lm_mod<-reactive({
    model_fitDF_sa() %>%
      mutate(log_a=log10(a),
             log_s=log10(s)) %>%
      lm(formula=log_s~log_a)
  })
    
  
  # Semilog (lm)
  semilog_lm_mod<-reactive({
    model_fitDF_sa() %>%
      mutate(log_a=log10(a)) %>%
      lm(formula=s~log_a)
  })
    
    
  ## Create plots
  # Power law
  #log-log scale
  output$plotly_datamod_pllog_sa<-renderPlotly({
    req(input$rad_dataset_sa)
    model_fitDF_sa() %>%
      plot_powerlog_sars(reg=TRUE,col_reg="darkblue") -> p

    #tooltip displays values to fewer digits for data points and model without any
      #additional formatting/mutating/etc
    with_options(list(digits=2),
                 ggplotly(p) %>%
                   config(displaylogo=FALSE,
                          modeBarButtonsToRemove=list(
                            "toImage",
                            "hoverClosestCartesian",
                            "hoverCompareCartesian",
                            "autoScale2d",
                            "pan2d",
                            "lasso2d",
                            "zoom2d",
                            "select2d")
                    )
    )
  })
    
  
  # Semilog model
  output$plotly_datamod_semilog_sa<-renderPlotly({
    req(input$rad_dataset_sa)
    model_fitDF_sa() %>%
      plot_semilog_sars(reg=TRUE,col_reg="darkblue") -> p
    
    with_options(list(digits=2),
                 ggplotly(p) %>%
                  config(displaylogo=FALSE,
                         modeBarButtonsToRemove=list(
                          "toImage",
                          "hoverClosestCartesian",
                          "hoverCompareCartesian",
                          "autoScale2d",
                          "pan2d",
                          "lasso2d",
                          "zoom2d",
                          "select2d")
                  )
    )
  })
  
  
  ## Compare models
  output$dt_modcomp_sa<-renderDT({
    req(input$rad_dataset_sa)
    compare_sars_mods(pl_lm_mod(),semilog_lm_mod(),nm=c("Power Law","Semi-log Model"))
  },options=list(dom="t"))
  
  
  
  
  
  
  ##### Rarefaction (=rf)===========================================================================
  #### UI-------------------------------------------------------------------------------------------
  ### Species Accumulation
  ## Load IB mini-app with sidebar hidden
  hide("sidebar_rf")
  
  ## Show sidebar if custom or specific scenario is selected and hide otherwise
  observeEvent(input$rad_dataset_rf,{
    show("sidebar_rf")
  })
  
  ## Update slider input following dataset selection
  observeEvent(input$rad_dataset_rf,{
    if(input$rad_dataset_rf=="BCI"){
      updateSliderInput(inputId="sld_r_rf",value=20,max=40,step=5)
    }
    if(input$rad_dataset_rf=="dune"){
      updateSliderInput(inputId="sld_r_rf",value=10,max=15,step=1)
    }
    if(input$rad_dataset_rf=="mite"){
      updateSliderInput(inputId="sld_r_rf",value=30,max=60,step=5)
    }
    if(input$rad_dataset_rf=="sipoo"){
      updateSliderInput(inputId="sld_r_rf",value=10,max=15,step=1)
    }
  })
  
  
  #### Back-end-------------------------------------------------------------------------------------
  ### Species Accumulation
  ## Create reactive df
  specaccumDF_rf<-reactive({
    switch(input$rad_dataset_rf,
      "BCI"=BCI[sample(input$sld_r_rf),],
      "dune"=dune[sample(input$sld_r_rf),],
      "mite"=mite[sample(input$sld_r_rf),],
      "sipoo"=sipoo[sample(input$sld_r_rf),])
  })
  
  ## Create reactive object for differentiating individuals and sites
  specaccum_type_vec_rf<-reactive({
    req(input$rad_specaccumtype_rf)
    if(input$rad_specaccumtype_rf=="rarefaction"){
      "individuals"
    } 
    # else if(input$rad_specaccumtype_rf %in% c("collector","random","exact")){
    #   "sites"
    # }
    else{"sites"}
  })
  
  ## Create reactive specaccum df
  specaccum_curveDF_rf<-reactive({
    req(input$rad_specaccumtype_rf)
    specaccumDF_rf() %>%
      specaccum(method=input$rad_specaccumtype_rf) %>%
      # {if(input$rad_specaccumtype_rf=="rarefaction") .[c("individuals","richness")]
      #  else .[c("sites","richness")]} %>%
      .[c(specaccum_type_vec_rf(),"richness")] %>%
      bind_rows()
    })
  
  
  ## Create plots
  output$plotly_specaccum_rf<-renderPlotly({
    req(input$chk_specaccumplot_rf)
    specaccum_curveDF_rf() %>%
      ggplot(aes(x=!!sym(specaccum_type_vec_rf()),y=richness)) +
      ggtitle("Species-Accumulation Curve") +
      geom_point() +
      theme_bw() +
      theme(title=element_text(size=12)) -> p
    
    p %>%
      ggplotly()
  })
  
  
  ## Create table
  output$dt_estTotS_rf<-renderDT({
    req(input$chk_estTotS_rf)
    specaccumDF_rf() %>%
      est_s_as_tib()
  },
  options=list(dom="t"),
  #add caption as a centered title
  caption=htmltools::tags$caption(style="caption-side: top; text-align: left;
                                  color: black; font-size: 150%;",
                                  "Estimated Richness")
  )
               
               



}




