## ===================================
## Additional file 2 Fig 02
## ===================================

source(file.path("scripts/settings.R"))

nameExperiment <- "LSM_constant_duration"
load(file.path("experiments", nameExperiment, "processedExpResults", "rawdat_combined_cum.Rdata"))
load(file.path("experiments", nameExperiment, "processedExpResults", "DataForAnalysis.Rdata"))

  rawdat_combined_redAggrSeeds <- rawdat_combined_red %>%
    dplyr::group_by(PostInterventionDays, LARVcov, EIR, Duration, developmentSurvival, developmentDuration, femaleEggsLaidByOviposit) %>%
    dplyr::summarize(Nv = mean(Nv), Nv0 = mean(Nv0), PR = mean(PR)) %>%
    mutate(LARVcovLabel = paste0("Coverage ", LARVcov, "%")) 
    
  
  rawdat_combined_redAggrSeeds$LARVcovLabel <- factor(rawdat_combined_redAggrSeeds$LARVcovLabel,
                                                      levels = unique(paste0("Coverage ", rawdat_combined_redAggrSeeds$LARVcov, "%")),
                                                      labels = unique(paste0("Coverage ", rawdat_combined_redAggrSeeds$LARVcov, "%"))
  )
  
  
  counterfactual <- rawdat_combined_redAggrSeeds %>% 
    ungroup() %>%
    filter(LARVcov==0) %>%
    dplyr::rename(Nv0_noLSM=Nv0) %>%
    dplyr::select(PostInterventionDays,Nv0_noLSM, EIR, Duration ,developmentSurvival, developmentDuration, femaleEggsLaidByOviposit)
  

  plotdat <- rawdat_combined_redAggrSeeds %>% 
    left_join(counterfactual) %>%  
    mutate(red =Nv0/ Nv0_noLSM) %>% 
    filter(LARVcov %in% c(20, 40, 80, 100) &
             PostInterventionDays <= 150 & Duration == 2)
 
  for (selected_EIR in c(3,10, 90)){
    
    tempdat = subset(plotdat, EIR==selected_EIR)
    scl = mean(  tempdat$Nv0[tempdat$PostInterventionDays==0]/ 10000, na.rm=TRUE)
  
  pall_Nv0_cov <- ggplot(data = tempdat) +
    geom_rect(xmin=0, xmax=60, ymin=-Inf, ymax=Inf, fill="grey", alpha=0.01) +
    geom_line(aes(x = PostInterventionDays, y = Nv0 / 10000, group = interaction(developmentSurvival, developmentDuration, femaleEggsLaidByOviposit)))+
    theme_cowplot() +
    background_grid(major =("xy"),minor =("xy"),size.major = 0.5,size.minor = 0.2)+
    geom_hline(yintercept = c(-Inf, Inf)) + geom_vline(xintercept = c(-Inf, Inf)) +
    facet_wrap(~LARVcovLabel, nrow = 1) +
    customTheme_noAngle+
    scale_y_continuous(sec.axis = sec_axis( ~ (1- . / scl) *100, breaks=seq(0,100,20), labels=seq(0,100,20), name = "% reduction")) +
    scale_x_continuous(breaks=seq(0,150,30),labels=seq(0,150,30)) +
    labs(y = "Emerging adult mosquitoes\n(Nv0/nHost)", 
         x = "Time after starting larviciding (days)",
         title=paste0("EIR = ",selected_EIR," ibpa\n")) 
  
  ggsave(paste0("A2_Fig2_EIR-", selected_EIR, ".png"), plot = pall_Nv0_cov, path = file.path("additional_figures"), width = 17, height = 7, device = "png")
  #ggsave(paste0("A2_Fig2_EIR-", selected_EIR, ".pdf"), plot = pall_Nv0_cov, path = file.path("additional_figures"), width = 17, height = 7, device = "pdf")
 
   }
  
