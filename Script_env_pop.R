library(aspe)
library(tidyverse)
library(ade4)

load("~/Analyses_stat/En_cours/aspe_habitat/tables_sauf_mei_2024_01_22_12_13_56.RData")

passerelle <- mef_creer_passerelle()

env_pop <- point_prelevement %>% 
  select(pop_id,pop_sta_id,
         pop_code_sandre,
         pop_altitude,
         pop_largeur_lit_mineur,
         pop_distance_source,
         pop_pente_ign_cours_eau,
         pop_surface_bassin_versant_amont) %>% 
  na.omit()
  
m_env_pop <- env_pop %>% 
  select(pop_altitude:pop_surface_bassin_versant_amont)

acp_env_pop <- dudi.pca(m_env_pop, scannf = FALSE, nf = 2)

scores_var_acp_env_pop <- acp_env_pop$co
scores_ind_acp_env_pop <- acp_env_pop$li

s.corcircle(acp_env_pop$co)
s.label(acp_env_pop$li)

env_pop_synt <- env_pop %>% 
  bind_cols(scores_ind_acp_env_pop) %>% 
  rename(Indice_gradient = Axis1,
         Indice_pente = Axis2) %>%
  select(-pop_altitude,
         -pop_largeur_lit_mineur,
         -pop_distance_source,
         -pop_pente_ign_cours_eau,
         -pop_surface_bassin_versant_amont)
