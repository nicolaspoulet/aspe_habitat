library(aspe)
library(tidyverse)

passerelle <- mef_creer_passerelle()

env <- passerelle %>%
  select(sta_id:ope_id) %>%
  distinct() %>%
  mef_ajouter_libelle_site() %>%
  mef_ajouter_ope_date() %>% 
  inner_join(y = operation_donnees_environnementales %>% 
               rename(ope_id = ode_ope_id)) %>%
  inner_join(y = facies%>% 
               rename(ope_id = fac_ode_ope_id)) %>%
  inner_join(y = habitat%>% 
               rename(ode_hab_id = hab_id))


facies2 <- facies %>%
  left_join(y=ref_type_facies %>%
              select(tyf_id,
                     facies = tyf_libelle) %>% 
              rename(fac_tyf_id = tyf_id)) %>%
  left_join(y=ref_vegetation_dominante %>%
              select(ved_id,
                     vegt_dominante = ved_libelle) %>% 
              rename(fac_ved_id = ved_id)) %>% 
  left_join(y=ref_granulometrie %>%
              select(gra_id,
                     gra_dominante = gra_libelle) %>% 
              rename(fac_gra_id_dominante = gra_id)) %>% 
  left_join(y=ref_granulometrie %>%
              select(gra_id,
                     gra_accessoire = gra_libelle) %>% 
              rename(fac_gra_id_accessoire = gra_id)) %>% 
  left_join(y=ref_type_colmatage %>%
              select(tyc_id,
                     colmatage = tyc_libelle) %>% 
              rename(fac_tyc_id = tyc_id)) %>%
  select(-fac_tyf_id,-fac_gra_id_accessoire,-fac_gra_id_dominante,-fac_tyc_id,-fac_tyf_id,-fac_ved_id)


#On cherche l'occurence des cas où la végétation est renseignée sans l'évaluation de la couverture

verif_vgt <- facies2 %>% 
  select(fac_id, fac_ode_ope_id, vegt_dominante, fac_recouvrement_vegetation) %>%
  mutate(verif = case_when(!is.na(vegt_dominante) & !is.na(fac_recouvrement_vegetation) ~ "OK",
                           is.na(vegt_dominante) & is.na(fac_recouvrement_vegetation) ~ "OK",
                           vegt_dominante == "Pas de végétation" & is.na(fac_recouvrement_vegetation) ~ "OK",
                           TRUE ~NA))


ggplot(verif_vgt, aes(x="", y=verif)) +
  geom_col(aes(fill = verif))

# On peut partir sur deux cas de figure : 
# a - intégrer le recouvrement vegetation et élminier les opréations pour lesquelles on a pas l'info quand la végétation dominante est indiquée
# b - tout intégrer sans le recouvrement en présence/absence (idem colmatage)

# Cas de figure 1

facies3a <- facies2 %>% 
  left_join(verif_vgt) %>% #On ajoute le filtre vgt
  group_by(fac_ode_ope_id) %>% # On identifie les opérations pour lesquelles la surface de la vgt n'a pas été renseignée pour au moins un facies
  mutate(select_vgt = case_when(!is.na(verif) ~"OK",
         TRUE ~ NA)) %>%
  ungroup() %>% 
  filter(select_vgt == "OK") %>% 
  mutate(fac_recouvrement_vegetation_rel = (fac_recouvrement_vegetation/100*fac_importance_relative/100)*100) %>%
  mutate(vegt_dominante = case_when(vegt_dominante == "Non observable" ~ "Vegt_dom_non_obs",
                                    TRUE ~ vegt_dominante)) %>%
  mutate(colmatage = case_when(colmatage == "Non observable" ~ "colm_non_obs",
                               TRUE ~ colmatage)) %>% 
  mutate(gra_dominante = case_when(gra_dominante == "Non observable" ~ "gra_dom_non_obs",
                                   TRUE ~ gra_dominante)) %>% 
  mutate(gra_accessoire = case_when(gra_accessoire == "Non observable" ~ "gra_acc_non_obs",
                                    TRUE ~ gra_accessoire)) %>%
  group_by(fac_ode_ope_id) %>%
  mutate(prof_moy = mean(fac_profondeur_moyenne),
         prof_max = max(fac_profondeur_moyenne),
         prof_min = min(fac_profondeur_moyenne),
         prof_cv = sd(fac_profondeur_moyenne)/mean(fac_profondeur_moyenne)) %>%
  pivot_wider(names_from = facies,values_from = fac_importance_relative,values_fill = 0) %>% 
  select(-"NA") %>% 
  pivot_wider(names_from = vegt_dominante,values_from = fac_recouvrement_vegetation_rel,values_fill = 0) %>% 
  select(-"NA") %>%
  pivot_wider(names_from = colmatage, values_from = colmatage, values_fn = ~1,values_fill = 0) %>% 
  select(-"NA") %>%
  select(-fac_profondeur_moyenne,
         -gra_dominante,
         -gra_accessoire)
              
facies4a <- facies3a %>%
  group_by(fac_ode_ope_id) %>%
  summarise(prof_moy = mean(prof_moy),
            prof_min = mean(prof_min),
            prof_max = mean(prof_max),
            prof_cv  = mean(prof_cv))

facies5a <- facies3a %>%
  select(fac_ode_ope_id,Courant:colm_non_obs) %>%
  group_by(fac_ode_ope_id) %>%
  summarise(across(everything(),sum, na.rm = TRUE))

facies6a <- left_join(facies4a,facies5a)              

# Cas de figure 2
facies3b <- facies2 %>% 
  mutate(vegt_dominante = case_when(vegt_dominante == "Non observable" ~ "Vegt_dom_non_obs",
            TRUE ~ vegt_dominante)) %>%
  mutate(colmatage = case_when(colmatage == "Non observable" ~ "colm_non_obs",
                   TRUE ~ colmatage)) %>% 
  mutate(gra_dominante = case_when(gra_dominante == "Non observable" ~ "gra_dom_non_obs",
                   TRUE ~ gra_dominante)) %>% 
  mutate(gra_accessoire = case_when(gra_accessoire == "Non observable" ~ "gra_acc_non_obs",
                   TRUE ~ gra_accessoire)) %>%
  group_by(fac_ode_ope_id) %>%
  mutate(prof_moy = mean(fac_profondeur_moyenne),
         prof_max = max(fac_profondeur_moyenne),
         prof_min = min(fac_profondeur_moyenne),
         prof_cv = sd(fac_profondeur_moyenne)/mean(fac_profondeur_moyenne)) %>%
  pivot_wider(names_from = facies,
              values_from = fac_importance_relative,values_fill = 0) %>% 
  select(-"NA") %>% 
  pivot_wider(names_from = vegt_dominante,values_from = vegt_dominante,values_fn = ~1,values_fill = 0) %>% 
  select(-"NA") %>%
  pivot_wider(names_from = colmatage, values_from = colmatage, values_fn = ~1,values_fill = 0) %>% 
  select(-"NA") %>%
  select(-fac_profondeur_moyenne,
         -fac_recouvrement_vegetation,
         -gra_dominante,
         -gra_accessoire)
  
facies4b <- facies3b %>%
    group_by(fac_ode_ope_id) %>%
    summarise(prof_moy = mean(prof_moy),
           prof_min = mean(prof_min),
           prof_max = mean(prof_max),
           prof_cv  = mean(prof_cv))
  
facies5b <- facies3b %>%
    select(fac_ode_ope_id,Courant:colm_non_obs) %>%
    group_by(fac_ode_ope_id) %>%
    summarise(across(everything(),sum, na.rm = TRUE))

facies6b <- left_join(facies4b,facies5b)
      
           
        

    
    
    

