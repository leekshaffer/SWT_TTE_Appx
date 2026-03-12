library(tidyverse)
library(swdpwr)
library(lme4)

## Load Data:
load("Vax_data.Rds")

## Identifying Matched-Comparison States for Intervention States:

### Compare Week 18 (Last All-Control Week) Survey Covariates:

Sort_list_int <- Vax_data %>% filter(Period==18) %>% dplyr::filter(lottery==1) %>%
  dplyr::select(Cluster,W18_Excl_Perc,W18_First_18Pop_Pct,W18_Persu_Perc)
Sort_list_comp <- Vax_data %>% filter(Period==18) %>% dplyr::filter(lottery==0) %>%
  dplyr::select(Cluster,W18_Excl_Perc,W18_First_18Pop_Pct,W18_Persu_Perc)

Get_comp <- function(vals, Comp_list) {
  SLC <- Comp_list %>%
    dplyr::mutate(Int_Excl=vals["W18_Excl_Perc"],
                  Int_First=vals["W18_First_18Pop_Pct"],
                  Int_Persu=vals["W18_Persu_Perc"],
                  Dist=(W18_Excl_Perc-Int_Excl)^2+
                    (W18_First_18Pop_Pct-Int_First)^2+
                    (W18_Persu_Perc-Int_Persu)^2) %>%
    dplyr::arrange(Dist)
  return(unlist(SLC[1,c("Cluster","Dist")]))
}

Comp_res <- as_tibble(t(apply(as.matrix(Sort_list_int %>% dplyr::select(starts_with("W18_"))),
                              1, function(x) Get_comp(x, Sort_list_comp)))) %>%
  dplyr::rename(Comp_Cluster=Cluster, Dist=Dist.W18_Excl_Perc)

### Return Intervention States with their Comparator and Calculated Distance:
Int_Comps <- Sort_list_int %>%
  dplyr::select(Cluster) %>% dplyr::rename(Int_Cluster=Cluster) %>%
  bind_cols(Comp_res)
Int_Comps

Int_States <- Int_Comps$Int_Cluster
Comp_States <- Int_Comps$Comp_Cluster

## Getting Parameters from Control Observations:

### Overall Outcome Trends in Control:

MeanResps <- predict(lm(Diff_First~Period*Pd20,
                        data=Vax_data %>% filter(!Interv) %>% 
                          mutate(Pd20=Period > 20)),
                     newdata=tibble(Period=c(15,30),
                                    Pd20=Period > 20))

### Model 1 (all clusters):

lme1 <- summary(lmer(Diff_First~as.factor(Period)+(1|Cluster), 
                     data=Vax_data %>% filter(!Interv)))
Mar_v_1 <- attr(lme1$varcor, "sc")^2+unname(attr(lme1$varcor$Cluster, "stddev"))^2
bpICC_1 <- unname(attr(lme1$varcor$Cluster, "stddev"))^2/Mar_v_1

### Model 2 (matched comparison clusters):

lme2 <- summary(lmer(Diff_First~as.factor(Period)+(1|Cluster), 
                     data=Vax_data %>% 
                       dplyr::filter(Cluster %in% c(Int_States, Comp_States),
                                     !Interv)))
Mar_v_2 <- attr(lme2$varcor, "sc")^2+unname(attr(lme2$varcor$Cluster, "stddev"))^2
bpICC_2 <- unname(attr(lme2$varcor$Cluster, "stddev"))^2/Mar_v_2

## Creating Design Sets:

### Option 1:

dataset_1=matrix(Vax_data %>% pull(Interv),
               nrow=12,
               ncol=16,
               byrow=TRUE)

### Option 2:

dataset_2=matrix(Vax_data %>% 
                   dplyr::filter(Cluster %in% c(Int_States,Comp_States)) %>% 
                   pull(Interv),
                 nrow=8,
                 ncol=16,
                 byrow=TRUE)

### Option 3:

dataset_3=matrix(Vax_data %>% 
                   dplyr::filter(Cluster %in% c(Int_States)) %>% 
                   pull(Interv),
                 nrow=4,
                 ncol=16,
                 byrow=TRUE)

## Estimating Power:

Effect <- 1/3

### Option 1:

swdpower(K=1, design=dataset_1,
         family="gaussian", model="conditional", link="identity",
         type="cross-sectional", 
         meanresponse_start=MeanResps[1], meanresponse_end0=MeanResps[2], meanresponse_end1=MeanResps[2]+Effect,
         sigma2=Mar_v_1, typeIerror=0.05,
         alpha0=0, alpha1=bpICC_1)

### Option 2:

swdpower(K=1, design=dataset_2,
         family="gaussian", model="conditional", link="identity",
         type="cross-sectional", 
         meanresponse_start=MeanResps[1], meanresponse_end0=MeanResps[2], meanresponse_end1=MeanResps[2]+Effect,
         sigma2=Mar_v_2, typeIerror=0.05,
         alpha0=0, alpha1=bpICC_2)

### Option 3:

swdpower(K=1, design=dataset_3,
         family="gaussian", model="conditional", link="identity",
         type="cross-sectional", 
         meanresponse_start=MeanResps[1], meanresponse_end0=MeanResps[2], meanresponse_end1=MeanResps[2]+Effect,
         sigma2=Mar_v_2, typeIerror=0.05,
         alpha0=0, alpha1=bpICC_2)

