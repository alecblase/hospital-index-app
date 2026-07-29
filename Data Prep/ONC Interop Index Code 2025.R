########################
### DATASET CREATION ###
########################

# Load necessary packages
require(dplyr)

# Load 2025 AHA IT Data
# Can be loaded from G drive folder below
# aha_refined_2025 <- read.csv("G:\Office of Planning Evaluation & Analysis\Secured Data\Hospital Adoption (AHA)\Refined Datasets\aha_refined_2025")

setwd("C:/Users/alec.blase/OneDrive - HHS Office of the Secretary/Desktop/hospital-index-app/Data Prep")
aha_refined_2025 <- read.csv(file = "aha_refined_2025.csv")
df <- aha_refined_2025 %>% filter(inscopeit==1, year==2025)

# After filtering, down to 2,351 hospitals

#########################
### VARIABLE CREATION ###
#########################

### CORE INDEX ###

# Core Index (1 of 3): Clinical Interop Functions
# Four functions: Find, Send, Receive, Integrate

# Find: Finding data electronically "Often" or "Sometimes"
df <- df %>% 
  mutate(Find_Interface = case_when(emointf == 1 ~ 1,
                                    emointf == 2 ~ 0.5,
                                    TRUE ~ 0),
         Find_HIE = case_when(emoreg == 1 ~ 1,
                              emoreg == 2 ~ 0.5,
                              TRUE ~ 0),
         Find_Vendor = case_when(emovbn == 1 ~ 1,
                                 emovbn == 2 ~ 0.5,
                                 TRUE ~ 0),
         Find_National = case_when(emonatnet == 1 ~ 1,
                                   emonatnet == 2 ~ 0.5,
                                   TRUE ~ 0))

df <- df %>% 
  mutate(Find = case_when(Find_Interface == 1 | Find_HIE == 1 | Find_Vendor == 1 | Find_National == 1 ~ 1,
                          Find_Interface == 0.5 | Find_HIE == 0.5 |Find_Vendor == 0.5 | Find_National == 0.5 ~ 0.5,
                          TRUE ~ 0))

# Send: Sending data electronically "Often" or "Sometimes"
df <- df %>% 
  mutate(Send_Interface = case_when(intconsnd == 1 ~ 1,
                                    intconsnd == 2 ~ 0.5,
                                    TRUE ~ 0),
         Send_HISP = case_when(hispsnd == 1 ~ 1,
                               hispsnd == 2 ~ 0.5,
                               TRUE ~ 0),
         Send_HIE = case_when(hiosnd == 1 ~ 1,
                              hiosnd == 2 ~ 0.5,
                              TRUE ~ 0),
         Send_Vendor = case_when(sehrsnd == 1 ~ 1,
                                 sehrsnd == 2 ~ 0.5,
                                 TRUE ~ 0),
         Send_National = case_when(mehrsnd == 1 ~ 1,
                                   mehrsnd == 2 ~ 0.5,
                                   TRUE ~ 0))

df <- df %>% 
  mutate(Send = case_when(Send_Interface == 1 | Send_HISP == 1 | Send_HIE == 1 | Send_Vendor == 1 | Send_National == 1 ~ 1,
                          Send_Interface == 0.5 | Send_HISP == 0.5 | Send_HIE == 0.5 | Send_Vendor == 0.5 | Send_National == 0.5 ~ 0.5,
                          TRUE ~ 0))

# Receive: Receiving data electronically "Often" or "Sometimes"
df <- df %>% 
  mutate(Receive_Interface = case_when(intconrcv == 1 ~ 1,
                                       intconrcv == 2 ~ 0.5,
                                       TRUE ~ 0),
         Receive_HISP = case_when(hisprcv == 1 ~ 1,
                                  hisprcv == 2 ~ 0.5,
                                  TRUE ~ 0),
         Receive_HIE = case_when(hiorcv == 1 ~ 1,
                                 hiorcv == 2 ~ 0.5,
                                 TRUE ~ 0),
         Receive_Vendor = case_when(sehrrcv == 1 ~ 1,
                                    sehrrcv == 2 ~ 0.5,
                                    TRUE ~ 0),
         Receive_National = case_when(mehrrcv == 1 ~ 1,
                                      mehrrcv == 2 ~ 0.5,
                                      TRUE ~ 0))

df <- df %>% 
  mutate(Receive = case_when(Receive_Interface == 1 | Receive_HISP == 1 | Receive_HIE == 1 | Receive_Vendor == 1 | Receive_National == 1 ~ 1,
                             Receive_Interface == 0.5 | Receive_HISP == 0.5 | Receive_HIE == 0.5 | Receive_Vendor == 0.5 | Receive_National == 0.5 ~ 0.5,
                             TRUE ~ 0))

# Integrate: Integrating data without manual intervention
# "Yes, routinely" and "Yes, but not routinely" both count - 1 points for routinely, 0.5 for not routinely
df <- df %>% 
  mutate(Integrate = case_when(socint == 1 ~ 1,
                               socint == 2 ~ 0.5,
                               TRUE ~ 0))

# Overall Clinical Interop Functions
df <- df %>% 
  mutate(ClinInteropFx = (Find + Send + Receive + Integrate) / 4)

# Core Index (2 of 3): Data Availability and Use
df <- df %>% 
  mutate(Available = case_when(ciaout == 1 ~ 1,
                               ciaout == 2 ~ 0.5, # 0.5 points for Yes, but not routinely to reflect survey question change
                               TRUE ~ 0),
         Used = case_when(phiout == 1 ~ 1,
                          phiout == 2 ~ 0.5,
                          TRUE ~ 0))

df <- df %>% 
  mutate(DataUse = (Available + Used) / 2)

# Core Index (3 of 3): Breadth of Exchange Partners
df <- df %>% 
  mutate(HospitalSend = case_when(troesa == 1 ~ 1,
                                  troesa == 2 ~ 0.5,
                                  TRUE ~ 0),
         HospitalReceive = case_when(troerqf == 1 ~ 1,
                                     troerqf == 2 ~ 0.5,
                                     TRUE ~ 0),
         AmbSend = case_when(traesa == 1 ~ 1,
                             traesa == 2 ~ 0.5,
                             TRUE ~ 0),
         AmbReceive = case_when(traerqf == 1 ~ 1,
                                traerqf == 2 ~ 0.5,
                                TRUE ~ 0),
         LTPACSend = case_when(trlesa == 1 ~ 1,
                               trlesa == 2 ~ 0.5,
                               TRUE ~ 0),
         LTPACReceive = case_when(trlerqf == 1 ~ 1,
                                  trlerqf == 2 ~ 0.5,
                                  TRUE ~ 0),
         BHSend = case_when(trbhesa == 1 ~ 1,
                            trbhesa == 2 ~ 0.5,
                            TRUE ~ 0),
         BHReceive = case_when(trbherqf == 1 ~ 1,
                               trbherqf == 2 ~ 0.5,
                               TRUE ~ 0))

df <- df %>% 
  mutate(breadth_of_exchange = (HospitalSend + HospitalReceive + AmbSend + AmbReceive + LTPACSend + LTPACReceive + BHSend + BHReceive) / 8)

### NO CORE+ INDEX FOR 2025 ###

### FRICTION INDEX ###

# Friction Index (1 of 3): Barriers to Exchange
# Omitting question related to privacy concerns when receiving data, in alignment with 2023 code
df <- df %>% 
  mutate(Barrier_Send = case_when(npehr == 1 ~ 1,
                                  npehr == 2 ~ 0.5,
                                  TRUE ~ 0) +
                        case_when(nopa == 1 ~ 1,
                                  nopa == 2 ~ 0.5,
                                  TRUE ~ 0) +
                        case_when(nuecs == 1 ~ 1,
                                  nuecs == 2 ~ 0.5,
                                  TRUE ~ 0),
         Barrier_Receive = case_when(noidp == 1 ~ 1,
                                     noidp == 2 ~ 0.5,
                                     TRUE ~ 0) +
                           case_when(prvnoex == 1 ~ 1,
                                     prvnoex == 2 ~ 0.5,
                                     TRUE ~ 0) +
                           case_when(nfmtrqst == 1 ~ 1,
                                     nfmtrqst == 2 ~ 0.5,
                                     TRUE ~ 0),
         Barrier_Other = case_when(difvend == 1 ~ 1,
                                   difvend == 2 ~ 0.5,
                                   TRUE ~ 0) +
                         case_when(adcost == 1 ~ 1,
                                   adcost == 2 ~ 0.5,
                                   TRUE ~ 0) +
                         case_when(custint == 1 ~ 1,
                                   custint == 2 ~ 0.5,
                                   TRUE ~ 0) +
                         case_when(nomitcyb == 1 ~ 1,
                                   nomitcyb == 2 ~ 0.5,
                                   TRUE ~ 0) +
                         case_when(nosegdat == 1 ~ 1,
                                   nosegdat == 2 ~ 0.5,
                                   TRUE ~ 0))

df <- df %>% 
  mutate(Barrier_Send = Barrier_Send / 3,
         Barrier_Receive = Barrier_Receive / 3,
         Barrier_Other = Barrier_Other / 5)

df <- df %>% 
  mutate(Barriers = (Barrier_Send + Barrier_Receive + Barrier_Other) / 3)

# Friction Index (2 of 3): Methods of Exchange
# 1 point for "Often" and 0 points otherwise, in alignment with 2023 code
df <- df %>% 
  mutate(MFind_Portal = case_when(emoport == 1 ~ 1,
                                  TRUE ~ 0),
         MFind_Interface = case_when(emointf == 1 ~ 1,
                                     TRUE ~ 0),
         MFind_Login = case_when(emodirac == 1 ~ 1,
                                 TRUE ~ 0),
         MFind_HIE = case_when(emoreg == 1 ~ 1,
                               TRUE ~ 0),
         MFind_Vendor = case_when(emovbn == 1 ~ 1,
                                  TRUE ~ 0),
         MFind_National = case_when(emonatnet ==  1 ~ 1,
                                    TRUE ~ 0),
         MSend_Portal = case_when(portsnd == 1 ~ 1,
                                  TRUE ~ 0),
         MSend_Interface = case_when(intconsnd == 1 ~ 1,
                                     TRUE ~ 0),
         MSend_Login = case_when(diraccsnd == 1 ~ 1,
                                 TRUE ~ 0),
         MSend_HISP = case_when(hispsnd == 1 ~ 1,
                                TRUE ~ 0),
         MSend_HIE = case_when(hiosnd == 1 ~ 1,
                               TRUE ~ 0),
         MSend_Vendor = case_when(sehrsnd == 1 ~ 1,
                                  TRUE ~ 0),
         MSend_National = case_when(mehrsnd ==  1 ~ 1,
                                    TRUE ~ 0),
         MReceive_Portal = case_when(portrcv == 1 ~ 1,
                                  TRUE ~ 0),
         MReceive_Interface = case_when(intconrcv == 1 ~ 1,
                                     TRUE ~ 0),
         MReceive_Login = case_when(diraccrcv == 1 ~ 1,
                                 TRUE ~ 0),
         MReceive_HISP = case_when(hisprcv == 1 ~ 1,
                                TRUE ~ 0),
         MReceive_HIE = case_when(hiorcv == 1 ~ 1,
                               TRUE ~ 0),
         MReceive_Vendor = case_when(sehrrcv == 1 ~ 1,
                                  TRUE ~ 0),
         MReceive_National = case_when(mehrrcv ==  1 ~ 1,
                                    TRUE ~ 0))

df <- df %>% 
  mutate(MFind = (MFind_Portal + MFind_Interface + MFind_Login + MFind_HIE + MFind_Vendor + MFind_National) / 6,
         MSend = (MSend_Portal + MSend_Interface + MSend_Login + MSend_HISP + MSend_HIE + MSend_Vendor + MSend_National) / 7,
         MReceive = (MReceive_Portal + MReceive_Interface + MReceive_Login + MReceive_HISP + MReceive_HIE + MReceive_Vendor + MReceive_National) / 7)

df <- df %>% 
  mutate(Methods = (MFind + MSend + MReceive) / 3)

# Friction Index (3 of 3): Information Blocking
# Extent for 3 groups: 1) EHR vendors, 2) HIE orgs (Natl Networks or State, Regional, Local HIEs), 3) provider orgs
# 1 point for "Often" or "Sometimes"
df <- df %>% 
  mutate(IB_Extent_EHR = case_when(shcertdv <= 2 ~ 1,
                                   TRUE ~ 0),
         IB_Extent_HIE = case_when(shntlntw <=2 | shsrlhie <=2 ~ 1,
                                   TRUE ~ 0),
         IB_Extent_Prov = case_when(shhcp <=2 ~ 1,
                                    TRUE ~ 0))

df <- df %>% 
  mutate(InfoBlocking = (IB_Extent_EHR + IB_Extent_HIE + IB_Extent_Prov ) / 3)

######################
### INDEX CREATION ###
######################

# Core Index
# Composite variables are ClinInteropFx, DataUse, and breadth_of_exchange
df <- df %>% 
  mutate(Core = ((ClinInteropFx + DataUse + breadth_of_exchange) / 3) * 100)

# Friction Index
# Composite variables are Barriers, Methods, and InfoBlocking
df <- df %>% 
  mutate(Friction = ((Barriers + Methods + InfoBlocking) / 3) * 100)

# Export relevant columns for index app import
index_items_2025 <- df %>% 
  select(aha_id, year, Find, Send, Receive, Integrate, ClinInteropFx, 
         Available, Used, DataUse, 
         HospitalSend, HospitalReceive, AmbSend, AmbReceive, LTPACSend, LTPACReceive, BHSend, BHReceive, breadth_of_exchange, 
         Barrier_Send, Barrier_Receive, Barrier_Other, Barriers,
         MFind, MSend, MReceive, Methods,
         IB_Extent_EHR, IB_Extent_HIE, IB_Extent_Prov, InfoBlocking,
         Core, Friction,
         largebed, medbed, smallbed, cah, cbsatype, piemr, mhsmemb, weight)

write.csv(index_items_2025, "Interop Index Export 2025 Index Items.csv", row.names = F)

index_export_app_2025 <- df %>% 
  select(aha_id, year, Core, ClinInteropFx, DataUse, breadth_of_exchange, Friction, Barriers, Methods, InfoBlocking, 
         largebed, medbed, smallbed, cah, cbsatype, piemr, mhsmemb, weight)

write.csv(index_export_app_2025, "Interop Index Export App 2025.csv", row.names = F)

### Presentation code - summary statistics
core_vars <- c("ClinInteropFx", "DataUse", "breadth_of_exchange", "Core")
core <- lapply(df[core_vars], summary)

friction_vars <- c("Barriers", "Methods", "InfoBlocking", "Friction")
friction <- lapply(df[friction_vars], summary)
