setwd("C:/Users/diego/Dropbox/IRNAD/OBServ/EBD/encuestas")
library(dplyr)

encuestas <- read.csv(file = 'encuestas.csv') #script armado partiendo de la tabla con todos los estudios.
#al final del script: manejo de tablas para GitHub.

### Biodiversity Friendly Practices ###
dataBFP <- encuestas %>% select(n_crops_tot,wild_veg_edge_incrop,wild_area_property,
                                fertil_org1_ino0,
                                pestic_chem_appl_year, pestic_che0_org1)
#n_crops (starts in 1) to binary:
crops_01z <- (dataBFP$n_crops_tot-1)#resto 1: paso "1 cultivo" a categor?a 0. Y "+ de 1 cultivo" a categor?a >0.
dataBFP$crops_01[crops_01z>0] <-1
dataBFP$crops_01[crops_01z==0] <-0  #hay q aclararle, si no pone NA. 

#pestic_chem_appl_year to binary: yes or no pesticide
#invierto codificación para tener si=0 y no=1.
dataBFP$pestic_01[dataBFP$pestic_chem_appl_year>0] <-100
dataBFP$pestic_01[dataBFP$pestic_chem_appl_year==0] <-1
dataBFP$pestic_01[dataBFP$pestic_01==100] <-0


## Build BFP variable. With all binary 0-1 
colnames(dataBFP)
#all BFP (all in apple Arg):
dataBFP <- dataBFP %>% mutate(BFP0= crops_01 + pestic_01+pestic_che0_org1+
                               fertil_org1_ino0+
                                wild_veg_edge_incrop+wild_area_property)
#all in coffe:
  dataBFP <- dataBFP %>% mutate(BFP1= crops_01 + pestic_01+
                               fertil_org1_ino0+
                              wild_veg_edge_incrop+wild_area_property)
#all in beans Ecu:
dataBFP <- dataBFP %>% mutate(BFP2= crops_01+pestic_che0_org1+fertil_org1_ino0)

# *...no pestic = pestic org: así como está, no aplicar pesticidas suma 1, igual que aplicar pesticida orgánico.
# ...una alternativa: probar asignarle 0.5 a pesticida orgánico, en vez de 1.
.
dataBFP$study_code <- encuestas$study_code  #incluyo indicador de estudio.


### Select predictors ###  *(conservation actions quedo afuera de ambas:"BFPs" y "predictors")
colnames(encuestas)
predictors <- encuestas %>% select(study_code, owner, education, interact_w_prof, gender_f1_m0,	
                               machin_s0_h1,	assoc_inst, other_income,hive_use, commerce,
                               access_credit,	irrigation,	family_tot_n,	family_female_n,	
                               family_male_n, bee_importance, age, internet_access,	certific,
                               employee_tot_n,	employee_male_n, employee_female_n, 
                               crops_area, property_ha, activs_field0_admin1_both2)
# sin: family_male_n, employee_male_n,
predictorsB <- encuestas %>% select(study_code, owner, education, interact_w_prof, gender_f1_m0,	
                                   machin_s0_h1,	assoc_inst, other_income,hive_use, commerce,
                                   access_credit,	irrigation,	family_tot_n,	family_female_n,	
                                    bee_importance, age, internet_access,	certific,
                                   employee_tot_n,	employee_female_n, 
                                   crops_area, property_ha, activs_field0_admin1_both2)


### Regression trees ### 
#subset per study

#Alto Valle _AV
dataBFP_AV <- filter(dataBFP, study_code == "AV") %>% select(-study_code)
predictors_AV <- filter(predictorsB, study_code == "AV") %>% select(-study_code)
#arrange data for RT for Alto Valle
BFP0 <- dataBFP_AV$BFP0   #response var
#junto predics con rta, y elimino NAs en respuesta (para party-ctree):
RT_AV <- cbind(BFP0, predictors_AV) %>% filter(!is.na(BFP0)) 

#Coffe _CBH
dataBFP_CBH <- filter(dataBFP, study_code == "CBH")%>% select(-study_code)
predictors_CBH <- filter(predictorsB, study_code == "CBH")%>% select(-study_code)
#arrange data for RT for coffe_bahia
BFP1 <- dataBFP_CBH$BFP1   #response var
#junto predics con rta, y elimino NAs en respuesta (para party-ctree):
RT_CBH <- cbind(BFP1, predictors_CBH)%>% filter(!is.na(BFP1)) 

#habas_cotopaxi
dataBFP_HCO <- filter(dataBFP, study_code == "HCO")%>% select(-study_code)
predictors_HCO <- filter(predictorsB, study_code == "HCO")%>% select(-study_code)
#arrange data for RT for coffe_bahia
BFP2 <- dataBFP_HCO$BFP2   #response var
#junto predics con rta, y elimino NAs en respuesta (para party-ctree):
RT_HCO <- cbind(BFP2, predictors_HCO)%>% filter(!is.na(BFP2)) 

### PRUEBAS DE Regression Trees PRELIMINARES ###
## rPART
library(rpart)

# grow tree
#Alto Valle _AV
fit0 <- rpart(BFP0~owner+ education+ interact_w_prof+ gender_f1_m0+	
               machin_s0_h1+	assoc_inst+ other_income+hive_use+ commerce+
               access_credit+	irrigation+	family_tot_n+	family_female_n+	
                bee_importance+ age+ internet_access+	certific+   #family_male_n+
               employee_tot_n+ employee_female_n+       #employee_male_n+
               crops_area+ property_ha+ activs_field0_admin1_both2, 
             method="anova", data=RT_AV)
#Coffe _CBH
fit1 <- rpart(BFP1~owner+ education+ interact_w_prof+ gender_f1_m0+	
                machin_s0_h1+	assoc_inst+ other_income+hive_use+ commerce+
                access_credit+	irrigation+	family_tot_n+	family_female_n+	
                bee_importance+ age+ internet_access+	certific+   #family_male_n+
                employee_tot_n+ employee_female_n+       #employee_male_n+
                crops_area+ property_ha+ activs_field0_admin1_both2, 
             method="anova", data=RT_CBH)
#habas_cotopaxi
fit2 <- rpart(BFP2~owner+ education+ interact_w_prof+ gender_f1_m0+	
                machin_s0_h1+	assoc_inst+ other_income+hive_use+ commerce+
                access_credit+	irrigation+	family_tot_n+	family_female_n+	
                bee_importance+ age+ internet_access+	certific+   #family_male_n+
                employee_tot_n+ employee_female_n+       #employee_male_n+
                crops_area+ property_ha+ activs_field0_admin1_both2, 
             method="anova", data=RT_HCO)

#Resultados
plot(fit2)
printcp(fit2) # display the results
plotcp(fit2) # visualize cross-validation results
summary(fit2) # detailed summary of splits
# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit2) # visualize cross-validation results  
par(mfrow=c(1,1)) # one plots on one page


### PARTY
install.packages("party")
library(party)
fit1y <- ctree(BFP1~., data=RT_CBH)   #VER lo del character...
plot(fit1y)
summary(fit1y)


### EXPLORATORY plots de datos crudos ###
library(Hmisc)
x11()
hist.data.frame(predictors_AV)
boxplot(predictors_AV)

#BFPs
par(mfrow=c(1,3)) # one plots on one page
hist(dataBFP_AV$BFP0)
hist(dataBFP_CBH$BFP1)
hist(dataBFP_HCO$BFP2)

#Predictores:
str(predictors_AV)
colnames(predictors)

#ALTO VALLE:
#variabilidad insignificante (intra-estudio): 
hist(predictors_AV$owner)
hist(predictors_AV$gender_f1_m0)
hist(predictors_AV$machin_s0_h1)
hist(predictors_AV$irrigation) #cero variabilidad.
hist(predictors_AV$internet_access)#cero variabilidad.

#variabilidad moderada
hist(predictors_AV$interact_w_prof)
hist(predictors_AV$family_female_n, breaks = 10)
hist(predictors_AV$bee_importance)
hist(predictors_AV$employee_tot_n, breaks = 200)
hist(predictors_AV$employee_female_n, breaks = 10)
hist(predictors_AV$crops_area, breaks = 100)
hist(predictors_AV$property_ha, breaks = 100)
hist(predictors_AV$activs_field0_admin1_both2) #baja variabilidad.

#variabilidad BUENA
hist(predictors_AV$education)
hist(predictors_AV$assoc_inst)
hist(predictors_AV$other_income)
hist(predictors_AV$hive_use)
hist(predictors_AV$commerce, breaks = 10)
hist(predictors_AV$access_credit)
hist(predictors_AV$family_tot_n, breaks = 20)
hist(predictors_AV$family_male_n)  #no está en predictoresB. hay q usar el otro: predictores.
hist(predictors_AV$age)
hist(predictors_AV$certific)
hist(predictors_AV$employee_male_n) #no está en predictoresB. hay q usar el otro: predictores.

#CAFE_BAHIA_BRASIL:
#variabilidad muy baja o insignificante (intra-estudio): 
hist(predictors_CBH$owner)
hist(predictors_CBH$gender_f1_m0)
hist(predictors_CBH$internet_access)
hist(predictors_CBH$bee_importance)

#variabilidad moderada
hist(predictors_CBH$crops_area, breaks = 100)
hist(predictors_CBH$property_ha, breaks = 100) #con 1 dato muy extremo.
hist(predictors_CBH$hive_use, breaks = 10) #baja variabilidad. 

#variabilidad BUENA
hist(predictors_CBH$machin_s0_h1)  #diff con estudio AV.
hist(predictors_CBH$irrigation) #diff con estudio AV.
hist(predictors_CBH$internet_access) #diff con estudio AV.
hist(predictors_CBH$interact_w_prof)#diff con estudio AV.
hist(predictors_CBH$family_female_n, breaks = 10)#diff con estudio AV.
hist(predictors_CBH$employee_tot_n, breaks = 200) #diff con estudio AV.
hist(predictors_CBH$activs_field0_admin1_both2) #un poco más que estudio AV.
hist(predictors_CBH$education)
hist(predictors_CBH$assoc_inst)
hist(predictors_CBH$other_income)
hist(predictors_CBH$access_credit)
hist(predictors_CBH$family_tot_n, breaks = 20)
hist(predictors_CBH$age)
#no data CBH: employee_female_n, family_male_n, commerce, certific.


#HABAS_COTOPAXI:
#variabilidad insignificante (intra-estudio):
hist(predictors_HCO$owner)
hist(predictors_HCO$commerce, breaks = 10)
hist(predictors_HCO$interact_w_prof)
hist(predictors_HCO$bee_importance)
hist(predictors_HCO$hive_use)
hist(predictors_HCO$commerce, breaks = 10)
hist(predictors_HCO$certific)

#variabilidad moderada
hist(predictors_HCO$employee_tot_n)
hist(predictors_HCO$crops_area, breaks = 100) #varía, pero el rango es muy pequeño.
hist(predictors_HCO$property_ha, breaks = 100)

#variabilidad BUENA
hist(predictors_HCO$gender_f1_m0)
hist(predictors_HCO$machin_s0_h1)
hist(predictors_HCO$irrigation) #cero variabilidad.
hist(predictors_HCO$internet_access)#cero variabilidad.

hist(predictors_HCO$education) # si se hace binaria: la varibilidad será moderada baja.
hist(predictors_HCO$assoc_inst)
hist(predictors_HCO$other_income)
hist(predictors_HCO$access_credit)
hist(predictors_HCO$family_tot_n, breaks = 20) # si se hace binaria: la varibilidad será baja.
hist(predictors_HCO$age)

#no data HCO: activs_field0_admin1_both2, employee_female_n, family_male_n



#bases para GitHub:
Apple_arg <- filter(encuestas, study_code == "AV")
Coffe_bra <- filter(encuestas, study_code == "CBH")
Beans_ecu <- filter(encuestas, study_code == "HCO")
write.csv(Apple_arg,"C:/Users/diego/Dropbox/IRNAD/OBServ/EBD/encuestas\\Apple_arg.csv", row.names = FALSE)
write.csv(Coffe_bra,"C:/Users/diego/Dropbox/IRNAD/OBServ/EBD/encuestas\\Coffe_bra.csv", row.names = FALSE)
write.csv(Beans_ecu,"C:/Users/diego/Dropbox/IRNAD/OBServ/EBD/encuestas\\Beans_ecu.csv", row.names = FALSE)


# tabla completa. falta SEGUIR AGREGANDO estudios.
setwd("C:/Users/diego/Dropbox/IRNAD/OBServ/EBD/2021 tablas")
library(readODS)
encuestas0 <- read_ods("encuestas_full.ods")   #incluye cargas de datos en proceso
encuestas1 <- encuestas0[1:111,]              #solo estudios cargados completamente.
write.csv(encuestas1,"C:/Users/diego/Dropbox/IRNAD/OBServ/EBD/encuestas\\encuestas.csv", row.names = FALSE)



###Elena
##Analisis exploratorios Elena
library(corrgram)
library(vegan)


str(predictors_AV)
colnames(predictors_AV)
summary(predictors_AV)
View(predictors_AV)

str(dataBFP_AV)
#Correlograma con PCA sobre buenas prácticas biodiversidad, Pearson
corrgram(dataBFP_AV[,1:8] , order="PCA", lower.panel=panel.cor,
         upper.panel=panel.pie, text.panel=panel.txt,
         cex.labels = 1,
         col.regions = colorRampPalette(c("red", "salmon", "white", "mediumseagreen", "darkgreen")),
         main="BioDiv Practices Alto Valle")

summary(dataBFP_AV[,1:8])

library(GGally)

g <- dataBFP_AV[,1:8]  %>% 
  ggpairs(aes(color = as.factor(fertil_org1_ino0)), 
          upper = list(continuous = wrap('cor', size = 3)),
          lower = list(combo = wrap("facethist", bins = 15), 
                       continuous = wrap("smooth_loess", alpha=0.3, size=0.1)),
          diag = list(continuous = wrap("densityDiag", alpha = 0.5)))


x11()
g

png("altovallepairs.png")
print(g)
dev.off()


#Clasificación de las fincas
bfp.dist <- vegdist(dataBFP_AV[,1:8], method = "gower", na.rm = TRUE)
hc.bfp.av <- hclust(bfp.dist, method = "centroid" )
plot(hc.bfp.av )

sub_grp_6_av <- cutree(hc.bfp.av, k = 6)
table(sub_grp_6_av)


hc.bfp.av.j <- hclust(bfp.dist.j, method = "centroid" )
bfp.dist.j <- vegdist(dataBFP_AV[,1:8], method = "jaccard", na.rm = TRUE)
plot(hc.bfp.av.j )

sub_grp_3_av.j <- cutree(hc.bfp.av.j, k = 3)
table(sub_grp_3_av.j)

pairs(dataBFP_AV[,1:8])
View(dataBFP_AV[,1:8])
