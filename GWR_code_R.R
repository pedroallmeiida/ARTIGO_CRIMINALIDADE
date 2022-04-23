
## Library
library(spgwr)
library(ggplot2)
library(maptools)
library(readxl)
library(dplyr)
library(magrittr)
library( maptools  )
library(spdep)
library(leaflet)
library(RColorBrewer)
library(rgeos)
library(rgdal)
library(spatialreg)
library(tmap)
library(tmaptools)

## add the directory
setwd("C:/Users/....")


## Datasets
DADOS <- read_excel("dados_artigo_homicidio_nordeste.xlsx")



### Shape variables
chi.poly <- rgdal::readOGR('região_nordeste.shp', layer = "região_nordeste" ,use_iconv=TRUE, encoding = "UTF-8", stringsAsFactors = F)
class( chi.poly )
str( slot(chi.poly,"data") )
names( chi.poly )

chi.poly$chefe_maes_sem_conjuge = DADOS$`mulher sem conjuge com filho`


ind_blau = c()
### create variable Index Blau's
for(i in 1:dim(chi.poly)[1]) 
{
  ind_blau[i] = 1 - sum( 
    (chi.poly$pop_t_bran[i]/chi.poly$pop_2010[i])^2,
    (chi.poly$pop_t_pard[i]/chi.poly$pop_2010[i])^2,
    (chi.poly$pop_t_amar[i]/chi.poly$pop_2010[i])^2,
    (chi.poly$pop_t_pret[i]/chi.poly$pop_2010[i])^2
  )
}

chi.poly$ind_blau <- ind_blau


## Correlation

Y = chi.poly$tax_hom_10 # response variable
hist( Y ) # histogram


### add news variables in dataset

chi.poly$preta <- (chi.poly$pop_t_pret/chi.poly$pop_2010)*100
chi.poly$branca <- (chi.poly$pop_t_bran/chi.poly$pop_2010)*100
chi.poly$parda <- (chi.poly$pop_t_pard/chi.poly$pop_2010)*100
chi.poly$indios <- (chi.poly$pop_t_indi/chi.poly$pop_2010)*100


### Correlation

VAR = data.frame(chi.poly$tax_hom_10,
                 chi.poly$ind_blau, 
                 chi.poly$indios, 
                 chi.poly$preta,
                 chi.poly$taxa_desem,
                 chi.poly$perc_dom_a,
                 chi.poly$chefe_maes_sem_conjuge 
)
colnames(VAR) <- c( "HR", "BI", "PIP", "PB", "UR", 
                    "PRH", "FHWS")

COR = round(cor(VAR), 2)

knitr::kable(COR)
corrplot::corrplot(COR, method = "number", bg = F ) # Display the correlation coefficient


## save figures
tiff( "correlacao.tiff",  width = 20, height = 20, 
      units = "cm",pointsize = 12, "lzw",res = 300 )
GGally::ggcorr(COR, palette = "RdYlGn", name = "rho", label = T )
dev.off()


#### OLS  MODELS----

## M1 ##

model1a = lm(
  formula = tax_hom_10 ~ 
    ind_blau + # indice de blau
    taxa_desem + # taxa desemprego
    perc_dom_a + # percentual domicilios alugados
    chefe_maes_sem_conjuge
  ,data = chi.poly
)

summary(model1a)
AIC(model1a)
car::vif(model1a)
lmtest::bptest(model1a,  studentize = F, data = chi.poly)


## M2 ##

model2a = lm(formula = tax_hom_10 ~ 
               preta + # percental de pretos
               taxa_desem + # taxa desemprego
               perc_dom_a + # percentual domicilios alugados
               chefe_maes_sem_conjuge
             , data = chi.poly)

summary(model2a)
lmtest::bptest(model2a,  studentize = F, data = chi.poly)
car::vif(model2a)


## M3 ##

model3a = lm(formula = tax_hom_10 ~ 
               indios + # percental de indios
               taxa_desem + # taxa desemprego
               perc_dom_a + # percentual domicilios alugados
               chefe_maes_sem_conjuge
             , data = chi.poly)

summary(model3a)
car::vif(model3a)
lmtest::bptest(model3a,  studentize = F, data = chi.poly)
AIC(model3a)


model.branco = lm(
  formula = tax_hom_10 ~ 
    branca+
    taxa_desem + # taxa desemprego
    perc_dom_a + # percentual domicilios alugados
    chefe_maes_sem_conjuge
  ,data = chi.poly
);summary(model.branco)

AIC(model.branco)
car::vif(model.branco)
lmtest::bptest(model.branco ,  studentize = F, data = chi.poly)

## Poligonos 
nc.coords <- cbind( chi.poly$latitude, chi.poly$longitude)
nc.5nn <- knearneigh(nc.coords, k=5, longlat = TRUE)
nc.5nn.nb <- knn2nb(nc.5nn)
plot(nc.5nn.nb, nc.coords)

nc.5nn.mat <- nb2mat(nc.5nn.nb)

y = chi.poly$tax_hom_10 # response variable
moranStatistic <- moran(y, nb2listw(nc.5nn.nb), length(y), Szero(nb2listw(nc.5nn.nb)))
moranStatistic

moranTest <- moran.test(y, nb2listw(nc.5nn.nb))
moranTest


#### GWR ####

reg.eq1 = tax_hom_10 ~ 
  ind_blau + # indice de blau
  taxa_desem + # taxa desemprego
  perc_dom_a + # percentual domicilios alugados
  chefe_maes_sem_conjuge

reg.eq2 = tax_hom_10 ~ 
  preta + # percentual de pessoas negras
  taxa_desem + # taxa desemprego
  perc_dom_a + # percentual domicilios alugados
  chefe_maes_sem_conjuge


reg.eq3 = tax_hom_10 ~ 
  indios + # percentual de indios
  taxa_desem + # taxa desemprego
  perc_dom_a + # percentual domicilios alugados
  chefe_maes_sem_conjuge

reg.eq4 = tax_hom_10 ~ 
  branca + # percentual de indios
  taxa_desem + # taxa desemprego
  perc_dom_a + # percentual domicilios alugados
  chefe_maes_sem_conjuge



#### MODELO 1 -- index Blau's-----

GWRbandwidth1 <- gwr.sel(reg.eq1,  data=chi.poly, coords=nc.coords,adapt=T) 
gwr.model1 = gwr(reg.eq1, data=chi.poly, coords=nc.coords, adapt=GWRbandwidth1, hatmatrix=TRUE, se.fit=TRUE) 

## Results 
results1 <- as.data.frame(gwr.model1$SDF)
head(results1)

chi.poly$coef_intercepto_M1 <- results1$X.Intercept.
chi.poly$coef_ind_blau_M1 <- results1$ind_blau
chi.poly$coef_taxa_desem_M1 <- results1$taxa_desem
chi.poly$coef_perc_dom_a_M1 <- results1$perc_dom_a
chi.poly$coef_chefe_maes_sem_conjuge_M1 <- results1$chefe_maes_sem_conjuge


gwr.map <- cbind(chi.poly, as.matrix(results1))
gwr.map1 <- st_as_sf(gwr.map)



### FIGURES 
my.palette2 <- brewer.pal(n = 100, name = "Blues")
my.palette <- wesanderson::wes_palette("Rushmore", 100, type = "continuous")

R2_mod1 <- spplot(gwr.map, "localR2" , col.regions = heat.colors(100, rev = T), at=seq(min(gwr.map$localR2), max(gwr.map$localR2), len=100)  ) 
map_mod1 <- spplot(gwr.map, "coef_ind_blau_M1" , col.regions= my.palette, at=seq(min(gwr.map$coef_ind_blau_M1), max(gwr.map$coef_ind_blau_M1), len=100)  ) 



##### Significance ----


dfree<-gwr.model1$results$edf
chi.poly$ind_blau_M1.t <- gwr.model1$SDF$ind_blau/gwr.model1$SDF$ind_blau_se
chi.poly$taxa_desem_M1.t <- gwr.model1$SDF$taxa_desem/gwr.model1$SDF$taxa_desem_se
chi.poly$perc_dom_a_M1.t <- gwr.model1$SDF$perc_dom_a/gwr.model1$SDF$perc_dom_a_se
chi.poly$chefe_maes_sem_conjuge_M1.t <- gwr.model1$SDF$chefe_maes_sem_conjuge/gwr.model1$SDF$chefe_maes_sem_conjuge_se
chi.poly$intercepto_M1.t <- gwr.model1$SDF$X.Intercept./gwr.model1$SDF$X.Intercept._se

chi.poly$ind_blau_M1.t.p<-2*pt(-abs(chi.poly$ind_blau_M1.t), dfree)
chi.poly$taxa_desem_M1.t.p<-2*pt(-abs(chi.poly$taxa_desem_M1.t), dfree)
chi.poly$perc_dom_a_M1.t.p<-2*pt(-abs(chi.poly$perc_dom_a_M1.t), dfree)
chi.poly$chefe_maes_sem_conjuge_M1.t.p<-2*pt(-abs(chi.poly$chefe_maes_sem_conjuge_M1.t), dfree)
chi.poly$intercepto_M1.t.p<-2*pt(-abs(chi.poly$intercepto_M1.t), dfree)



paletegreen = colorRampPalette(c("white", "darkgreen"))
at=seq(min(chi.poly$ind_blau_M1.t.p), max(chi.poly$ind_blau_M1.t.p), len = 50) 
       
pvalor_ind_blau_M1 <- spplot(chi.poly, "ind_blau_M1.t.p" , col.regions= paletegreen(50), at=at
                             ,colorkey = list(height = 1, labels = list(at = c(0, 0.05, 0.10, 0.20, 0.40, 0.6, 0.8, 1) ), labels = at)) #seq(0, 1, 0.05)



#### MODEL 2 -- percental of black people -----

GWRbandwidth2 <- gwr.sel(reg.eq2,  data=chi.poly, coords=nc.coords,adapt=T) 
gwr.model2 = gwr(reg.eq2, data=chi.poly, coords=nc.coords, adapt=GWRbandwidth2, hatmatrix=TRUE, se.fit=TRUE) 
names(gwr.model2)


## Results 
results2 <- as.data.frame(gwr.model2$SDF)
head(results2)

chi.poly$coef_intercepto_M2 <- results2$X.Intercept.
chi.poly$coef_preta_M2 <- results2$preta
chi.poly$coef_taxa_desem_M2 <- results2$taxa_desem
chi.poly$coef_perc_dom_a_M2 <- results2$perc_dom_a
chi.poly$coef_chefe_maes_sem_conjuge_M2 <- results2$chefe_maes_sem_conjuge

gwr.map <- cbind(chi.poly, as.matrix(results2))
gwr.map2 <- st_as_sf(gwr.map)

R2_mod2 <- spplot(gwr.map, "localR2" , col.regions = heat.colors(100, rev = T), at=seq(min(gwr.map$localR2), max(gwr.map$localR2), len=100)  ) 
map_mod2 <- spplot(gwr.map, "coef_preta_M2" , col.regions= my.palette, at=seq(min(gwr.map$coef_preta_M2), max(gwr.map$coef_preta_M2), len=100)  ) 


### Significance ----

dfree<-gwr.model2$results$edf
chi.poly$preta_M2.t <- gwr.model2$SDF$preta/gwr.model2$SDF$preta_se
chi.poly$taxa_desem_M2.t <- gwr.model2$SDF$taxa_desem/gwr.model2$SDF$taxa_desem_se
chi.poly$perc_dom_a_M2.t <- gwr.model2$SDF$perc_dom_a/gwr.model2$SDF$perc_dom_a_se
chi.poly$chefe_maes_sem_conjuge_M2.t <- gwr.model2$SDF$chefe_maes_sem_conjuge/gwr.model2$SDF$chefe_maes_sem_conjuge_se
chi.poly$intercepto_M2.t <- gwr.model2$SDF$X.Intercept./gwr.model2$SDF$X.Intercept._se

chi.poly$preta_M2.t.p<-2*pt(-abs(chi.poly$preta_M2.t), dfree)
chi.poly$taxa_desem_M2.t.p<-2*pt(-abs(chi.poly$taxa_desem_M2.t), dfree)
chi.poly$perc_dom_a_M2.t.p<-2*pt(-abs(chi.poly$perc_dom_a_M2.t), dfree)
chi.poly$chefe_maes_sem_conjuge_M2.t.p<-2*pt(-abs(chi.poly$chefe_maes_sem_conjuge_M2.t), dfree)
chi.poly$intercepto_M2.t.p<-2*pt(-abs(chi.poly$intercepto_M2.t), dfree)


pallete.green <- brewer.pal(n = 9, name = "YlGn")
pvalor_pretas_M2 <- spplot(chi.poly, "preta_M2.t.p" , cuts = 8, col.regions= pallete.green )



paletegreen = colorRampPalette(c("white", "darkgreen"))
at=seq(min(chi.poly$preta_M2.t.p), max(chi.poly$preta_M2.t.p), len = 50) 

pvalor_pretas_M2<- spplot(chi.poly, "preta_M2.t.p" , col.regions= paletegreen(50), at=at
                             ,colorkey = list(height = 1, labels = list(at = c(0, 0.05, 0.10, 0.20, 0.40, 0.6, 0.8, 1) ), labels = at)) #seq(0, 1, 0.05)

pvalor_pretas_M2


#### MODEL 3 -- percental of indigenous people -----

GWRbandwidth3 <- gwr.sel(reg.eq3,  data=chi.poly, coords=nc.coords,adapt=T) 
gwr.model3 = gwr(reg.eq3, data=chi.poly, coords=nc.coords, adapt=GWRbandwidth3, hatmatrix=TRUE, se.fit=TRUE) 
names(gwr.model3)


## Results 
results3 <- as.data.frame(gwr.model3$SDF)
head(results3)

chi.poly$coef_intercepto_M3 <- results3$X.Intercept.
chi.poly$coef_indios_M3 <- results3$indios
chi.poly$coef_taxa_desem_M3 <- results3$taxa_desem
chi.poly$coef_perc_dom_a_M3 <- results3$perc_dom_a
chi.poly$coef_chefe_maes_sem_conjuge_M3 <- results3$chefe_maes_sem_conjuge


gwr.map <- cbind(chi.poly, as.matrix(results3))
gwr.map3 <- st_as_sf(gwr.map)


### Figures 


R2_mod3 <- spplot(gwr.map, "localR2" , col.regions = heat.colors(100, rev = T), at=seq(min(gwr.map$localR2), max(gwr.map$localR2), len=100)  ) 
map_mod3 <- spplot(gwr.map, "coef_indios_M3" , col.regions= my.palette, at=seq(min(gwr.map$coef_indios_M3), max(gwr.map$coef_indios_M3), len=100)  ) 




##### Significance ----


dfree<-gwr.model3$results$edf
chi.poly$indios_M3.t <- gwr.model3$SDF$indios/gwr.model3$SDF$indios_se
chi.poly$taxa_desem_M3.t <- gwr.model3$SDF$taxa_desem/gwr.model3$SDF$taxa_desem_se
chi.poly$perc_dom_a_M3.t <- gwr.model3$SDF$perc_dom_a/gwr.model3$SDF$perc_dom_a_se
chi.poly$chefe_maes_sem_conjuge_M3.t <- gwr.model3$SDF$chefe_maes_sem_conjuge/gwr.model3$SDF$chefe_maes_sem_conjuge_se
chi.poly$intercepto_M3.t <- gwr.model3$SDF$X.Intercept./gwr.model3$SDF$X.Intercept._se

chi.poly$indios_M3.t.p<-2*pt(-abs(chi.poly$indios_M3.t), dfree)
chi.poly$taxa_desem_M3.t.p<-2*pt(-abs(chi.poly$taxa_desem_M3.t), dfree)
chi.poly$perc_dom_a_M3.t.p<-2*pt(-abs(chi.poly$perc_dom_a_M3.t), dfree)
chi.poly$chefe_maes_sem_conjuge_M3.t.p<-2*pt(-abs(chi.poly$chefe_maes_sem_conjuge_M3.t), dfree)
chi.poly$intercepto_M3.t.p<-2*pt(-abs(chi.poly$intercepto_M3.t), dfree)


paletegreen = colorRampPalette(c("white", "darkgreen"))
at=seq(min(chi.poly$indios_M3.t.p), max(chi.poly$indios_M3.t.p), len = 50) 

pvalor_indios_M3<- spplot(chi.poly, "indios_M3.t.p" , col.regions= paletegreen(50), at=at
                          ,colorkey = list(height = 1, labels = list(at = c(0, 0.05, 0.10, 0.20, 0.40, 0.6, 0.8, 1) ), labels = at)) #seq(0, 1, 0.05)
pvalor_indios_M3



#### MODEL 4 -- percental of white people -----

GWRbandwidth4 <- gwr.sel(reg.eq4,  data=chi.poly, coords=nc.coords,adapt=T) 
gwr.model4 = gwr(reg.eq4, data=chi.poly, coords=nc.coords, adapt=GWRbandwidth4, hatmatrix=TRUE, se.fit=TRUE) 
names(gwr.model4)


## Results 
results4 <- as.data.frame(gwr.model4$SDF)
head(results4)

chi.poly$coef_intercepto_M4 <- results4$X.Intercept.
chi.poly$coef_branca_M4 <- results4$branca
chi.poly$coef_taxa_desem_M4 <- results4$taxa_desem
chi.poly$coef_perc_dom_a_M4 <- results4$perc_dom_a
chi.poly$coef_chefe_maes_sem_conjuge_M4 <- results4$chefe_maes_sem_conjuge


gwr.map <- cbind(chi.poly, as.matrix(results4))
gwr.map4 <- st_as_sf(gwr.map)


### Figures 


R2_mod4 <- spplot(gwr.map, "localR2" , cuts = 4, col.regions= my.palette )
map_mod4 <- spplot(gwr.map, "coef_branca_M4", cuts = 4, col.regions= my.palette  )


R2_mod4 <- spplot(gwr.map, "localR2" , col.regions = heat.colors(100, rev = T), at=seq(min(gwr.map$localR2), max(gwr.map$localR2), len=100)  ) 
map_mod4 <- spplot(gwr.map, "coef_branca_M4" , col.regions= my.palette, at=seq(min(gwr.map$coef_branca_M4), max(gwr.map$coef_branca_M4), len=100)  ) 


##### Significance ----

dfree<-gwr.model4$results$edf
chi.poly$branca_M4.t <- gwr.model4$SDF$branca/gwr.model4$SDF$branca_se
chi.poly$taxa_desem_M4.t <- gwr.model4$SDF$taxa_desem/gwr.model4$SDF$taxa_desem_se
chi.poly$perc_dom_a_M4.t <- gwr.model4$SDF$perc_dom_a/gwr.model4$SDF$perc_dom_a_se
chi.poly$chefe_maes_sem_conjuge_M4.t <- gwr.model4$SDF$chefe_maes_sem_conjuge/gwr.model4$SDF$chefe_maes_sem_conjuge_se
chi.poly$intercepto_M4.t <- gwr.model4$SDF$X.Intercept./gwr.model4$SDF$X.Intercept._se

chi.poly$branca_M4.t.p<-2*pt(-abs(chi.poly$branca_M4.t), dfree)
chi.poly$taxa_desem_M4.t.p<-2*pt(-abs(chi.poly$taxa_desem_M4.t), dfree)
chi.poly$perc_dom_a_M4.t.p<-2*pt(-abs(chi.poly$perc_dom_a_M4.t), dfree)
chi.poly$chefe_maes_sem_conjuge_M4.t.p<-2*pt(-abs(chi.poly$chefe_maes_sem_conjuge_M4.t), dfree)
chi.poly$intercepto_M4.t.p<-2*pt(-abs(chi.poly$intercepto_M4.t), dfree)


paletegreen = colorRampPalette(c("white", "darkgreen"))
at=seq(min(chi.poly$branca_M4.t.p), max(chi.poly$branca_M4.t.p), len = 50) 

pvalor_branca_M4 <- spplot(chi.poly, "branca_M4.t.p" , col.regions= paletegreen(50), at=at
                          ,colorkey = list(height = 1, labels = list(at = c(0, 0.05, 0.10, 0.20, 0.40, 0.6, 0.8, 1) ), labels = at)) #seq(0, 1, 0.05)
pvalor_branca_M4
