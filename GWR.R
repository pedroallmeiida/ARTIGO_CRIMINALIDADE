
## Bibliotecas 
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

## Adicionar o diretorio
setwd("/home/pedro/Consultoria/Carlos/Homicidios/ARTIGO2/ARTIGO DESORGANIZAÇÃO SOCIAL/Ultima_versao/")


## Ler Banco de dados
DADOS_ARTIGO <- read_excel("dados artigo_22.10.2020.xlsx")
names(DADOS_ARTIGO)  


### Variaveis do Shape
chi.poly <- rgdal::readOGR('NORDESTE.shp', layer = "NORDESTE")
class( chi.poly )
str( slot(chi.poly,"data") )
names( chi.poly )


### Gerando a variavel Indice de Blau
for(i in 1:dim(DADOS_ARTIGO)[1]) 
  {
  DADOS_ARTIGO$ind_blau[i] = 1 - sum( 
                                (DADOS_ARTIGO$`População total Branca`[i]/DADOS_ARTIGO$`Poplução 2010`[i])^2,
                                (DADOS_ARTIGO$`População total Preta`[i]/DADOS_ARTIGO$`Poplução 2010`[i])^2,
                                (DADOS_ARTIGO$`População total Amarela`[i]/DADOS_ARTIGO$`Poplução 2010`[i])^2,
                                (DADOS_ARTIGO$`População total Parda`[i]/DADOS_ARTIGO$`Poplução 2010`[i])^2
                                 )
  }
     





## Correlacao 

Y = DADOS_ARTIGO$`taxa hom` # variavel resposta
hist(Y) # histograma

homi_0 = ifelse(Y == 0, 1, 0)
prop.table(table(homi_0)) ## proporcao de municipios sem homicidios


### Selecionando as covariaveis para testar correlacao
names(DADOS_ARTIGO) # nome das covariaveis
X = DADOS_ARTIGO[,c(4:25,27,28,32:45)] # selecionando as covariaveis 
X = data.frame(X) 

### Correlacao das covariaveis com a variavel resposta
corr=c()
for(i in 1:dim(X)[2]) corr = c( corr, cor( Y, X[,i] )  )

BD.COR = data.frame(names(X),  corr);  ## Correlacoes
ind <- with(BD.COR, order(corr, decreasing = T) )
Y.COR = BD.COR[ind,];Y.COR ## correlacoes ordenadas

#write.table(Y.COR, "correlacao_com_y.txt"  ) ## Salvar correlacoes


#### MODELOS OLS ----

## modelo stepWise
step( lm(Y~.,data=X), direction = "backward")
attach(DADOS_ARTIGO)

### regressores do modelo de desorganizacao social
names(DADOS_ARTIGO)
eq.reg1 = Y ~ TAXA.DE.DEZEMPREGO +
  Percentual.de.Domicílios.alugados + 
  Densidade.Demográfica + IDHM + #Poplução.2010 + #taxa.de.analfabetismo + 
  População_alfabetizada + #População_não_alfabetizada + 
  COLETA.DE.LIXO.Domicílios + Índice.de.Gini.2010 + #X..de.pessoas.em.domicílios.urbanos.com.coleta.de.lixo.2010 + 
  X..de.pessoas.em.domicílios.com.energia.elétrica.2010 + 
  ind_blau +
  População.masculina.de.15.a.24.anos.de.idade.2010

#+ `Responsáveis por domicílio que vivem com cônjuge` 
attach(DADOS_ARTIGO)
prop_alfabetizados = DADOS_ARTIGO$População_alfabetizada/`Poplução 2010`  

### Modelo de regressão de desorganizacao social
model1 = lm(formula = Y ~ 
              ind_blau + 
              #População.Masculina.Preta + 
              #Domicílios.alugados + 
              Percentual.de.Domicílios.alugados + 
              #Densidade.Demográfica + 
              #IDHM +
              #`taxa de analfabetismo` +
              #População_de_15_anos_ou_mais.ALFABETIZADA+
              prop_alfabetizados + 
              #População_não_alfabetizada + 
              #Índice.de.Gini.2010 + 
              #COLETA.DE.LIXO.Domicílios +
              #X..da.população.que.vive.em.domicílios.com.banheiro.e.água.encanada.2010 +
              #Responsáveis.por.domicílio.que.vivem.com.cônjuge+
              #X..da.população.em.domicílios.com.água.encanada.2010 +
              #X..de.pessoas.em.domicílios.urbanos.com.coleta.de.lixo.2010 + 
              X..de.pessoas.em.domicílios.com.energia.elétrica.2010  
              #População.masculina.de.15.a.24.anos.de.idade.2010
            , data = X)
car::vif(model1)
summary(model1)

## Correlacoes covariaveis modelo Desorganizacao social
COVA = data.frame(  Y, ind_blau, prop_alfabetizados, DADOS_ARTIGO$`Percentual de Domicílios alugados`,DADOS_ARTIGO$`Índice de Gini 2010`, DADOS_ARTIGO$`% de pessoas em domicílios com energia elétrica 2010`  )
cor(COVA)  

## Modelo proposto : Desorganizacao social + variveis de desigualdade e educacao

model2 <- lm(formula = Y ~ 
               TAXA.DE.DEZEMPREGO +
               Percentual.de.Domicílios.alugados + 
               Densidade.Demográfica + 
               IDHM +
               prop_alfabetizados +
               Índice.de.Gini.2010 + 
               X..de.pessoas.em.domicílios.com.energia.elétrica.2010 + 
               #`grau de urbanização` +
               #`Porcentagem de famílias pobres elegíveis para o Bolsa Família que realmente são cobertas pelo Bolsa Família (ou seja, taxa de cobertura)`+
               #`Porcentagem de residentes com 18 anos ou mais que estão empregados` +
               #`Responsáveis por domicílio que vivem com cônjuge` +
               #Renda_média_domic._per_capita +
               #`PROPORÇÃO DE PESSOAS COM BAIXA RENDA` +
               ind_blau 
             , data = X)
summary(model2)
car::vif(model2)



##### ---- DISTRIBUICAO DOS RESIDUOS DO MODELO OLS -----  #####

resids<-residuals(model1)
colours <- c("dark blue", "blue", "red", "dark red") 
nc.coords <- cbind( chi.poly$Latitude, chi.poly$Longitude)

### Mapa dos residuos OLS
map.resids <- SpatialPointsDataFrame(data=data.frame(resids), coords=nc.coords ) 
spplot(map.resids, cuts=quantile(resids), col.regions=colours, cex=1) 


#### MODELO GWR ####

GWRbandwidth <- gwr.sel(reg.eq1, data=chi.poly, coords=nc.coords,adapt=T) 
gwr.model = gwr(reg.eq1, data=chi.poly, coords=nc.coords, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
#print o resultado do modelo
names(gwr.model)
moran.mc( resid(gwr.model), listw1, 999) # MOran resíduos

## Multicolinearidade 
gwr.lcr1 <- GWmodel::gwr.lcr(reg.eq1, data=chi.poly,  bw=100000)
names(gwr.lcr1)


results <- as.data.frame(gwr.model$SDF)
head(results)


chi.poly$coef_IDHM <- results$IDHM
chi.poly$coef_ind_theil <- results$ind_theil
chi.poly$coef_tax_urbani <- results$tax_urbani
chi.poly$coef_REN.P.POBR <- results$REN.P.POBR
chi.poly$coef_ind_gini <- results$ind_gini
chi.poly$coef_intercepto <- results$X.Intercept.
chi.poly$t_IDHM <- results$IDHM_se
chi.poly$t_ind_theil <- results$ind_theil_se
chi.poly$t_tax_urbani <- results$tax_urbani_se
chi.poly$t_REN.P.POBR <- results$REN.P.POBR_se
chi.poly$t_ind_gini <- results$ind_gini_se
chi.poly$t_intercepto <- results$X.Intercept._se



## Poligonos

gwr.map <- cbind(chi.poly, as.matrix(results))
gwr.map2 <- st_as_sf(gwr.map)

tmap::qtm(gwr.map, fill = "localR2" ) +
#  tm_borders(col = 'white', lwd = .5) +
  tm_legend(legend.outside = TRUE, legend.outside.position="right") +
  tm_layout(frame = F )

map_localR2 <- tm_shape(gwr.map) + 
  tm_fill("localR2",
          n = 5,
          palette =  "Blues",
          #style = "quantile",
          title = "R2 local" ) +
  tm_borders(col = 'black', lwd = .2) +
  tm_legend(legend.outside = TRUE, legend.outside.position="right") +
  tm_layout(frame = F,
            #legend.outside=TRUE, 
            #legend.outside.position = "right",
            #legend.title.size = 2,
            #legend.outside.size =1
            #legend.title.size = 2
  )
map_localR2


map1 <- tm_shape(gwr.map2) + 
  tm_fill("coef_IDHM",
          n = 5,
          palette =  "RdBu",
          style = "quantile",
          title = "Coef." ) +
  tm_borders(col = 'black', lwd = .3) +
  tm_legend(legend.outside = TRUE, legend.outside.position="right") +
  tm_layout(frame = F,
            #legend.outside=TRUE, 
            #legend.outside.position = "right",
            #legend.title.size = 2,
            #legend.outside.size =1
            #legend.title.size = 2
  )

map1
  
map2 <- tm_shape(gwr.map2) +
  tm_fill("coef_ind_theil",
          n = 5,
          palette =  "RdBu",
          style = "quantile",
          title = "Coef." ) +
  tm_borders(col = 'black', lwd = .3) +
  tm_legend(legend.outside = TRUE, legend.outside.position="right") +
  tm_layout(frame = F,
            #legend.outside=TRUE, 
            #legend.outside.position = "right",
            #legend.title.size = 2,
            #legend.outside.size =1
            #legend.title.size = 2
  )
map2

map3 <- tm_shape(gwr.map2) +
  tm_fill("coef_tax_urbani",
          n = 5,
          palette = "RdBu",
          style = "quantile",
          title = "Coef." ) +
  tm_borders(col = 'black', lwd = .3) +
  tm_legend(legend.outside = TRUE, legend.outside.position="right") +
  tm_layout(frame = F,
            #legend.outside=TRUE, 
            #legend.outside.position = "right",
            #legend.title.size = 2,
            #legend.outside.size =1
            #legend.title.size = 2
  )
map3

map4 <- tm_shape(gwr.map2) +
  tm_fill("coef_REN.P.POBR",
          n = 5,
          palette = "Blues",
          style = "quantile",
          title = "Coef." ) +
  tm_borders(col = 'black', lwd = .3) +
  tm_legend(legend.outside = TRUE, legend.outside.position="right") +
  tm_layout(frame = F  )
map4




map5 <- tm_shape(gwr.map2) +
  tm_fill("coef_ind_gini",
          n = 5,
          palette = "Blues",
          #auto.palette.mapping = T,
          style = "quantile",
          title = "Coef." ) +
  tm_borders(col = 'black', lwd = .3) +
  tm_legend(legend.outside = TRUE, legend.outside.position="right") +
  tm_layout(frame = F,
            #legend.outside=TRUE, 
            #legend.outside.position = "right",
            #legend.title.size = 2,
            #legend.outside.size =1
            #legend.title.size = 2
            )
map5


map6 <- tm_shape(gwr.map2) +
  tm_fill("coef_intercepto",
          n = 5,
          palette = "Blues",
          #auto.palette.mapping = T,
          style = "quantile",
          title = "Coef." ) +
  tm_borders(col = 'black', lwd = .3) +
  tm_legend(legend.outside = TRUE, legend.outside.position="right") +
  tm_layout(frame = F,
            #legend.outside=TRUE, 
            #legend.outside.position = "right",
            #legend.title.size = 2,
            #legend.outside.size =1
            #legend.title.size = 2
  )
map6


### SALVANDO AS FIGURAS 

tmap_save(map1, filename = "COEF_IDHM.jpeg")
tmap_save(map2, filename = "COEF_IND_THEIL.jpeg")
tmap_save(map3, filename = "COEF_TAXA_URB.jpeg")
tmap_save(map4, filename = "COEF_RENDA_POBRES.jpeg")
tmap_save(map5, filename = "COEF_INDICE_GINI.jpeg")
tmap_save(map6, filename = "COEF_INTERCEPTO.jpeg")
tmap_save(map_localR2, filename = "map_localR2.jpeg")


### SALVAR EM GRIDS

library(gridGraphics)

grid.newpage()

# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(3,2)))

# prints a map object into a defined cell   

print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map3, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map4, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
print(map5, vp=viewport(layout.pos.col = 1, layout.pos.row =3))
print(map6, vp=viewport(layout.pos.col = 2, layout.pos.row =3))
dev.off()


##### Siginificancia ----

t = results$X.Intercept.
sig.map = SpatialPointsDataFrame(chi.poly, data.frame(t))
colours=c("green","red","green")
breaks=c(min(t),-4,4,max(t))
spplot(sig.map, cuts=breaks, col.regions=colours, cex=c(0.3,1,0.3))


t = results$IDHM
sig.map = SpatialPointsDataFrame(chi.poly, data.frame(t))
colours=c("green","red","green")
breaks=c(min(t),-4,4,max(t))
spplot(sig.map, cuts=breaks, col.regions=colours, cex=c(0.3,1,0.3))

t = results$ind_theil
sig.map = SpatialPointsDataFrame(chi.poly, data.frame(t))
colours=c("green","red","green")
breaks=c(min(t),-4,4,max(t))
spplot(sig.map, cuts=breaks, col.regions=colours, cex=c(0.3,1,0.3))


