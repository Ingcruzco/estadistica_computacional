# Ejecutar manualmente UNA SOLA VEZ desde la raiz ride_analytics.
# No colocar en R/: Shiny ejecuta automaticamente los scripts de R/.
library(dplyr)
library(tidyr)
library(broom)
source('R/bookdown_helpers.R')
set.seed(2024)
d <- readRDS('data/processed/ride_analytics.rds') %>% ride_ready() %>%
  filter(!is.na(price),is.finite(price),price>0)
# Bookdown usa otra integracion meteorologica; por lo tanto resultados del clima
# y modelo no son exactamente reproducibles hasta armonizar el ETL.
intervalos <- d %>% group_by(cab_type) %>% summarise(n=n(),media=mean(price),desviacion=sd(price),
 mediana=median(price),iqr=IQR(price),error=desviacion/sqrt(n),
 ic_inf=media-stats::qt(.975,n-1)*error,ic_sup=media+stats::qt(.975,n-1)*error,.groups='drop')
comparacion_global <- ride_comparar(d,'price','cab_type')
equiv <- d %>% mutate(name=as.character(name)) %>% inner_join(ride_equiv,by='name') %>%
  mutate(categoria=factor(categoria,levels=unique(ride_equiv$categoria)))
comparacion_categorias <- equiv %>% group_by(categoria) %>%
  group_modify(~ride_comparar(.x,'price','cab_type')) %>% ungroup() %>%
  mutate(p_ajustado=p.adjust(p_valor,method='holm'))
surge_lyft <- d %>% filter(cab_type=='Lyft')
comparacion_surge <- ride_comparar(surge_lyft,'price','surge',invertir=TRUE)
comparacion_lluvia <- d %>% filter(!is.na(lluvia)) %>% group_by(cab_type) %>%
  group_modify(~ride_comparar(.x,'price','lluvia',invertir=TRUE)) %>% ungroup() %>%
  mutate(p_ajustado=p.adjust(p_valor,method='holm'))
# ANOVA top 10 servicios, como el Bookdown
servicios <- d %>% count(name,sort=TRUE) %>% slice_head(n=10) %>% pull(name)
da <- d %>% filter(name %in% servicios) %>% droplevels()
anova_clasica <- aov(price~name,data=da)
levene_anova <- ride_levene(da$price,da$name)
anova_welch <- oneway.test(price~name,data=da,var.equal=FALSE)
anova_tabla <- tibble::tibble(metodo=c('ANOVA clásica','ANOVA Welch','Levene'),
 estadistico=c(unname(summary(anova_clasica)[[1]][['F value']][1]),unname(anova_welch$statistic),levene_anova$F),
 p=c(summary(anova_clasica)[[1]][['Pr(>F)']][1],anova_welch$p.value,levene_anova$p))
# Pares Tukey si varianzas iguales, Welch-Holm si diferentes
if(isTRUE(levene_anova$p >= .05)) {
 pares <- as.data.frame(TukeyHSD(anova_clasica)$name) %>% tibble::rownames_to_column('comparacion') %>%
   rename(diff=diff,lwr=lwr,upr=upr,p_ajustado=`p adj`)
 metodo_pares <- 'Tukey HSD'
} else {
 combinaciones <- combn(levels(droplevels(da$name)),2,simplify=FALSE)
 m<-length(combinaciones)
 pares<-purrr::map_dfr(combinaciones,function(z){
   x1<-da$price[da$name==z[1]];x2<-da$price[da$name==z[2]]
   res<-t.test(x2,x1,conf.level=1-.05/m)
   tibble::tibble(comparacion=paste(z[2],z[1],sep=' - '),diff=mean(x2)-mean(x1),lwr=res$conf.int[1],upr=res$conf.int[2],p=res$p.value)
 }) %>% mutate(p_ajustado=p.adjust(p,method='holm'))
 metodo_pares <- 'Welch por pares + Holm; IC Bonferroni'
}
# Correlaciones del Bookdown
vars_num <- c('distance','price','surge_multiplier','temp','clouds','pressure','rain','humidity','wind')
matriz_cor <- stats::cor(d %>% select(all_of(vars_num)),use='pairwise.complete.obs')
cor_distancia <- list(pearson=cor.test(d$distance,d$price,method='pearson'),
 spearman=cor.test(d$distance,d$price,method='spearman',exact=FALSE))
# Modelo log-lineal con terminos EXACTOS del Bookdown; se excluye lluvia NA sin imputar.
dm <- d %>% filter(!is.na(lluvia),is.finite(temp),is.finite(wind),is.finite(humidity)) %>%
 mutate(across(c(temp,wind,humidity),~as.numeric(scale(.x)),.names='{.col}_z')) %>% droplevels()
modelo <- stats::lm(log(price)~distance+name+surge_multiplier+hora_bloque+lluvia+temp_z+wind_z+humidity_z,data=dm)
coefs <- broom::tidy(modelo,conf.int=TRUE) %>%
 filter(term!='(Intercept)',!grepl('^name',term)) %>%
 mutate(efecto=(exp(estimate)-1)*100,ic_inf=(exp(conf.low)-1)*100,ic_sup=(exp(conf.high)-1)*100)
metricas_modelo <- broom::glance(modelo)
# Guardar sin incluir 700k observaciones; artefactos ligeros para Shiny
resultados <- list(intervalos=intervalos,comparacion_global=comparacion_global,
 categorias=comparacion_categorias,surge=comparacion_surge,lluvia=comparacion_lluvia,
 anova=anova_tabla,pares=pares,metodo_pares=metodo_pares,
 correlaciones=matriz_cor,cor_distancia=c(pearson=unname(cor_distancia$pearson$estimate),spearman=unname(cor_distancia$spearman$estimate)),
 coefs=coefs,metricas=metricas_modelo,n_modelo=nobs(modelo))
dir.create('data/processed',showWarnings=FALSE,recursive=TRUE)
dir.create('models',showWarnings=FALSE,recursive=TRUE)
saveRDS(resultados,'data/processed/bookdown_resultados.rds')
saveRDS(modelo,'models/modelo_bookdown.rds')
cat('OK: artefactos guardados; n modelo =', nobs(modelo),'\n')
