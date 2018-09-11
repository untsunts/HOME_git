# break even point salario
#
library(tidyverse)
library(magrittr)
library(stringr)
library(data.table)
library(xts)
###  parámetros de persona ----
pension <- 40000
###  tasas de interés ----
i <- .0775
V <- 1/(1+i)
###  estructuras de crecimiento salarial ----
# subir 10 es la norma (80%), sin embargo parece un sistema condicional
# subir 30 sucede ya que subiste ciertas veces 10. 
salario_pwc <- 29074
###  distribuciones de dinámica de crecimiento de salario ----
funcion_aumento <- function(prob = 0.8, n_intento = 1){
  unif <- runif(n = 1, min = 0, max = 1)
  if(unif < prob - (n_intento-1)*0.2){
    aumento <- 0.1
  }else{
    aumento <- 0.3
  }
  aumento
}
funcion_aumento2 <-function(prob = 0.95, n_intento = 1){
  unif <- runif(n = 1, min = 0, max = 1)
  ajuste_prob <- n_intento*0.025
  if(n_intento >5){
    ajuste_prob <- .25  + (n_intento-5)*0.05  
  }
  if(unif < prob - ajuste_prob){
    aumento <- 0.1
  }else{
    aumento <- 0.25
  }
  aumento
}
###  MCMC----
sim_num <- 1000
y <- 40-30
mc_distr <- matrix(data = NA_real_, nrow = sim_num, ncol = y)

distr_vp <-function(mcmc = mc_distr,y_aux = y, prob_sim = 0.8, salario = salario_pwc, V_aux = V){
  for(i in 1:sim_num){
    salario_aux <- salario
    n_intento <- 1
    mcmc[i,1] <- salario *12 
    for(j in 2:y_aux){
      # aumento_aux <- funcion_aumento(prob = prob_sim, n_intento = n_intento)
      aumento_aux <- funcion_aumento2(prob = prob_sim, n_intento = n_intento)
      salario_aux <- salario_aux*(1+aumento_aux)
      if(aumento_aux>0.1){
        n_intento <-1
      }else{
        n_intento <- n_intento+1
      }
      mcmc[i,j] <- salario_aux*(V_aux**(j-1)) *12
    }
  }
  mcmc
}
set.seed(11)
dsalario_pwc <- distr_vp()
dsalario_pwc <- dsalario_pwc %>% as.data.frame()
names(dsalario_pwc) <- c("e30", "e31", "e32", "e33", "e34", "e35", "e36", "e37", "e38", "e39")
dsalario_pwc %>% 
  tidyr::gather(key = edad, value = salario, e30:e39) %>% 
  dplyr::mutate(edad_num  = gsub(edad, pattern = "e", replacement = "") %>% as.numeric()) %>% 
  ggplot(aes(x = cut(edad_num, include.lowest = T, breaks = 30:39, right = F), y = salario)) + 
  geom_boxplot() + theme_bw() + xlab("edad")

vp_salario_pwc <- dsalario_pwc %>% rowSums()
vp_salario_pwc %>% fivenum()


salario_s <- 60000
set.seed(50)
dsalario_s <- distr_vp(salario = salario_s, prob_sim = 0.95)
dsalario_s <- dsalario_s %>% as.data.frame()
names(dsalario_s) <- c("e30", "e31", "e32", "e33", "e34", "e35", "e36", "e37", "e38", "e39")
dsalario_s %>% 
  tidyr::gather(key = edad, value = salario, e30:e39) %>% 
  dplyr::mutate(edad_num  = gsub(edad, pattern = "e", replacement = "") %>% as.numeric()) %>% 
  ggplot(aes(x = cut(edad_num, include.lowest = T, breaks = 30:39, right = F), y = salario)) + 
  geom_boxplot() + theme_bw() + xlab("edad")
vp_salario_s <- dsalario_s %>% rowSums()
vp_salario_s %>% fivenum()
vp_salario_pwc %>% fivenum()
1000000/((1.075)**9)
