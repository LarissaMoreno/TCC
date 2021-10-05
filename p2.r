
###################################

q3 <- array(c(7,55,31,269,
              7,20,18,112,
              7,33,24,114,
              40,45,88,172,
              34,50, 24,55,
              27,55,24,58,
              30,34,17,17
             ),dim = c(2,2,7),
              dimnames = list(Cafe=c(">=5","<5"),
                               Infarto =c("Sim","Nao"),
                                Fumante=c("Nunca","Ex Fumante","1 a 14","15 a 24","25 a 34","35 a 44",
                                          "45 ou +")))
q3
###########################################3
#Descritiva
q3.margin.table <- margin.table(q3, 1:2)
q3.margin.table
fourfold(q3.margin.table, std = "ind.max")   # unstandardized
fourfold(q3.margin.table, margin = 1)        # equating gender
fourfold(q3.margin.table, margin = 2)        # standardize both margins


mosaic(q3.margin.table, shade=T)
#no grafico vemos que a divisão do numero de individuos que tomam menos de 5 xicaras 
#de café é de cerca de 2/3 e cerca de 1/3 daqueles que tomam 5 ou mais xicaras de café
#Tambem podemos notar que há um distanciamento da hipotese nula de ausencia de associação em quase todos os 
#grupos menos no grupo de mulheres que tomam mais que 5 xicaras de cafe e não tiveram infarto




library(vcd)


sieve(q3, shade=TRUE, labeling = labeling_values,legend = T)


##############################



#####################################
#letra a

#Tabelas Parciais
(q3.1 <- q3[,,1])
(q3.2 <- q3[,,2])
(q3.3 <- q3[,,3])
(q3.4 <- q3[,,4])
(q3.5 <- q3[,,5])
(q3.6 <- q3[,,6])
(q3.7 <- q3[,,7])

#Razão chances condicional para cada categoria
library(fmsb)
oddsratio(q3.1, conf.level = 0.95)
oddsratio(q3.2, conf.level = 0.95)
oddsratio(q3.3, conf.level = 0.95)
oddsratio(q3.4, conf.level = 0.95)
oddsratio(q3.5, conf.level = 0.95)
oddsratio(q3.6, conf.level = 0.95)
oddsratio(q3.7, conf.level = 0.95)



##odds ratio tabela marginal
(q3.margin <- apply(q3,c(1,2),sum))
oddsratio(q3.margin, conf.level = 0.95)


##Letra b

#Cochran-Mantel-Haenszel 
mantelhaen.test(q3,correct = F,alternative = "two.sided")

#Breslow Day
library(DescTools)
BreslowDayTest(q3)


#letra c
w=NULL
for (i in 1:7) {
  w[i]=(1/q3[,,i][1]+1/q3[,,i][2]+1/q3[,,i][3]+1/q3[,,i][4])^(-1)
}
theta=c(1.104,2.177,1.007,1.737,1.558,1.186,0.882)
theta.l=exp(sum(w*log(theta))/sum(w))
theta.l


c(exp(log(theta.l)-1.96*(sum(w))^(-1/2)),exp(log(theta.l)+1.96*(sum(w))^(-1/2)))


getAnywhere(oddsratio())
##########################
#Questao 4

cancer_idade <- matrix(c(1422, 4432, 2893,1092,406,
                         320, 1206, 1011, 463, 220),2,5,byrow=T)

dimnames(cancer_idade ) <- list(Tratamento=c("Controle","Caso"),
                                idade=c("< 20","20-24", "25-29", "30-34", ">34"))
cancer_idade

#Letra a


#Letra b
library(vcd); library(fmsb)

t(cancer_idade)

cancer_idadem <- as.matrix(cancer_idade)
cancer_idadem
chisq.test(cancer_idadem)
assocstats(cancer_idadem)

library(vcdExtra)

CMHT <- CMHtest(cancer_idade, cscores = c(0,1,2,3,4))
CMHT
R <- sqrt(CMHT$table[1]/(sum(cancer_idade)-1))
R
M <- sqrt(CMHT$table[1])
M
2*(1 - pnorm(M))

CMHT <- CMHtest(cancer_idade, cscores = "midrank")
CMHT
M <- sqrt(CMHT$table[1])
M
2*(1 - pnorm(M))


#M2 = 6.5699 com p-valor 0.0104 sugerindo forte evidência de correlação.
#- A correlação amostral é R = 0.0142 que parece fraca. Em tabelas com
#grandes diferenças nos totais marginais, não é possível obter valores elevados
#para R e portanto a medida não é adequada neste caso.
#- M = 2.5632 com p-valor 0.0052 indicando correlação (teste unilateral).


