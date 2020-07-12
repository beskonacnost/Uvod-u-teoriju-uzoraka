library(dplyr)
library(lattice)

IDA = read.csv("salary-information-for-industrial-development-agencies.csv")
LA = read.csv("salary-information-for-local-authorities.csv")
LDC = read.csv("salary-information-for-local-development-corporations.csv")
SA = read.csv("salary-information-for-state-authorities.csv")
N_no_filtered = length(IDA$Fiscal.Year.End.Date) + length(LA$Fiscal.Year.End.Date) + length(LDC$Fiscal.Year.End.Date) + length(SA$Fiscal.Year.End.Date)
N_no_filtered

#View(IDA)
#View(LA)
#View(LDC)
#View(SA)
data = list(IDA,LA,LDC,SA)
h = length(data)
h

##init stratums
data_filtered = list()
Mi = c()
str_dates = list()
landmark_data = list()

# criteria
full_time = list()
part_time = list()

# criteria
operational = list()
administrative_clerical = list()
technical_engineering = list()
professional = list()
managerial = list()
executive = list()

for(i in 1:length(data)){
  data_filtered[[i]] = subset(data[[i]], select = -c(Authority.Name, Last.Name, Middle.Initial,
                                               Has.Employees,First.Name,Title, Exempt.Indicator,
                                               Department, Paid.By.Another.Entity, Paid.by.State.or.Local.Government,
                                               Base.Annualized.Salary, Overtime.Paid, Performance.Bonus, Extra.Pay,
                                               Other.Compensation, Actual.Salary.Paid))
  data_filtered[[i]] = na.exclude(filter(data_filtered[[i]], (data_filtered[[i]])$Total.Compensation > 0))
  #Added new column, ID of instance in dataframe
  data_filtered[[i]]$id = seq.int(nrow(data_filtered[[i]]))
  #Added new column, ID of Sector
  data_filtered[[i]]$Sector = i
  Mi[i]= length(unlist(data_filtered[[i]]$Total.Compensation))
  
  #by years
  str_dates[[i]] = as.character(data_filtered[[i]]$Fiscal.Year.End.Date)
  landmark_data[[i]] = data_filtered[[i]]$Total.Compensation
  
  full_time[[i]] = filter(data_filtered[[i]], data_filtered[[i]]$Pay.Type == "FT")$id
  part_time[[i]] = filter(data_filtered[[i]], data_filtered[[i]]$Pay.Type == "PT")$id
  
  operational[[i]] = filter(data_filtered[[i]], data_filtered[[i]]$Group == "Operational")$id
  administrative_clerical[[i]] = filter(data_filtered[[i]], data_filtered[[i]]$Group == "Administrative and Clerical")$id
  technical_engineering[[i]] = filter(data_filtered[[i]], data_filtered[[i]]$Group == "Technical and Engineering")$id
  professional[[i]] = filter(data_filtered[[i]], data_filtered[[i]]$Group == "Professional")$id
  managerial[[i]] = filter(data_filtered[[i]], data_filtered[[i]]$Group == "Managerial")$id
  executive[[i]] = filter(data_filtered[[i]], data_filtered[[i]]$Group == "Executive")$id
}
ft_len = unlist(lapply(full_time, length))
ft_len
pt_len = unlist(lapply(part_time, length))
pt_len
ft = length(unlist(full_time))
pt = length(unlist(part_time))
ft
pt

oper_len = unlist(lapply(operational, length))
oper_len
admin_cler_len = unlist(lapply(administrative_clerical, length))
techn_eng_len = unlist(lapply(technical_engineering, length))
prof_len = unlist(lapply(professional, length))
manag_len = unlist(lapply(managerial, length))
exe_len = unlist(lapply(executive, length))

oper = length(unlist(operational))
admin_cler = length(unlist(administrative_clerical))
techn_eng = length(unlist(technical_engineering))
prof = length(unlist(professional))
manag = length(unlist(managerial))
exe = length(unlist(executive))

lendmark_data = unlist(landmark_data)
N_pop = length(lendmark_data)
N_pop # filtered
N_no_filtered - N_pop # lost in filter process
Mi
sum(Mi) == N_pop
N_pop == pt + ft

View(data_filtered[[1]])
col_num = length(data_filtered[[1]])
col_num

# Analyses of dataset
slices <- Mi
lbls <- c("Industrial\nDevelopment\nAgencies", "Local Authorities", "Local Development\nCorporations", "State Authorities")
pie(slices, cex=0.8,  radius = 0.8, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Public Sector Sections")

slices <- c(ft, pt)
lbls <- c("Full time", "Part time")
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Public Sector Pay type")

slices <- c(length(full_time[[1]]), length(part_time[[1]]))
lbls <- c("Full time", "Part time")
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Public Sector Pay type/Industrial Development Agencies")

slices <- c(length(full_time[[2]]), length(part_time[[2]]))
lbls <- c("Full time", "Part time")
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Public Sector Pay type/Local Authorities")

slices <- c(length(full_time[[3]]), length(part_time[[3]]))
lbls <- c("Full time", "Part time")
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Public Sector Pay type/Local Development Corporations")

slices <- c(length(full_time[[4]]), length(part_time[[4]]))
lbls <- c("Full time", "Part time")
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Public Sector Pay type/State Authorities")

#counts <- table(ft_len, pt_len)
#barplot(counts, main="Pay type distribution",
#        xlab="Number of employees", col=c("darkblue","red"),
#        legend = rownames(counts), beside=TRUE)

slices <- c(oper, admin_cler, techn_eng, prof, manag, exe)
lbls <- c("Operational", "Administrative\nand\nClerical", "Technical\nand\nEngineering", "Professional", 
          "Managerial", "Executive")
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Public Sector Group")

slices <- c(oper_len[1], admin_cler_len[1], techn_eng_len[1], prof_len[1], manag_len[1], exe_len[1])
lbls <- c("Operational", "Administrative\nand\nClerical", "Technical\nand\nEngineering", "Professional", 
          "Managerial", "Executive")
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Public Sector Group/Industrial Development Agencies")

slices <- c(oper_len[2], admin_cler_len[2], techn_eng_len[2], prof_len[2], manag_len[2], exe_len[2])
lbls <- c("Operational", "Administrative\nand\nClerical", "Technical\nand\nEngineering", "Professional", 
          "Managerial", "Executive")
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Public Sector Group/Local Authorities")

slices <- c(oper_len[3], admin_cler_len[3], techn_eng_len[3], prof_len[3], manag_len[3], exe_len[3])
lbls <- c("Operational", "Administrative\nand\nClerical", "Technical\nand\nEngineering", "Professional", 
          "Managerial", "Executive")
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Public Sector Group/Local Development Corporations")

slices <- c(oper_len[4], admin_cler_len[4], techn_eng_len[4], prof_len[4], manag_len[4], exe_len[4])
lbls <- c("Operational", "Administrative\nand\nClerical", "Technical\nand\nEngineering", "Professional", 
          "Managerial", "Executive")
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Public Sector Group/State Authorities")

# sorted by sector/years 2011-2018
IDA_year = list()
LA_year = list()
LDC_year = list()
SA_year = list()

IDA_year[[1]] = filter(data_filtered[[1]], data_filtered[[1]]$Fiscal.Year.End.Date == "2011-12-31T00:00:00.000")
IDA_year[[2]] = filter(data_filtered[[1]], data_filtered[[1]]$Fiscal.Year.End.Date == "2012-12-31T00:00:00.000")
IDA_year[[3]] = filter(data_filtered[[1]], data_filtered[[1]]$Fiscal.Year.End.Date == "2013-12-31T00:00:00.000")
IDA_year[[4]] = filter(data_filtered[[1]], data_filtered[[1]]$Fiscal.Year.End.Date == "2014-12-31T00:00:00.000")
IDA_year[[5]] = filter(data_filtered[[1]], data_filtered[[1]]$Fiscal.Year.End.Date == "2015-12-31T00:00:00.000")
IDA_year[[6]] = filter(data_filtered[[1]], data_filtered[[1]]$Fiscal.Year.End.Date == "2016-12-31T00:00:00.000")
IDA_year[[7]] = filter(data_filtered[[1]], data_filtered[[1]]$Fiscal.Year.End.Date == "2017-12-31T00:00:00.000")
IDA_year[[8]] = filter(data_filtered[[1]], data_filtered[[1]]$Fiscal.Year.End.Date == "2018-12-31T00:00:00.000")
  
IDA_year_TC = lapply(IDA_year, function(x) {x$Total.Compensation})
IDA_year_TC
IDA_Ni_year = as.numeric(lapply(IDA_year_TC, length))
IDA_Ni_year

LA_year[[1]] = filter(data_filtered[[2]], data_filtered[[2]]$Fiscal.Year.End.Date == "2011-12-31T00:00:00.000")
LA_year[[2]] = filter(data_filtered[[2]], data_filtered[[2]]$Fiscal.Year.End.Date == "2012-12-31T00:00:00.000")
LA_year[[3]] = filter(data_filtered[[2]], data_filtered[[2]]$Fiscal.Year.End.Date == "2013-12-31T00:00:00.000")
LA_year[[4]] = filter(data_filtered[[2]], data_filtered[[2]]$Fiscal.Year.End.Date == "2014-12-31T00:00:00.000")
LA_year[[5]] = filter(data_filtered[[2]], data_filtered[[2]]$Fiscal.Year.End.Date == "2015-12-31T00:00:00.000")
LA_year[[6]] = filter(data_filtered[[2]], data_filtered[[2]]$Fiscal.Year.End.Date == "2016-12-31T00:00:00.000")
LA_year[[7]] = filter(data_filtered[[2]], data_filtered[[2]]$Fiscal.Year.End.Date == "2017-12-31T00:00:00.000")
LA_year[[8]] = filter(data_filtered[[2]], data_filtered[[2]]$Fiscal.Year.End.Date == "2018-12-31T00:00:00.000")

LA_year_TC = lapply(LA_year, function(x) {x$Total.Compensation})
LA_year_TC
LA_Ni_year = as.numeric(lapply(LA_year_TC, length))
LA_Ni_year

LDC_year[[1]] = filter(data_filtered[[3]], data_filtered[[3]]$Fiscal.Year.End.Date == "2011-12-31T00:00:00.000")
LDC_year[[2]] = filter(data_filtered[[3]], data_filtered[[3]]$Fiscal.Year.End.Date == "2012-12-31T00:00:00.000")
LDC_year[[3]] = filter(data_filtered[[3]], data_filtered[[3]]$Fiscal.Year.End.Date == "2013-12-31T00:00:00.000")
LDC_year[[4]] = filter(data_filtered[[3]], data_filtered[[3]]$Fiscal.Year.End.Date == "2014-12-31T00:00:00.000")
LDC_year[[5]] = filter(data_filtered[[3]], data_filtered[[3]]$Fiscal.Year.End.Date == "2015-12-31T00:00:00.000")
LDC_year[[6]] = filter(data_filtered[[3]], data_filtered[[3]]$Fiscal.Year.End.Date == "2016-12-31T00:00:00.000")
LDC_year[[7]] = filter(data_filtered[[3]], data_filtered[[3]]$Fiscal.Year.End.Date == "2017-12-31T00:00:00.000")
LDC_year[[8]] = filter(data_filtered[[3]], data_filtered[[3]]$Fiscal.Year.End.Date == "2018-12-31T00:00:00.000")

LDC_year_TC = lapply(LDC_year, function(x) {x$Total.Compensation})
LDC_year_TC
LDC_Ni_year = as.numeric(lapply(LDC_year_TC, length))
LDC_Ni_year

SA_year[[1]] = filter(data_filtered[[4]], data_filtered[[4]]$Fiscal.Year.End.Date == "2011-12-31T00:00:00.000")
SA_year[[2]] = filter(data_filtered[[4]], data_filtered[[4]]$Fiscal.Year.End.Date == "2012-12-31T00:00:00.000")
SA_year[[3]] = filter(data_filtered[[4]], data_filtered[[4]]$Fiscal.Year.End.Date == "2013-12-31T00:00:00.000")
SA_year[[4]] = filter(data_filtered[[4]], data_filtered[[4]]$Fiscal.Year.End.Date == "2014-12-31T00:00:00.000")
SA_year[[5]] = filter(data_filtered[[4]], data_filtered[[4]]$Fiscal.Year.End.Date == "2015-12-31T00:00:00.000")
SA_year[[6]] = filter(data_filtered[[4]], data_filtered[[4]]$Fiscal.Year.End.Date == "2016-12-31T00:00:00.000")
SA_year[[7]] = filter(data_filtered[[4]], data_filtered[[4]]$Fiscal.Year.End.Date == "2017-12-31T00:00:00.000")
SA_year[[8]] = filter(data_filtered[[4]], data_filtered[[4]]$Fiscal.Year.End.Date == "2018-12-31T00:00:00.000")

SA_year_TC = lapply(SA_year, function(x) {x$Total.Compensation})
SA_year_TC
SA_Ni_year = as.numeric(lapply(SA_year_TC, length))
SA_Ni_year

##2018_example
salary_IDA_2018 = IDA_year[[8]]$Total.Compensation
salary_IDA_2018
X_salary_IDA_2018 = mean(salary_IDA_2018)
X_salary_IDA_2018

##Real
X_salary_real_by_str = list()
for(i in 1:4){
  X_salary_real_by_str[[i]] = mean(data_filtered[[i]]$Total.Compensation)
}
X_salary_real_by_str

X_salary_real = mean(lendmark_data)
X_salary_real

##Prost slučajan uzorak bez ponavljanja
n_psu = 400
salary_psu_wor = sample(lendmark_data, n_psu, replace=F)
X_salary_psu_wor = mean(salary_psu_wor)
X_salary_psu_wor

sn2_psu_wor = var(salary_psu_wor)
D_X_psu_wor = (N_pop-n_psu)*sn2_psu_wor/(N_pop*n_psu)
D_X_psu_wor
sqrt(D_X_psu_wor)

alpha = 1-0.90
z = qnorm(1-alpha/2)
z
I_X_psu_wor_90 = c(X_salary_psu_wor-z*sqrt(D_X_psu_wor), X_salary_psu_wor+z*sqrt(D_X_psu_wor))
I_X_psu_wor_90

#sa ponavljanjem
salary_psu_wr = sample(lendmark_data, n_psu, replace=T) #bez ponavljanja
X_salary_psu_wr = mean(salary_psu_wr)
X_salary_psu_wr
# precenio pravu vrednost

sn2_psu_wr = var(salary_psu_wr)
D_X_psu_wr = (N_pop-n_psu)*sn2_psu_wr/(N_pop*n_psu)
D_X_psu_wr
sqrt(D_X_psu_wr) #bolja ocena wr or wor

I_X_psu_wr_90 = c(X_salary_psu_wr-z*sqrt(D_X_psu_wr), X_salary_psu_wr+z*sqrt(D_X_psu_wr))
I_X_psu_wr_90

#Nema smisla odredjivati kolicnicku ocenu po godinama, jer nemamo podatke za 
#iste entitete ali za razlicite godine, u pitanju su razliciti entiteti
#Mozemo samo gledati godine kao stratume ili grupe ili napraviti viseetapno uzorkovanje na osnovu njih

#Stratifikovano uzorkovanje
#proporcionalan raspored
Nh = Mi
nh_prop = round(n_psu/N_pop * Nh)
nh_prop
sum(nh_prop)
if(sum(nh_prop)!=n_psu) { # ako nije = n
  while (sum(nh_prop)!=n_psu) { # ponavljamo sledece korake dok ne bude = n
    if(sum(nh_prop)>n_psu) {
      # ako je >n, biramo neki i smanjujemo za 1
      i = sample(1:length(nh_prop),1)
      nh_prop[i] = nh_prop[i]-1
    }
    else {
      # ako je <n, biramo neki i povecavamo za 1
      i = sample(1:length(nh_prop),1)
      nh_prop[i] = nh_prop[i]+1
    }
  }
}
nh_prop
while(TRUE){
  if (any(nh_prop < 2)){ # ako je neki manji
    j = which(nh_prop < 2)# vidi koji su to
    if(length(j) > 1){
      for(k in 1:j){
        i = sample(1:length(nh_prop),1)
        if (i %!in% j){ #uzmi samo one koji vec nisu po 1
          nh_prop[i] = nh_prop[i]-1
          nh_prop[j] = nh_prop[j]+1
        }
      }
    }
    else{
      i = sample(1:length(nh_prop),1)
      if (i != j){ #uzmi samo one koji vec nisu po 1
        nh_prop[i] = nh_prop[i]-1
        nh_prop[j] = nh_prop[j]+1
      }
    }
  }else{
    break
  }
}
nh_prop
sum(nh_prop) == n_psu

uzorak_nh_prop = list()
for(i in 1:h){
  uzorak_nh_prop[[i]] = sample(data_filtered[[i]]$Total.Compensation, nh_prop[i], replace = F)
}
length(unlist(uzorak_nh_prop)) == n_psu#provera

tn_prop_str = c()
sn2_prop_str = c()
si2_str = c()
Di_x_prop_str = c()
for(i in 1:h){
  tn_prop_str[i] = Nh[i]*mean(uzorak_nh_prop[[i]])
  sn2_prop_str[i] = var(uzorak_nh_prop[[i]])
  si2_str[i] = var(data_filtered[[i]]$Total.Compensation)
  Di_x_prop_str[i] = Nh[i]^2 * sn2_prop_str[i] * (1 - nh_prop[i]/Nh[i]) / nh_prop[i]
}
sn2_prop_str
Di_x_prop_str
D_x_prop_str = sum(Di_x_prop_str) / (N_pop^2)
sqrt(D_x_prop_str)
t_prop_str = sum(tn_prop_str)
t_prop_str
X_prop_str = t_prop_str/N_pop
X_prop_str

#Intervalna ocena
alpha = 1-0.90
z = qt(1-alpha/2, sum(n_prop) - h)
z
I_str_prop_90 = c(X_prop_str-z*sqrt(D_x_prop_str), X_prop_str+z*sqrt(D_x_prop_str))
I_str_prop_90

#Nejmanov izbor
nh_nejman = Nh*sqrt(si2_str)*n_psu/sum(Nh*sqrt(si2_str))
nh_nejman = round(nh_nejman)
nh_nejman
sum(nh_nejman)

if(sum(nh_nejman)!=n_psu) {
  while (sum(nh_nejman)!=n_psu) {
    if(sum(nh_nejman)>n_psu) {
      i = sample(1:length(nh_nejman),1)
      nh_nejman[i] = nh_nejman[i]-1
    }
    else {
      i = sample(1:length(nh_nejman),1)
      nh_nejman[i] = nh_nejman[i]+1
    }
  }
}

nh_nejman
while(TRUE){
  if (any(nh_nejman < 2)){ # ako je neki manji
    j = which(nh_nejman < 2)# vidi koji su to
    if(length(j) > 1){
      for(k in 1:j){
        i = sample(1:length(nh_nejman),1)
        if (i %!in% j){ #uzmi samo one koji vec nisu po 1
          nh_nejman[i] = nh_nejman[i]-1
          nh_nejman[j] = nh_nejman[j]+1
        }
      }
    }
    else{
      i = sample(1:length(nh_nejman),1)
      if (i != j){ #uzmi samo one koji vec nisu po 1
        nh_nejman[i] = nh_nejman[i]-1
        nh_nejman[j] = nh_nejman[j]+1
      }
    }
  }else{
    break
  }
}

nh_nejman
sum(nh_nejman) == n_psu

uzorak_nh_nejman = list()
for(i in 1:4){
  uzorak_nh_nejman[[i]] = sample(data_filtered[[i]]$Total.Compensation, nh_nejman[i], replace = F)
}
uzorak_nh_nejman
length(unlist(uzorak_nh_nejman)) #provera

tn_nejman_str = c()
sn2_nejman_str = c()
for(i in 1:4){
  tn_nejman_str[i] = Mi[i]*mean(uzorak_nh_nejman[[i]])
  sn2_nejman_str[i] = var(uzorak_nh_nejman[[i]])
}
length(nh_nejman[1])
sn2_nejman_str
t_nejman_str = sum(tn_nejman_str)
X_nejman_str = t_nejman_str/N_pop
X_nejman_str

D_X_nejman_str = sum(Mi^2*sn2_nejman_str*(1-nh_nejman/Mi)/nh_nejman)/(N_pop^2)
D_X_nejman_str
sqrt(D_X_nejman_str)

#Intervalna ocena
alpha = 1-0.90
z = qt(1-alpha/2, sum(n_nejman) - h)
z
I_str_nejman_90 = c(X_nejman_str-z*sqrt(D_X_nejman_str), X_nejman_str+z*sqrt(D_X_nejman_str))
I_str_nejman_90

#Grupni uzorak
N_pop
M = N_pop

#PSU bez ponavljanja
#set.seed(42)
h
n_group = 2
index_group_wor = sample(h, n_group, replace=F)
ti_group_psu_wor = c()
for(i in 1:h){
  ti_group_psu_wor[i] = sum(data_filtered[[i]]$Total.Compensation)
}
ti_group_psu_wor
t_group_psu_wor = h*mean(ti_group_psu_wor[index_group_wor])
t_group_psu_wor
X_group_psu_wor = t_group_psu_wor / M
X_group_psu_wor

D_t_group_psu_wor = (h^2)*(1-n_group/h)*(sum((ti_group_psu_wor[index_group_wor]-sum(ti_group_psu_wor[index_group_wor])/n_group)^2))/(n_group*(n_group-1))
D_t_group_psu_wor
D_X_group_psu_wor = D_t_group_psu_wor / (M^2)
D_X_group_psu_wor
sqrt(D_X_group_psu_wor)

# Interval poverenja
alpha = 1-0.90
z = qnorm(1-alpha/2)
z
I_grwor_90 = c(X_group_psu_wor-z*sqrt(D_X_group_psu_wor), X_group_psu_wor+z*sqrt(D_X_group_psu_wor))
I_grwor_wor_90

# kolicnicke ocene, nije nepristrasna
b = cor(ti_group_psu_wor[index_group_wor], Mi[index_group_wor])
b #1
R_group_ocena = sum(ti_group_psu_wor[index_group_wor])/sum(Mi[index_group_wor])
R_group_ocena

D_t_R_group_ocena = (h^2)*(1-n_group/h)*sum((ti_group_psu_wor[index_group_wor]-R_group_ocena*Mi[index_group_wor])^2)/(n_group*(n_group-1))
D_t_R_group_ocena
D_R_group_ocena = D_t_R_group_ocena / (M^2)
D_R_group_ocena
sqrt(D_R_group_ocena)

# Interval poverenja
alpha = 1-0.90
z = qnorm(1-alpha/2)
z
I_grkol_90 = c(R_group_ocena-z*sqrt(D_R_group_ocena), R_group_ocena+z*sqrt(D_R_group_ocena))
I_grkol_90

#sa nejednakim verovatnocama izbora, sa ponavljanjem HH; sa i bez HT
n_hh = 2
pi = Mi/N_pop
pi
sum(pi)

index_hh = sample(h, n_hh, replace=T, prob=pi)
index_hh
#original podaci
ti_group_kol = c()
for(i in 1:h){
  ti_group_kol[i] = sum(data_filtered[[i]]$Total.Compensation)
}
ti_group_kol
Mi
t_hh = sum(ti_group_kol[index_hh]/pi[index_hh])/n_hh
t_hh
X_hh = t_hh / M
X_hh

D_t_hh_ocena = sum((ti_group_kol[index_hh]/pi[index_hh] - t_hh)^2) / (n_hh*(n_hh-1))
D_t_hh_ocena

D_X_hh_ocena = D_t_hh_ocena / (M^2)
D_X_hh_ocena
sqrt(D_X_hh_ocena)

# Interval poverenja
alpha = 1-0.90
z = qnorm(1-alpha/2)
z
I_grhh_90 = c(X_hh-z*sqrt(D_X_hh_ocena), X_hh+z*sqrt(D_X_hh_ocena))
I_grhh_90

#HT ocena

pii = 1-(1-pi)^2
pii

t_ht = sum(ti_group_kol[index_hh]/pii[index_hh])
t_ht
X_ht = t_hh / M
X_ht
index_hh #razlicite jedinke, to je ok

D_x_ht_ocena = sum((1-pii[index_hh])*((ti_group_kol[index_hh])^2)/((pii[index_hh])^2))
for(i in index_hh) {
  for(j in index_hh) {
    if(i!=j) {
      pij = pii[i]+pii[j]-1+(1-pi[i]-pi[j])^n_hh
      D_x_ht_ocena = D_x_ht_ocena + (pij-pii[i]*pii[j])*(ti_group_kol[i]*ti_group_kol[j])/(pii[i]*pii[j]*pij)
    }
  }
}
D_x_ht_ocena
N_pop
D_x_ht_ocena = D_x_ht_ocena/(M^2)
D_x_ht_ocena
sqrt(D_x_ht_ocena)

# Interval poverenja
alpha = 1-0.90
z = qnorm(1-alpha/2)
z
I_grht_90 = c(X_ht-z*sqrt(D_x_ht_ocena), X_ht+z*sqrt(D_x_ht_ocena))
I_grht_90

# Sen-Yates-Grundy ocena

uzorci = list()
#obelezje_na_uzorku = list()
i = 1
for(j in 1:h) {
  for(k in 1:h) {
    if(j<k) {
      uzorci[[i]] = c(j,k)
      i = i+1
    }
  }
}
uzorci
length(uzorci)
choose(4,2)
#obelezje_na_uzorku

Mi
vca_uzorka = function(i,j) {
  (Mi[i]/M)*(Mi[j]/(M-Mi[i])) + (Mi[j]/M)*(Mi[i]/(M-Mi[j]))
}

p_uzorka = c()
for(i in 1:length(uzorci)) {
  p_uzorka[i] = vca_uzorka(uzorci[[i]][1], uzorci[[i]][2])
}
p_uzorka
sum(p_uzorka) #1

#za uzorak bez ponavljanja ne vazi u opstem slucaju formula
pii2 = rep(0,h)
pii2
for(i in 1:h) {
  for(j in 1:length(uzorci)) {
    if(i %in% uzorci[[j]]) {
      pii2[i] = pii2[i]+p_uzorka[j]
    }
  }
}
h
pii2
sum(pii2) == n_hh

index_syg = sample(h, n_hh, replace=F, prob=pi)
index_syg
t_syg = sum(ti_group_kol[index_syg]/pii2[index_syg])
t_syg
X_syg = t_syg / M
X_syg

D_t_ht_ocena_syg = 0
for (i in index_syg) {
  for(j in index_syg) {
    if(i<j) {
      pij = pii2[i]+pii2[j]-1+(1-pi[i]-pi[j])^n_hh
      D_t_ht_ocena_syg = D_t_ht_ocena_syg + (pii2[i]*pii2[j]-pij)*((ti_group_kol[i]/pii2[i]-ti_group_kol[j]/pii2[j])^2)/pij
    }
  }
}

D_t_ht_ocena_syg
D_x_ht_ocena_syg = D_t_ht_ocena_syg / ((M^2)*2)
D_x_ht_ocena_syg
sqrt(D_x_ht_ocena_syg)

# Interval poverenja
alpha = 1-0.90
z = qnorm(1-alpha/2)
z
I_grht_90 = c(X_syg-z*sqrt(D_x_ht_ocena_syg), X_syg+z*sqrt(D_x_ht_ocena_syg))
I_grht_90


##viseetapni
M = N_pop

#PSU bez ponavljanja
#set.seed(42)
#uzimamo odozgo primarne jedinice, iz grupnog uzorka

sekundarne = list()
Mi_mgroup=c()
for(i in 1:n_group){
  # biram proporcionalno obimu uzorka... 
  sekundarne[[i]] = sample(data_filtered[[index_group_wor[i]]]$Total.Compensation, nh_prop[index_group_wor[i]], replace=F)
  Mi_mgroup[i] = Mi[index_group_wor[i]]
}
ti_mgroup_psu_wor = c() #uzorkovane
n_group
for(i in 1:n_group){
  ti_mgroup_psu_wor[i] = Mi_mgroup[i]*mean(sekundarne[[i]])
}
ti_mgroup_psu_wor
nh_prop[index_group_wor]
h
t_mgroup_psu_wor = h*mean(ti_mgroup_psu_wor)
t_mgroup_psu_wor

X_mgroup_psu_wor = t_mgroup_psu_wor / M
X_mgroup_psu_wor

s_t2 = sum((ti_mgroup_psu_wor-t_mgroup_psu_wor/h)^2)/(n_group-1)
s_i2 = c()
for(i in 1:n_group) {
  s_i2[i] = var(sekundarne[[i]])
}

D_t_mgroup_psu_wor = h^2*(1-n_group/h)*s_t2/(n_group) + h*sum(Mi_mgroup^2*(1-nh_prop[index_group_wor]/Mi_mgroup)*s_i2/nh_prop[index_group_wor])/n_group
D_X_mgroup_psu_wor = D_t_group_psu_wor / (M^2)
D_X_mgroup_psu_wor
sqrt(D_X_mgroup_psu_wor)

#Intervalna ocena
alpha = 1-0.90
z = qt(1-alpha/2, sum(n_prop) - h)
z
I_multiwor_90 = c(X_mgroup_psu_wor-z*sqrt(D_X_mgroup_psu_wor), X_mgroup_psu_wor+z*sqrt(D_X_mgroup_psu_wor))
I_multiwor_90

#kolicinska ocena
b_m = cor(ti_mgroup_psu_wor, Mi_mgroup)
b_m #1
R_mgroup_ocena = sum(ti_mgroup_psu_wor)/sum(Mi_mgroup)
R_mgroup_ocena

s_t2_R = sum((ti_mgroup_psu_wor-R_mgroup_ocena*Mi_mgroup)^2)/(n_group-1)
D_t_R_mgroup_psu_wor = h^2*(1-n_group/h)*s_t2_R/n_group + h*sum(Mi_mgroup^2*(1-nh_prop[index_group_wor]/Mi_mgroup)*s_i2/nh_prop[index_group_wor])/n_group
D_X_R_mgroup_psu_wor = D_t_R_mgroup_psu_wor / (M^2)
D_X_R_mgroup_psu_wor
sqrt(D_X_R_mgroup_psu_wor)

#Intervalna ocena
alpha = 1-0.90
z = qt(1-alpha/2, sum(n_prop) - h)
z
I_multikol_90 = c(R_mgroup_ocena-z*sqrt(D_X_R_mgroup_psu_wor), R_mgroup_ocena+z*sqrt(D_X_R_mgroup_psu_wor))
I_multikol_90

############################################## REZULTATI #######################################################
#REAL
X_salary_real
X_salary_real_by_str

##PSU
#X_salary_psu_wor
D_X_psu_wor
I_X_psu_wor_90
#X_salary_psu_wr
D_X_psu_wr

##stratifikovani
#X_prop_str
D_x_prop_str
#X_nejman_str
D_X_nejman_str

##grupni
X_group_psu_wor
D_X_group_psu_wor
sqrt(D_X_group_psu_wor)

R_group_ocena
D_R_group_ocena

#nej verov
X_hh
D_X_hh_ocena
sqrt(D_X_hh_ocena)

X_ht
D_x_ht_ocena
sqrt(D_x_ht_ocena)

X_syg
D_x_ht_ocena_syg

##viseetapni, koriscen prop raspored
X_mgroup_psu_wor
D_X_mgroup_psu_wor

#kolicnicka ocena
R_mgroup_ocena
D_X_R_mgroup_psu_wor

########################### PLOTOVI ##########################################

############## GRAFIK 1 #####################
x1 =  c("SRSWOR", "SRSWR", 
        "StrProp", "StrNeyman", 
        "GrSRSWOR", 
        "GrKolic", "GrHH", "GrHT", "GrSYG",
        "MultiSRSWOR", "MultiKolic")

disperzije1 = c(D_X_psu_wor, D_X_psu_wr, D_x_prop_str, D_X_nejman_str, 
                D_X_group_psu_wor, D_R_group_ocena, D_X_hh_ocena, D_x_ht_ocena, 
                D_x_ht_ocena_syg,D_X_mgroup_psu_wor, D_X_R_mgroup_psu_wor)
y1 = disperzije1

dotplot(sqrt(y1)~x1, main="Poređenje vrednosti standardnih grešaka ocena",
        xlab="Metoda uzorkovanja i tehnika ocenjivanja", ylab="Vrednost standardne greške ocene")

############## GRAFIK 2 #####################
x2 = c("SRSWOR", "SRSWR", 
      "StrProp", "StrNeyman", 
      "GrKolic", "GrHH", "GrSYG",
      "MultiKolic")

disperzije2 = c(D_X_psu_wor, D_X_psu_wr, D_x_prop_str, D_X_nejman_str, 
               D_R_group_ocena, D_X_hh_ocena, D_x_ht_ocena_syg, D_X_R_mgroup_psu_wor)
disperzije2

y2 = disperzije2
y2
dotplot(sqrt(y2)~x2, main="Poređenje vrednosti standardnih grešaka ocena",
        xlab="Metoda uzorkovanja i tehnika ocenjivanja", ylab="Vrednost standardne greške ocene")

############## GRAFIK 3 #####################
x3 = c("GrKolic", "GrSYG")

disperzije3 = c(D_R_group_ocena, D_x_ht_ocena_syg)
y3 = disperzije3
dotplot(sqrt(y3)~x3,  main="Poređenje vrednosti standardnih grešaka ocena",
        xlab="Metoda uzorkovanja i tehnika ocenjivanja", ylab="Vrednost standardne greške ocene")

