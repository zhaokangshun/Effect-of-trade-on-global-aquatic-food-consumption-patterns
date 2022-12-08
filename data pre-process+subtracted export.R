#########################################################################################
#############Trade is improving global aquatic food consumption##########################
#########################################################################################
####Import-reexport---step1--Subtracting reexport from import data

##Loading data
library(readxl)
production_1<- read_xlsx("Aquaculture and capture and import production.xlsx",sheet = 1)
export_1 <- read_xlsx("Export data.xlsx",sheet=2)

##Data cleaning export_2
library(stringr)
library(reshape2)
dhandle <- function(export_1){
  export <- export_1[,c(1,7:52)]
  export_001 <- filter(export,Import.items!="NA")
  export_002 <- aggregate(. ~ Import.items+Country, data = export_001, sum)
  
}

export_2_m = dhandle(export_1)
export_2_m =export_2_m%>% distinct() 

##Creating production_2
mt_a <- function (production_1,ci){
  p_country_pi = filter(production_1,Country == ci)
  count <- as.data.frame(table(p_country_pi$Import.items))
  count['country'] = ci
  return(count)
}

countr <-as.vector(unique(production_1$Country))

dd<-data.frame()
for (ci in countr){
  mm<-mt_a(production_1,ci)
  dd<-rbind(dd,mm)
}

filter(dd,Freq>1)

production_2 <- aggregate(. ~ Country+Import.items+Trade.flow+Taxon+Mean.Trophic.Level, data = production_1, sum)

##Matching data

bs<-names(production_2)[1:5]
bs1 <- names(export_2_m)[1:2]
yt = as.vector(names(production_2)[6:50])
country<-as.vector(unique(production_2$Country))

dt_t = data.frame()
for (ci in country){
  #if(ci =="Afghanistan"){next}
  for (yi in yt){
    result<-mt_b(production_2,export_2_m,ci,yi)
    dt_t = rbind(dt_t,result)
  }
}



dt_final <- dt_t %>%
  pivot_wider(names_from = year, 
              values_from = f2)

write.csv(dt_final,"subtracted import.csv")


##Data less than 0 is converted to 0
positive <- function(x){
  x[x<0]<- 0;
  x
}
dt_final_copy <-dt_final
x1<-names(dt_final_copy)[6:50]
for (xi in x1){
  dt_final_copy[xi]<-positive(dt_final_copy[xi])
}
write.csv(dt_final_copy,"subtracted import_0.csv")


##Main function
mt_b <- function(production_2,export_2_m,ci,yi){
  
  pm1<-filter(production_2,Country == ci)
  pm2<-pm1[,c(bs,yi)]
  tm1<-filter(export_2_m,Country == ci)
  tm2<-tm1[,c(bs1,yi)]
  tm3<-tm2[,c(1,3)]
  names(tm3)<-c("Import.items","f1")
  result<-left_join(pm2,tm3,by='Import.items')
  fillna <- function(x){
    x[is.na(x )]<- 0;
    x
  }
  result['f1']<-fillna(result['f1'])
  result['f2']<-result[6]-result['f1']
  result_final<-result[,c(1:5,8)]
  result_final['year']=yi
  return(result_final)
}

########################################################################################################################
####################################################################################################
#####################Combine and integrate aquaculture, capture, subtracted import data---step2
library(plyr)
library(readxl)
library(dplyr)
library(tidyverse)

##Loading data
production_1<- read_xlsx("Aquaculture and capture and subtracted import production.xlsx")

##Step-1
p_country_1 <- as.vector(unique(production_1$Country))


x <- c(
  "Afghanistan",
  "Chad",
  "Nepal",
  "Northern Mariana Islands",
  "South Sudan"
)

tt <- c()
for (i in x) {
  ii <- which(p_country_1 == i)
  tt <- append(tt, ii)
}
tt
B <- p_country_1[-tt]

mt_big <- function(production_1, ci) {
  
  p_country_pi <- filter(production_1, Country == ci)
  count <- as.data.frame(table(p_country_pi$Aqua.Cap.items)) 
  item_freq <- filter(count, Freq > 1)
  itemss <- filter(count, Freq == 1)
  col_cut <- p_country_pi[1:4]
  if (dim(item_freq)[1] > 0) {
    items <- as.vector(item_freq$Var1) 

    ROWS_M <- data.frame()
    for (xi in items) {
      cut_freq <- filter(col_cut, Aqua.Cap.items == xi)
      row_merge <- cut_freq %>%
        group_by(Country, Aqua.Cap.items) %>%
        mutate(
          Detailed.production.source = paste(Detailed.production.source, collapse = ","),
          Taxon = paste(Taxon, collapse = ",")
        )
      ROWS_M <- rbind(row_merge, ROWS_M)
    }
    ROWS_M_d <- ROWS_M[!duplicated(ROWS_M$Aqua.Cap.items), ]
  } else {
    NULL
  }

  return(ROWS_M_d)
}


mt_small <- function(production_1, ci) {
  p_country_pi <- filter(production_1, Country == ci)
  count <- as.data.frame(table(p_country_pi$Aqua.Cap.items)) 
  itemss <- filter(count, Freq == 1)
  col_cut <- p_country_pi[1:4]
  if (dim(itemss)[1] > 0) {
    items <- as.vector(itemss$Var1) 
    ROWS_M_1 <- data.frame()
    for (xi in items) {
      cut_freq <- filter(col_cut, Aqua.Cap.items == xi)
      ROWS_M_1 <- rbind(cut_freq, ROWS_M_1)
    }
    ROWS_M_1 <- ROWS_M_1[!duplicated(ROWS_M_1$Aqua.Cap.items), ]
  } else {
    NULL
  }

  return(ROWS_M_1)
}


#####################
#result <- data.frame()
#for (xi in p_country_1) {
#  x1 <- mt_big(production_1, xi)
 # x2 <- mt_small(production_1, xi)
#  x3 <- rbind(x1, x2)
#  result <- rbind(result, x3)
#}


result1 <- data.frame()
for (xi in B) {
  x1 <- mt_big(production_1, xi)
  #x2 <- mt_small(production_1, xi)
  #x3 <- rbind(x1, x2)
  result1 <- rbind(result1, x1)
}


result2 <- data.frame()
for (xi in p_country_1) {
  #x1 <- mt_big(production_1, xi)
  x2 <- mt_small(production_1, xi)
  #x3 <- rbind(x1, x2)
  result2 <- rbind(result2, x2)
}

result<-rbind(result1,result2)
#################################


## step-2

weight_1 <- function(production_1, ci) {
  p_country_pi <- filter(production_1, Country == ci)
  count <- as.data.frame(table(p_country_pi$Aqua.Cap.items))
  item_freq <- filter(count, Freq > 1)
  d_weight <- p_country_pi[4:6]

  if (dim(item_freq)[1] > 0) {
    items <- as.vector(item_freq$Var1)
    items_weight <- list()
    for (yi in items) { 
      dx_01 <- filter(d_weight, Aqua.Cap.items == yi)
      a <- dx_01[2] * dx_01[3]
      a_n <- sum(a)
      a_m <- sum(dx_01[3])
      a_mn <- a_n / a_m
      items_weight[[yi]] <- a_mn
    }
  }
  # else if (dim(item_freq)[1]==0){NULL}

  else {
    NULL
  }
  item_matrix <- data.frame(matrix(unlist(items_weight), nrow = length(items_weight), byrow = T), stringsAsFactors = FALSE)
  colnames(item_matrix) <- "Mean.Trophic.Level"
  item_matrix["Aqua.Cap.items"] <- items

  return(item_matrix)
}


weight_2 <- function(production_1, ci) {
  p_country_pi <- filter(production_1, Country == ci)
  count <- as.data.frame(table(p_country_pi$Aqua.Cap.items))
  itemss <- filter(count, Freq == 1)
  d_weight <- p_country_pi[4:6]

  if (dim(itemss)[1] > 0) {
    items_weight <- list()
    items <- as.vector(itemss$Var1)
    for (xi in items) { 
      dx_02 <- filter(d_weight, Aqua.Cap.items == xi)
      ax <- as.numeric(dx_02[2])
      items_weight[[xi]] <- ax
    }
  } else {
    NULL
  }
  item_matrix <- data.frame(matrix(unlist(items_weight), nrow = length(items_weight), byrow = T), stringsAsFactors = FALSE)
  colnames(item_matrix) <- "Mean.Trophic.Level"
  item_matrix["Aqua.Cap.items"] <- items
  return(item_matrix)
}


result_1 <- data.frame()
for (xi in B) {
  x1 <- weight_1(production_1, xi)
  x2 <- weight_2(production_1, xi)
  x3 <- rbind(x1, x2)
  x3["Country"] <- xi
  result_1 <- rbind(result_1, x3)
}

result_t <- result_1 %>% distinct()

result_p <- data.frame()
for (xi in x) {
  x2 <- weight_2(production_1, xi)

  x2["Country"] <- xi
  result_p <- rbind(result_p, x2)
}

result_pt <- rbind(result_p, result_t)


### step-3

summ <- function(production_1, ci) {
  p_country_pi <- filter(production_1, Country == ci)
  p_country_subset <- p_country_pi[, 4:50]
  p_subset <- subset(p_country_subset, select = -c(Mean.Trophic.Level)) 
  ## Aggregating results
  p_subset_agg <- aggregate(. ~ Aqua.Cap.items, data = p_subset, sum)
  return(p_subset_agg)
}


sum_result <- data.frame()
for (ci in p_country_1) {
  s1 <- summ(production_1, ci)
  s1["Country"] <- ci
  sum_result <- rbind(sum_result, s1)
}

## step-4

m_01 <- data.frame()
for (ci in p_country_1) {
  c1 <- filter(result, Country == ci)
  c2 <- filter(result_pt, Country == ci)[1:2]
  y1 <- left_join(c1, c2, by = "Aqua.Cap.items")
  c3 <- filter(sum_result, Country == ci)[1:46]
  y2 <- left_join(y1, c3, by = "Aqua.Cap.items")
  m_01 <- rbind(m_01, y2)
}

write.csv(m_01,"Aquaculture and capture and subtracted import production-integrated.csv")


#########################################################################################
#########################################################################################
#########################################################################################
####Aquaculture+capture+subtracted import-export---step3

##Loading data
production_1<- read_xlsx("Aquaculture and capture and subtracted import production-integrated.xlsx",sheet = 1)
export_1 <- read_xlsx("Export data.xlsx",sheet=1)

library(stringr)
library(reshape2)

##Data cleaning export_2

dhandle <- function(export_1){
  export <- export_1[,c(1,7:52)]
  export_001 <- filter(export,Aqua.Cap.items!="NA")
  export_002 <- aggregate(. ~ Aqua.Cap.items+Country, data = export_001, sum)
  
}

export_1_m = dhandle(export_1) 
export_1_m =export_1_m%>% distinct() 

write.csv(export_1_m,"Export data-integrated.csv")

##Production+subtracted import-export
##Main function
mt_b <- function(production_1,export_1_m,ci,yi){
  
  pm1<-filter(production_1,Country == ci)
  pm2<-pm1[,c(bs,yi)]
  tm1<-filter(export_1_m,Country == ci)
  tm2<-tm1[,c(bs1,yi)]
  tm3<-tm2[,c(1,3)]
  names(tm3)<-c("Aqua.Cap.items","f1")
  result<-left_join(pm2,tm3,by='Aqua.Cap.items')
  fillna <- function(x){
    x[is.na(x )]<- 0;
    x
  }
  result['f1']<-fillna(result['f1'])
  result['f2']<-result[6]-result['f1']
  result_final<-result[,c(1:5,8)]
  result_final['year']=yi
  return(result_final)
}
bs<-names(production_1)[1:5]
bs1 <- names(export_1_m)[1:2]
yt = as.vector(names(production_1)[6:50])
country<-as.vector(unique(production_1$Country))

dt_t = data.frame()
for (ci in country){
  #if(ci =="Afghanistan"){next}
  for (yi in yt){
    result<-mt_b(production_1,export_1_m,ci,yi)
    dt_t = rbind(dt_t,result)
  }
}


dt_final <- dt_t %>%
  pivot_wider(names_from = year, 
              values_from = f2)

write.csv(dt_final,"Production+subtracted import-export.csv")


##Data greater than 0 is converted to 0
positive <- function(x){
  x[x>0]<- 0;
  x
}
dt_final_copy <-dt_final
x1<-names(dt_final_copy)[6:50]
for (xi in x1){
  dt_final_copy[xi]<-positive(dt_final_copy[xi])
}
write.csv(dt_final_copy,"Production+subtracted import-export(positive value is 0).csv")

##Data less than 0 is converted to 0
positive <- function(x){
  x[x<0]<- 0;
  x
}
dt_final_copy <-dt_final
x1<-names(dt_final_copy)[6:50]
for (xi in x1){
  dt_final_copy[xi]<-positive(dt_final_copy[xi])
}
write.csv(dt_final_copy,"Production+subtracted import-export(negative value is 0).csv")

#########################################################################################
#########################################################################################
#########################################################################################
####Aquaculture+capture+subtracted import-export-remaining export1--step4

library(plyr)
library(readxl)
library(dplyr)
library(tidyverse)

##Loading data
m_01<- read_xlsx("Production+subtracted import-export(positive value).xlsx")
export_1<- read_xlsx("Remaining export-1.xlsx")


datahandle <- function(export_1) {
  export <- export_1[, c(1,7, 8:52)]
  export_001 <- filter(export, Aqua.Cap.items != "NA")
  export_002 <- aggregate(. ~ Aqua.Cap.items + Country, data = export_001, sum)
}
export_m <- datahandle(export_1)
write.csv(export_m,"Integrated remaining export-1.csv")

##Main function
Vol_a <- function(m_01,export_m,ci,yi){
  #ci = 'Peru'
  #yi = "2019"
  # ci = 'Afghanistan'
  #  yi ='2019'
  t1 = filter(m_01,Country ==ci)
  t2 = t1[,c(base1,yi)]
  t_items = as.vector(t2$Aqua.Cap.items)
  s1 = filter(export_m,Country ==ci)
  s2 = s1[,c(base2,yi)]
  items_total = as.vector(s2$Aqua.Cap.items)
  ##筛选s2中带逗号
  s2_1 = subset(s2,grepl(",",Aqua.Cap.items))
  items_1 = as.vector(s2_1$Aqua.Cap.items)
  
  tt = c()
  for (i in items_1){
    ii = which(items_total ==i)
    tt = append(tt,ii)
  }
  
  items_2 = items_total[-tt]
  
  s2_2 = data.frame()
  for (i in items_2){
    s1 = filter(s2,Aqua.Cap.items==i)
    s2_2 = rbind(s1,s2_2)
    
  }
  
  
  ##12345
  dt1 = data.frame()
  ##
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[1]
    dt1 = rbind(x,dt1)
  }
  names(dt1) <- c('Aqua.Cap.items','merge.1')
  x_join_1 = left_join(t2,dt1,by='Aqua.Cap.items')
  
  ##12345
  dt2 = data.frame()
  ##
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[2]
    dt2 = rbind(x,dt2)
  }
  names(dt2) <- c('Aqua.Cap.items','merge.2')
  x_join_2 = left_join( x_join_1,dt2,by='Aqua.Cap.items')
  
  ##12345
  dt3 = data.frame()
  ##
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[3]
    dt3 = rbind(x,dt3)
  }
  names(dt3) <- c('Aqua.Cap.items','merge.3')
  x_join_3 = left_join( x_join_2,dt3,by='Aqua.Cap.items')
  
  
  ##12345
  dt4 = data.frame()
  ##
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[4]
    dt4 = rbind(x,dt4)
  }
  names(dt4) <- c('Aqua.Cap.items','merge.4')
  x_join_4 = left_join( x_join_3,dt4,by='Aqua.Cap.items')
  
  ##12345
  dt5 = data.frame()
  ##
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[5]
    dt5 = rbind(x,dt5)
  }
  names(dt5) <- c('Aqua.Cap.items','merge.5')
  x_join_5 = left_join( x_join_4,dt5,by='Aqua.Cap.items')
  
  # s2_2['merge.6'] = s2_2['Aqua.Cap.items']
  #s2_2_0 = s2_2[,c(1,4)]
  #x_join_6 = left_join(x_join_5,s2_2_0,by = 'Aqua.Cap.items')
  
  
  
  
  #######
  # x_join_6 = x_join_5[,c(base1,]
  names(dt1) <- c('Aqua.Cap.items','merge')
  names(dt2) <- c('Aqua.Cap.items','merge')
  names(dt3) <- c('Aqua.Cap.items','merge')
  names(dt4) <- c('Aqua.Cap.items','merge')
  names(dt5) <- c('Aqua.Cap.items','merge')
  dt6<-rbind(dt1,dt2,dt3,dt4,dt5)
  dt6 <- dt6 %>% distinct()
  dt7<-na.omit(dt6)
  
  dt8<-left_join(dt7,t2,by = 'Aqua.Cap.items')
  s2_3 <- s2_1[,c(1,3)]
  names(s2_3) = c('merge','value')
  dt9<-left_join(dt8,s2_3,by='merge')
  
  t9<-as.vector(unique(dt9$merge))
  
  dt10<-data.frame()
  for (xi in t9){
    xx = filter(dt9,merge ==xi)
    pop = prop.table(xx[8])
    xx['f1'] = xx$value*pop
    dt10 = rbind(dt10,xx)
    
  }
  
  pos <- function(x) {
    x[is.na(x)] <- 0
    x
  }
  
  dt10['f1'] = pos(dt10['f1'])
  
  ##分配完的结果数据
  dt11<-ddply(dt10,.(Aqua.Cap.items),summarize,f2 = sum(f1))
  
  ##matching and subtract
  
  s2_4 = s2[,c(1,3)]
  names(s2_4) = c("Aqua.Cap.items","f3")
  
  dt12<-left_join(t2,s2_4,by ="Aqua.Cap.items")
  dt13<-left_join(dt12,dt11,by = "Aqua.Cap.items")
  
  dt13['f3'] <- pos(dt13['f3'])
  dt13['f2'] <- pos(dt13['f2']) 
  dt13['f4'] <- dt13[yi]-dt13['f2']-dt13['f3']
  
  dt14<-dt13[,c(base1,'f4')]
  
  
  dt14['year']=yi
  
  return(dt14)
}

Vol_b <- function(m_01,export_m,ci,yi){
  #  ci = 'United States of America'
  # yi = "2019"
  # ci = 'Afghanistan'
  #yi ='2019'
  t1 = filter(m_01,Country ==ci)
  t2 = t1[,c(base1,yi)]
  t_items = as.vector(t2$Aqua.Cap.items)
  s1 = filter(export_m,Country ==ci)
  s2 = s1[,c(base2,yi)]
  items_total = as.vector(s2$Aqua.Cap.items)
  ##
  s2_1 = subset(s2,grepl(",",Aqua.Cap.items))
  items_1 = as.vector(s2_1$Aqua.Cap.items)
  
  tt = c()
  for (i in items_1){
    ii = which(items_total ==i)
    tt = append(tt,ii)
  }
  
  ##12345
  dt1 = data.frame()
  ##
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[1]
    dt1 = rbind(x,dt1)
  }
  names(dt1) <- c('Aqua.Cap.items','merge.1')
  x_join_1 = left_join(t2,dt1,by='Aqua.Cap.items')
  
  ##12345
  dt2 = data.frame()
  ##
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[2]
    dt2 = rbind(x,dt2)
  }
  names(dt2) <- c('Aqua.Cap.items','merge.2')
  x_join_2 = left_join( x_join_1,dt2,by='Aqua.Cap.items')
  
  ##12345
  dt3 = data.frame()
  ##
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[3]
    dt3 = rbind(x,dt3)
  }
  names(dt3) <- c('Aqua.Cap.items','merge.3')
  x_join_3 = left_join( x_join_2,dt3,by='Aqua.Cap.items')
  
  
  ##12345
  dt4 = data.frame()
  ##
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[4]
    dt4 = rbind(x,dt4)
  }
  names(dt4) <- c('Aqua.Cap.items','merge.4')
  x_join_4 = left_join( x_join_3,dt4,by='Aqua.Cap.items')
  
  ##12345
  dt5 = data.frame()
  ##
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[5]
    dt5 = rbind(x,dt5)
  }
  names(dt5) <- c('Aqua.Cap.items','merge.5')
  x_join_5 = left_join( x_join_4,dt5,by='Aqua.Cap.items')
  
  ##data matching-1
  s2_1_t = s2_1[,c(1,3)]
  names(s2_1_t) = c('Aqua.Cap.items','value1')
  y_join_1 = left_join(x_join_5,s2_1_t,by=c('merge.1'='Aqua.Cap.items'))
  #xt_1 = filter(y_join_1,value1 >0 )
  xm1<-as.vector(unique(y_join_1$merge.1))
  
  r1 = data.frame()
  for (i in xm1){
    # i = "Atlantic bluefin tuna, Bigeye tuna, Blackfin tuna, Pacific bluefin tuna, Skipjack tuna, Tuna and bonito, Yellowfin tuna"
    dm = filter(y_join_1,merge.1==i)
    pop = prop.table(dm[7])
    dm['f1'] = dm$value1*pop
    r1 = rbind(r1,dm)
  }
  
  ##data matching-2
  s2_1_t = s2_1[,c(1,3)]
  names(s2_1_t) = c('Aqua.Cap.items','value1')
  y_join_1 = left_join(x_join_5,s2_1_t,by=c('merge.2'='Aqua.Cap.items'))
  #xt_1 = filter(y_join_1,value1 >0 )
  xm1<-as.vector(unique(y_join_1$merge.2))
  
  r2 = data.frame()
  for (i in xm1){
    # i = "Atlantic bluefin tuna, Bigeye tuna, Blackfin tuna, Pacific bluefin tuna, Skipjack tuna, Tuna and bonito, Yellowfin tuna"
    dm = filter(y_join_1,merge.2==i)
    pop = prop.table(dm[7])
    dm['f1'] = dm$value1*pop
    r2 = rbind(r2,dm)
  }
  
  ##data matching-3
  s2_1_t = s2_1[,c(1,3)]
  names(s2_1_t) = c('Aqua.Cap.items','value1')
  y_join_1 = left_join(x_join_5,s2_1_t,by=c('merge.3'='Aqua.Cap.items'))
  #xt_1 = filter(y_join_1,value1 >0 )
  xm1<-as.vector(unique(y_join_1$merge.3))
  
  r3 = data.frame()
  for (i in xm1){
    # i = "Atlantic bluefin tuna, Bigeye tuna, Blackfin tuna, Pacific bluefin tuna, Skipjack tuna, Tuna and bonito, Yellowfin tuna"
    dm = filter(y_join_1,merge.3==i)
    pop = prop.table(dm[7])
    dm['f1'] = dm$value1*pop
    r3 = rbind(r3,dm)
  }
  
  ##data matching-4
  s2_1_t = s2_1[,c(1,3)]
  names(s2_1_t) = c('Aqua.Cap.items','value1')
  y_join_1 = left_join(x_join_5,s2_1_t,by=c('merge.4'='Aqua.Cap.items'))
  #xt_1 = filter(y_join_1,value1 >0 )
  xm1<-as.vector(na.omit(unique(y_join_1$merge.4)))
  
  r4 = data.frame()
  for (i in xm1){
    # i = "Atlantic bluefin tuna, Bigeye tuna, Blackfin tuna, Pacific bluefin tuna, Skipjack tuna, Tuna and bonito, Yellowfin tuna"
    dm = filter(y_join_1,merge.4==i)
    pop = prop.table(dm[7])
    dm['f1'] = dm$value1*pop
    r4 = rbind(r4,dm)
  }
  
  
  ##data matchingdata matching-5
  s2_1_t = s2_1[,c(1,3)]
  names(s2_1_t) = c('Aqua.Cap.items','value1')
  y_join_1 = left_join(x_join_5,s2_1_t,by=c('merge.5'='Aqua.Cap.items'))
  #xt_1 = filter(y_join_1,value1 >0 )
  xm1<-as.vector(na.omit(unique(y_join_1$merge.5)))
  
  r5 = data.frame()
  for (i in xm1){
    # i = "Atlantic bluefin tuna, Bigeye tuna, Blackfin tuna, Pacific bluefin tuna, Skipjack tuna, Tuna and bonito, Yellowfin tuna"
    dm = filter(y_join_1,merge.5==i)
    pop = prop.table(dm[7])
    dm['f1'] = dm$value1*pop
    r5 = rbind(r5,dm)
  }
  
  ##data matching-6
  s2_2_t = s2[,c(1,3)]
  names(s2_2_t) = c('Aqua.Cap.items','value1')
  y_join_1 = left_join(x_join_5,s2_2_t,by='Aqua.Cap.items')
  #xt_1 = filter(y_join_1,value1 >0 )
  xm1<-as.vector(na.omit(unique(y_join_1$Aqua.Cap.items)))
  
  r6 = data.frame()
  for (i in xm1){
    # i = "Atlantic bluefin tuna, Bigeye tuna, Blackfin tuna, Pacific bluefin tuna, Skipjack tuna, Tuna and bonito, Yellowfin tuna"
    dm = filter(y_join_1,Aqua.Cap.items==i)
    #pop = prop.table(dm$`2019`)
    dm['f2'] = dm$value1
    r6 = rbind(r6,dm)
  }
  
  ##combine r1-r5
  #r1_5 = rbind(r1,r2,r3,r4,r5)
  #r1_5 = r1_5[,c("Aqua.Cap.items","f1")]
  #r1_5 = ddply(r1_5,.(Aqua.Cap.items),summarize,f3=sum(f1))
  
  
  ##combine with r6-result
  #r8 <- left_join(r6,r1_5,by = 'Aqua.Cap.items')
  # r8['f4']<-ifelse()
  
  fillna <- function(x){
    x[is.na(x )]<- 0;
    x
  }
  
  r6['f2']<-fillna( r6['f2'])
  #r8['f3']<-fillna( r8['f3'])
  r6['f4']<- r6[7]-r6['f2']
  # r8[11]<- r8['f4']
  #r6['f4'] <-r6['f2']
  
  
  r9<-r6[,c(1:6,15)]
  r9['year']=yi
  
  return(r9)}  

##### Aquaculture+capture+subtracted import-export-remaining export-1
base1 = names(m_01)[1:6]
base2 = names(export_m)[1:2]
yt = as.vector(names(m_01)[7:51])
country = as.vector(unique(m_01$Country))
dataf <- subset(export_m,grepl(",",Aqua.Cap.items))
countries <-as.vector(unique(dataf$Country))

tf = c()
for (i in countries){
  ii = which(country ==i)
  tf = append(tf,ii)
}
countries_f = country[-tf]

##step1
dt = data.frame()
for (ci in countries){
  # if(ci =="Peru"){next}
  for (yi in yt){
    result<-Vol_a(m_01,export_m,ci,yi)
    
    dt = rbind(dt,result)
  }
}

##step2
dt_f = data.frame()
for (ci in countries_f){
  #if(ci =="Peru"){next}
  
  for (yi in yt){
    result<-Vol_b(m_01,export_m,ci,yi)
    dt_f = rbind(dt_f,result)
  }
}
##step3
dt_result<-rbind(dt,dt_f)
##result
#dt <- dt_141[,c(2:13)]
dt_result_final <- dt_result %>%
  pivot_wider(names_from = year, 
              values_from = f4)

write.csv(dt_result_final,"Production+subtracted import-export-remaining export-1.csv")

##Data greater than 0 is converted to 0
positive <- function(x){
  x[x>0]<- 0;
  x
}

dt_result_final_copy <-dt_result_final

x1<-names(dt_result_final_copy)[7:51]

for (xi in x1){
  
  dt_result_final_copy[xi]<-positive(dt_result_final_copy[xi])
}

write.csv(dt_result_final_copy,"Production+subtracted import-export-remaining export-1(positive value is 0).csv")
##Data less than 0 is converted to 0
positive <- function(x){
  x[x<0]<- 0;
  x
}

dt_result_final_copy <-dt_result_final

x1<-names(dt_result_final_copy)[7:51]

for (xi in x1){
  
  dt_result_final_copy[xi]<-positive(dt_result_final_copy[xi])
}

write.csv(dt_result_final_copy,"Production+subtracted import-export-remaining export-1(negative value is 0).csv")


#########################################################################################
#########################################################################################
#########################################################################################
####Aquaculture+capture+subtracted import-export-remaining export-last remaining export-step5

##Loading data
m_01<- read_xlsx("Production+subtracted import-export-remaining export-1(positive value).xlsx")
export_1<- read_xlsx("Remaining export-2.xlsx")

datahandle <- function(export_1) {
  export <- export_1[, c(1,7, 8:52)]
  export_001 <- filter(export, Aqua.Cap.items != "NA")
  export_002 <- aggregate(. ~ Aqua.Cap.items + Country, data = export_001, sum)
}
export_m <- datahandle(export_1)
write.csv(export_m,"Integrated remaining export-2.csv")


##Main function
Vol_a <- function(m_01,export_m,ci,yi){
  #ci = 'Peru'
  #yi = "2019"
  # ci = 'Afghanistan'
  #  yi ='2019'
  t1 = filter(m_01,Country ==ci)
  t2 = t1[,c(base1,yi)]
  t_items = as.vector(t2$Aqua.Cap.items)
  s1 = filter(export_m,Country ==ci)
  s2 = s1[,c(base2,yi)]
  items_total = as.vector(s2$Aqua.Cap.items)
  ##
  s2_1 = subset(s2,grepl(",",Aqua.Cap.items))
  items_1 = as.vector(s2_1$Aqua.Cap.items)
  
  tt = c()
  for (i in items_1){
    ii = which(items_total ==i)
    tt = append(tt,ii)
  }
  
  items_2 = items_total[-tt]
  
  s2_2 = data.frame()
  for (i in items_2){
    s1 = filter(s2,Aqua.Cap.items==i)
    s2_2 = rbind(s1,s2_2)
    
  }
  
  
  ##12345
  dt1 = data.frame()
  ## Split and match the commas first, then match the non-commas
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[1]
    dt1 = rbind(x,dt1)
  }
  names(dt1) <- c('Aqua.Cap.items','merge.1')
  x_join_1 = left_join(t2,dt1,by='Aqua.Cap.items')
  
  ##12345
  dt2 = data.frame()
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[2]
    dt2 = rbind(x,dt2)
  }
  names(dt2) <- c('Aqua.Cap.items','merge.2')
  x_join_2 = left_join( x_join_1,dt2,by='Aqua.Cap.items')
  
  ##12345
  dt3 = data.frame()
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[3]
    dt3 = rbind(x,dt3)
  }
  names(dt3) <- c('Aqua.Cap.items','merge.3')
  x_join_3 = left_join( x_join_2,dt3,by='Aqua.Cap.items')
  
  
  ##12345
  dt4 = data.frame()
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[4]
    dt4 = rbind(x,dt4)
  }
  names(dt4) <- c('Aqua.Cap.items','merge.4')
  x_join_4 = left_join( x_join_3,dt4,by='Aqua.Cap.items')
  
  ##12345
  dt5 = data.frame()
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[5]
    dt5 = rbind(x,dt5)
  }
  names(dt5) <- c('Aqua.Cap.items','merge.5')
  x_join_5 = left_join( x_join_4,dt5,by='Aqua.Cap.items')
  
  # s2_2['merge.6'] = s2_2['Aqua.Cap.items']
  #s2_2_0 = s2_2[,c(1,4)]
  #x_join_6 = left_join(x_join_5,s2_2_0,by = 'Aqua.Cap.items')
  
  
  #######
  # x_join_6 = x_join_5[,c(base1,]
  names(dt1) <- c('Aqua.Cap.items','merge')
  names(dt2) <- c('Aqua.Cap.items','merge')
  names(dt3) <- c('Aqua.Cap.items','merge')
  names(dt4) <- c('Aqua.Cap.items','merge')
  names(dt5) <- c('Aqua.Cap.items','merge')
  dt6<-rbind(dt1,dt2,dt3,dt4,dt5)
  dt6 <- dt6 %>% distinct()
  dt7<-na.omit(dt6)
  
  dt8<-left_join(dt7,t2,by = 'Aqua.Cap.items')
  s2_3 <- s2_1[,c(1,3)]
  names(s2_3) = c('merge','value')
  dt9<-left_join(dt8,s2_3,by='merge')
  
  t9<-as.vector(unique(dt9$merge))
  
  dt10<-data.frame()
  for (xi in t9){
    xx = filter(dt9,merge ==xi)
    pop = prop.table(xx[8])
    xx['f1'] = xx$value*pop
    dt10 = rbind(dt10,xx)
    
  }
  
  pos <- function(x) {
    x[is.na(x)] <- 0
    x
  }
  
  dt10['f1'] = pos(dt10['f1'])
  
  ##The result data is allocated
  dt11<-ddply(dt10,.(Aqua.Cap.items),summarize,f2 = sum(f1))
  
  ##Matching and subtracting
  
  s2_4 = s2[,c(1,3)]
  names(s2_4) = c("Aqua.Cap.items","f3")
  
  dt12<-left_join(t2,s2_4,by ="Aqua.Cap.items")
  dt13<-left_join(dt12,dt11,by = "Aqua.Cap.items")
  
  dt13['f3'] <- pos(dt13['f3'])
  dt13['f2'] <- pos(dt13['f2']) 
  dt13['f4'] <- dt13[yi]-dt13['f2']-dt13['f3']
  
  dt14<-dt13[,c(base1,'f4')]
  
  
  dt14['year']=yi
  
  return(dt14)
}

Vol_b <- function(m_01,export_m,ci,yi){
  #  ci = 'United States of America'
  # yi = "2019"
  # ci = 'Afghanistan'
  #yi ='2019'
  t1 = filter(m_01,Country ==ci)
  t2 = t1[,c(base1,yi)]
  t_items = as.vector(t2$Aqua.Cap.items)
  s1 = filter(export_m,Country ==ci)
  s2 = s1[,c(base2,yi)]
  items_total = as.vector(s2$Aqua.Cap.items)
  ##
  s2_1 = subset(s2,grepl(",",Aqua.Cap.items))
  items_1 = as.vector(s2_1$Aqua.Cap.items)
  
  tt = c()
  for (i in items_1){
    ii = which(items_total ==i)
    tt = append(tt,ii)
  }
  
  ##12345
  dt1 = data.frame()
  ## Split and match the commas first, then match the non-commas
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[1]
    dt1 = rbind(x,dt1)
  }
  names(dt1) <- c('Aqua.Cap.items','merge.1')
  x_join_1 = left_join(t2,dt1,by='Aqua.Cap.items')
  
  ##12345
  dt2 = data.frame()
  
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[2]
    dt2 = rbind(x,dt2)
  }
  names(dt2) <- c('Aqua.Cap.items','merge.2')
  x_join_2 = left_join( x_join_1,dt2,by='Aqua.Cap.items')
  
  ##12345
  dt3 = data.frame()
  
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[3]
    dt3 = rbind(x,dt3)
  }
  names(dt3) <- c('Aqua.Cap.items','merge.3')
  x_join_3 = left_join( x_join_2,dt3,by='Aqua.Cap.items')
  
  
  ##12345
  dt4 = data.frame()
  
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[4]
    dt4 = rbind(x,dt4)
  }
  names(dt4) <- c('Aqua.Cap.items','merge.4')
  x_join_4 = left_join( x_join_3,dt4,by='Aqua.Cap.items')
  
  ##12345
  dt5 = data.frame()
  
  for (i in t_items){
    num = grep(i,items_1,value = T)
    x = data.frame(i)
    x['n'] = num[5]
    dt5 = rbind(x,dt5)
  }
  names(dt5) <- c('Aqua.Cap.items','merge.5')
  x_join_5 = left_join( x_join_4,dt5,by='Aqua.Cap.items')
  
  ##Data matching-1
  s2_1_t = s2_1[,c(1,3)]
  names(s2_1_t) = c('Aqua.Cap.items','value1')
  y_join_1 = left_join(x_join_5,s2_1_t,by=c('merge.1'='Aqua.Cap.items'))
  #xt_1 = filter(y_join_1,value1 >0 )
  xm1<-as.vector(unique(y_join_1$merge.1))
  
  r1 = data.frame()
  for (i in xm1){
    # i = "Atlantic bluefin tuna, Bigeye tuna, Blackfin tuna, Pacific bluefin tuna, Skipjack tuna, Tuna and bonito, Yellowfin tuna"
    dm = filter(y_join_1,merge.1==i)
    pop = prop.table(dm[7])
    dm['f1'] = dm$value1*pop
    r1 = rbind(r1,dm)
  }
  
  ##Data matching-2
  s2_1_t = s2_1[,c(1,3)]
  names(s2_1_t) = c('Aqua.Cap.items','value1')
  y_join_1 = left_join(x_join_5,s2_1_t,by=c('merge.2'='Aqua.Cap.items'))
  #xt_1 = filter(y_join_1,value1 >0 )
  xm1<-as.vector(unique(y_join_1$merge.2))
  
  r2 = data.frame()
  for (i in xm1){
    # i = "Atlantic bluefin tuna, Bigeye tuna, Blackfin tuna, Pacific bluefin tuna, Skipjack tuna, Tuna and bonito, Yellowfin tuna"
    dm = filter(y_join_1,merge.2==i)
    pop = prop.table(dm[7])
    dm['f1'] = dm$value1*pop
    r2 = rbind(r2,dm)
  }
  
  ##Data matching-3
  s2_1_t = s2_1[,c(1,3)]
  names(s2_1_t) = c('Aqua.Cap.items','value1')
  y_join_1 = left_join(x_join_5,s2_1_t,by=c('merge.3'='Aqua.Cap.items'))
  #xt_1 = filter(y_join_1,value1 >0 )
  xm1<-as.vector(unique(y_join_1$merge.3))
  
  r3 = data.frame()
  for (i in xm1){
    # i = "Atlantic bluefin tuna, Bigeye tuna, Blackfin tuna, Pacific bluefin tuna, Skipjack tuna, Tuna and bonito, Yellowfin tuna"
    dm = filter(y_join_1,merge.3==i)
    pop = prop.table(dm[7])
    dm['f1'] = dm$value1*pop
    r3 = rbind(r3,dm)
  }
  
  ##Data matching-4
  s2_1_t = s2_1[,c(1,3)]
  names(s2_1_t) = c('Aqua.Cap.items','value1')
  y_join_1 = left_join(x_join_5,s2_1_t,by=c('merge.4'='Aqua.Cap.items'))
  #xt_1 = filter(y_join_1,value1 >0 )
  xm1<-as.vector(na.omit(unique(y_join_1$merge.4)))
  
  r4 = data.frame()
  for (i in xm1){
    # i = "Atlantic bluefin tuna, Bigeye tuna, Blackfin tuna, Pacific bluefin tuna, Skipjack tuna, Tuna and bonito, Yellowfin tuna"
    dm = filter(y_join_1,merge.4==i)
    pop = prop.table(dm[7])
    dm['f1'] = dm$value1*pop
    r4 = rbind(r4,dm)
  }
  
  
  ##Data matching-5
  s2_1_t = s2_1[,c(1,3)]
  names(s2_1_t) = c('Aqua.Cap.items','value1')
  y_join_1 = left_join(x_join_5,s2_1_t,by=c('merge.5'='Aqua.Cap.items'))
  #xt_1 = filter(y_join_1,value1 >0 )
  xm1<-as.vector(na.omit(unique(y_join_1$merge.5)))
  
  r5 = data.frame()
  for (i in xm1){
    # i = "Atlantic bluefin tuna, Bigeye tuna, Blackfin tuna, Pacific bluefin tuna, Skipjack tuna, Tuna and bonito, Yellowfin tuna"
    dm = filter(y_join_1,merge.5==i)
    pop = prop.table(dm[7])
    dm['f1'] = dm$value1*pop
    r5 = rbind(r5,dm)
  }
  
  ##Data matching-6
  s2_2_t = s2[,c(1,3)]
  names(s2_2_t) = c('Aqua.Cap.items','value1')
  y_join_1 = left_join(x_join_5,s2_2_t,by='Aqua.Cap.items')
  #xt_1 = filter(y_join_1,value1 >0 )
  xm1<-as.vector(na.omit(unique(y_join_1$Aqua.Cap.items)))
  
  r6 = data.frame()
  for (i in xm1){
    # i = "Atlantic bluefin tuna, Bigeye tuna, Blackfin tuna, Pacific bluefin tuna, Skipjack tuna, Tuna and bonito, Yellowfin tuna"
    dm = filter(y_join_1,Aqua.Cap.items==i)
    #pop = prop.table(dm$`2019`)
    dm['f2'] = dm$value1
    r6 = rbind(r6,dm)
  }
  
  ##Combine r1-r5
  #r1_5 = rbind(r1,r2,r3,r4,r5)
  #r1_5 = r1_5[,c("Aqua.Cap.items","f1")]
  #r1_5 = ddply(r1_5,.(Aqua.Cap.items),summarize,f3=sum(f1))
  
  
  ##Match r6 -- the result
  #r8 <- left_join(r6,r1_5,by = 'Aqua.Cap.items')
  # r8['f4']<-ifelse()
  
  fillna <- function(x){
    x[is.na(x )]<- 0;
    x
  }
  
  r6['f2']<-fillna( r6['f2'])
  #r8['f3']<-fillna( r8['f3'])
  r6['f4']<- r6[7]-r6['f2']
  # r8[11]<- r8['f4']
  #r6['f4'] <-r6['f2']
  
  
  r9<-r6[,c(1:6,15)]
  r9['year']=yi
  
  return(r9)}  

##### Aquaculture+capture+subtracted import-export-remaining export-last remaining export
base1 = names(m_01)[1:6]
base2 = names(export_m)[1:2]
yt = as.vector(names(m_01)[7:51])
country = as.vector(unique(m_01$Country))
dataf <- subset(export_m,grepl(",",Aqua.Cap.items))
countries <-as.vector(unique(dataf$Country))

tf = c()
for (i in countries){
  ii = which(country ==i)
  tf = append(tf,ii)
}
countries_f = country[-tf]

##step1
dt = data.frame()
for (ci in countries){
  # if(ci =="Peru"){next}
  for (yi in yt){
    result<-Vol_a(m_01,export_m,ci,yi)
    
    dt = rbind(dt,result)
  }
}

##step2
dt_f = data.frame()
for (ci in countries_f){
  #if(ci =="Peru"){next}
  
  for (yi in yt){
    result<-Vol_b(m_01,export_m,ci,yi)
    dt_f = rbind(dt_f,result)
  }
}
##step3
dt_result<-rbind(dt,dt_f)
##result
#dt <- dt_141[,c(2:13)]

dt_result_final <- dt_result %>%
  pivot_wider(names_from = year, 
              values_from = f4)

write.csv(dt_result_final,"Production+subtracted import-export-remaining export1-remaining export2.csv")

##The data greater than 0 turns to 0
positive <- function(x){
  x[x>0]<- 0;
  x
}

dt_result_final_copy <-dt_result_final

x1<-names(dt_result_final_copy)[7:51]

for (xi in x1){
  
  dt_result_final_copy[xi]<-positive(dt_result_final_copy[xi])
}

write.csv(dt_result_final_copy,"Production+subtracted import-export-remaining export1-remaining export2(positive value is 0).csv")

##Data less than 0 is converted to 0
positive <- function(x){
  x[x<0]<- 0;
  x
}

dt_result_final_copy <-dt_result_final

x1<-names(dt_result_final_copy)[7:51]

for (xi in x1){
  
  dt_result_final_copy[xi]<-positive(dt_result_final_copy[xi])
}

write.csv(dt_result_final_copy,"Production+subtracted import-export-remaining export1-remaining export2(negative value is 0).csv")
########################################################################################################################################
########################################################################################################################################
#####Description of the results
#The result of "Production+subtracted import-export-remaining export1-remaining export2(negative value is 0).csv" is theoretical apparent aquatic food domestic consumption of all countries.
#The difference of "Production+subtracted import-export-remaining export1-remaining export2(negative value is 0).csv" and "Aquaculture and capture and subtracted import production-integrated.csv" is theoretical export data.
#The difference of "Import" and "subtracted import" is theoretical reexport data
