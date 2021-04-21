

names <- fread('firstnames.csv')

#what is the quant lay out for name distribution 
names[firstname != 'ALL OTHER FIRST NAMES', summary(obs)]

pop_names <- names[firstname != 'ALL OTHER FIRST NAMES' & 
                     obs > 205,] #top 25% of observed names, by observation


#calculate white_diff, basically white percentage - (black%+hisp%+asian%)
pop_names[,white_diff := pctwhite - (pcthispanic+pctblack+pctapi+pct2prace)]
pop_names[,black_diff := pctblack - (pcthispanic+pctwhite+pctapi+pct2prace)]

write.csv(pop_names, 'pop_fn.csv')

#extract white fn with between 70-80% frequency and less than 15% black frequency
pop_names_w <- pop_names[order(-as.numeric(white_diff))][1:10,firstname]
pop_names_b <- pop_names[order(-as.numeric(black_diff))][1:10,firstname]

write.csv(pop_names_b, 'pn-b.csv')
write.csv(pop_names_w, 'pn-w.csv')

pop_names_w
pop_names_b

snames <- fread("https://api.census.gov/data/2000/surname?get=NAME,PCTWHITE,PCTBLACK,COUNT&RANK=1:200")

data.table::setnames(snames,old = "[[\"NAME\"", new = "NAME")
data.table::setnames(snames,old = "\"RANK\"]", new = "RANK")

#clean data
snames[,NAME := gsub('[\"[]', '', NAME)]
snames[,RANK := gsub('[\"]]', '', RANK)]
snames[,RANK := gsub('[\"]', '', RANK)]

#create a column that subtracts white% - black%, two highest pos wm/wf sn, 
#two greatest neg bm/bf sn

write.csv(snames[order(-as.numeric(PCTBLACK))], 'top_sn-b.csv')
write.csv(snames[order(-as.numeric(PCTWHITE))], 'top_sn-w.csv')

#now order and sample last names 
top_sn.b <- snames[order(-as.numeric(PCTBLACK))][1:2,NAME]
top_sn.w <- snames[order(-as.numeric(PCTWHITE))][1:2,NAME]

exp_sn.b <- sample(top_sn.b,size = 1) #m name
exp_sn.w <- sample(top_sn.w,size = 1) #m name

#survey builder
#structure: 
address_order <- sample(c('A','B','C','D'), size = 4, replace = TRUE)
address_order
form_gen <- sample(c('sc','sc','dc','dc'))
form_gen
structures <- sample(c('PPAA','PAPA','PAAP','AAPP','APAP','APPA'), replace = TRUE, size = 4)
structures
element_gen <- function(n = 4){
  res <- NA
  for (i in 1:n){
    res[i] <- list(sample(c('A','B'),size = 3, replace = TRUE))
  }
  res
}

elems <- element_gen()
elems

rg_assign <- sample(c('wm','wf','bm','bf'))
rg_assign



