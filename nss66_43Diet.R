install.packages("Hmisc")
nss66R <- read.csv("IndiaDiet/NSS66_DietR.csv")
nss66U <- read.csv("IndiaDiet/NSS66_DietU.csv")
nss43R <- read.csv("IndiaDiet/NSS43_DietR.csv")
nss43U <- read.csv("IndiaDiet/NSS43_DietU.csv")
nss50R <- read.csv("IndiaDiet/NSS50_DietR.csv")
nss50U <- read.csv("IndiaDiet/NSS50_DietU.csv")
#http://agritech.tnau.ac.in/animal_husbandry/ani_chik_breeds%20of%20chicken.html - chicken weight 1kg/bird

library(ggplot2)
library(dplyr)
library(tidyr)
library(quantreg)
library(Hmisc)


#selecting dataframes
#interested factors- demographics and exp/qty of each meat/eggs/fish type
#cooking/lighting code, meat exp/qty by type,
# hhtype, hhsize, dwelling ownership, 
#education of head, gender of head, mpce, household exp
# state
nss66_selR <- nss66R %>% 
  select(hhid=hhid, state=state, weight=hhwt, cooking=S1B3_v16, lighting=S1B3_v17, mpce2009=MPCE_MRPfinal,
         earningtype=S1B3_v04, dwellingtype=S1B3_v18, hhsize=S1B3_v01, hhgroup= S1B3_v06, religion= S1B3_v05,
         eggsqty=S1B56_v05200, eggsval=S1B56_v06200,
         fishqty=S1B56_v05201, fishval=S1B56_v06201,
         muttonqty=S1B56_v05202, muttonval=S1B56_v06202,
         beefqty=S1B56_v05203, beefval=S1B56_v06203,
         porkqty=S1B56_v05204, porkval=S1B56_v06204,
         chickenqty=S1B56_v05205, chickenval=S1B56_v06205,
         othersqty=S1B56_v05206, othersval=S1B56_v06206, 
         totnonvegval=S1B56_v06209)

nss66_selU <- nss66U %>% 
  select(hhid=hhid, state=state, weight=hhwt, cooking=S1B3_v16, lighting=S1B3_v17, mpce2009=MPCE_MRPfinal,
         earningtype=S1B3_v04, dwellingtype=S1B3_v18, hhsize=S1B3_v01, hhgroup= S1B3_v06, religion= S1B3_v05,
         eggsqty=S1B56_v05200, eggsval=S1B56_v06200,
         fishqty=S1B56_v05201, fishval=S1B56_v06201,
         muttonqty=S1B56_v05202, muttonval=S1B56_v06202,
         beefqty=S1B56_v05203, beefval=S1B56_v06203,
         porkqty=S1B56_v05204, porkval=S1B56_v06204,
         chickenqty=S1B56_v05205, chickenval=S1B56_v06205,
         othersqty=S1B56_v05206, othersval=S1B56_v06206,
         totnonvegval=S1B56_v06209)

rm(nss66R, nss66U)
#chicken in no. of birds for 43rd. But assuming each bird = 1kg in weight
nss43_selR <- nss43R %>% 
  select(hhid=hhid, state=state, weight=multfinal, cooking=Ckgcode, lighting=Ligtgcode, mpce=MPCEfinal,
         earningtype=Hhtype, dwellingtype= DwellingCode, hhsize=Hhsize, hhgroup=Group, religion= Religion,
         eggsqty=TotalQtyEggs, eggsval=TotalValEggs,
         freshfishqty=TotalQtyFreshFish, freshfishval=TotalValFreshFish,
         dryfishqty=TotalQtyDryFish, dryfishval=TotalValDryFish,
         muttonqty=TotalQtyMutton, muttonval=TotalValMutton,
         goatmeatqty=TotalQtyGoatMeat, goatmeatval=TotalValGoatMeat,
         beefqty=TotalQtyBeef, beefval=TotalValBeef,
         buffaloqty=TotalQtyBuffalo, buffaloval=TotalValBuffalo,
         porkqty=TotalQtyPork, porkval=TotalValPork,
         chickenqty=TotalQtyPoultry, chickenval=TotalValPoultry,
         othermeatqty=TotalQtyOtherMeat, othermeatval=TotalValOtherMeat,
         otherbirdqty=TotalQtyOtherBirds, otherbirdval=TotalValOtherBirds) %>% 
  filter(!is.na(mpce))

nss43_selU <- nss43U %>% 
  select(hhid=hhid, state=state, weight=Multfinal, cooking=Ckgcode, lighting=Ligtgcode, mpce=MPCEfinal,
         earningtype=Hhtype, dwellingtype= DwellingCode, hhsize=Hhsize, hhgroup=Group, religion= Religion,
         eggsqty=TotalQtyEggs, eggsval=TotalValEggs,
         freshfishqty=TotalQtyFreshFish, freshfishval=TotalValFreshFish,
         dryfishqty=TotalQtyDryFish, dryfishval=TotalValDryFish,
         muttonqty=TotalQtyMutton, muttonval=TotalValMutton,
         goatmeatqty=TotalQtyGoatMeat, goatmeatval=TotalValGoatMeat,
         beefqty=TotalQtyBeef, beefval=TotalValBeef,
         buffaloqty=TotalQtyBuffalo, buffaloval=TotalValBuffalo,
         porkqty=TotalQtyPork, porkval=TotalValPork,
         chickenqty=TotalQtyPoultry, chickenval=TotalValPoultry,
         othermeatqty=TotalQtyOtherMeat, othermeatval=TotalValOtherMeat,
         otherbirdqty=TotalQtyOtherBirds, otherbirdval=TotalValOtherBirds) %>% 
  filter(!is.na(mpce))

rm(nss43R, nss43U)

nss50_selR <- nss50R %>% 
  select(hhid=hhid, state=state, weight=multfinal, cooking=Ckgcode, lighting=Ligtgcode, mpce=MPCEfinal,
         dwellingtype= DwellingCode, hhsize=Hhsize, religion= Religion,
         eggsqty=TotalQtyEggs, eggsval=TotalValEggs,
         freshfishqty=TotalQtyFreshfish, freshfishval=TotalValFreshfish,
         dryfishqty=TotalQtyDryfish, dryfishval=TotalValDryfish,
         muttonqty=TotalQtyMutton, muttonval=TotalValMutton,
         goatmeatqty=TotalQtyGoat, goatmeatval=TotalValGoat,
         beefqty=TotalQtyBeef, beefval=TotalValBeef,
         buffaloqty=TotalQtyBuffalo, buffaloval=TotalValBuffalo,
         porkqty=TotalQtyPork, porkval=TotalValPork,
         chickenqty=TotalQtyChicken, chickenval=TotalValChicken,
         otherbirdqty=TotalQtyOtherbird, otherbirdval=TotalValOtherbird,
         othermeatqty=TotalQtyOther, othermeatval=TotalValOther) %>% 
  filter(!(mpce==0|hhsize==0))

nss50_selU <- nss50U %>% 
  select(hhid=hhid, state=state, weight=multfinal, cooking=Ckgcode, lighting=Ligtgcode, mpce=MPCEfinal,
         dwellingtype= DwellingCode, hhsize=Hhsize, religion= Religion,
         eggsqty=TotalQtyEggs, eggsval=TotalValEggs,
         freshfishqty=TotalQtyFreshfish, freshfishval=TotalValFreshfish,
         dryfishqty=TotalQtyDryfish, dryfishval=TotalValDryfish,
         muttonqty=TotalQtyMutton, muttonval=TotalValMutton,
         goatmeatqty=TotalQtyGoat, goatmeatval=TotalValGoat,
         beefqty=TotalQtyBeef, beefval=TotalValBeef,
         buffaloqty=TotalQtyBuffalo, buffaloval=TotalValBuffalo,
         porkqty=TotalQtyPork, porkval=TotalValPork,
         chickenqty=TotalQtyChicken, chickenval=TotalValChicken,
         otherbirdqty=TotalQtyOtherbird, otherbirdval=TotalValOtherbird,
         othermeatqty=TotalQtyOther, othermeatval=TotalValOther) %>% 
  filter(!(mpce==0|hhsize==0))

nss43_selR[is.na(nss43_selR)] <- 0
nss43_selU[is.na(nss43_selU)] <- 0
nss66_selR[is.na(nss66_selR)] <- 0
nss66_selU[is.na(nss66_selU)] <- 0
nss50_selR[is.na(nss50_selR)] <- 0
nss50_selU[is.na(nss50_selU)] <- 0


hist(nss43_selU$beefqty, xlim = c(0,20), breaks = 20)
hist(nss66_selU$beefqty, xlim = c(0,20), breaks = 20)
nss43_selU %>% ggplot(aes(chickenqty))+geom_histogram()+xlim(c(0,25))+ylim(c(0,1000))
nss66_selU %>% ggplot(aes(chickenqty))+geom_histogram()+xlim(c(0,25))+ylim(c(0,1000))

nss43_selR <- nss43_selR %>% 
  mutate(year=1987, sector="Rural", hhexp=hhsize*mpce, 
         fishqty=freshfishqty+dryfishqty,
         fishval=freshfishval+dryfishval,
         muttonqty=muttonqty+goatmeatqty,
         muttonval=muttonval+goatmeatval,
         beefqty=beefqty+buffaloqty,
         beefval=beefval+buffaloval,
         othersqty=othermeatqty+otherbirdqty,
         othersval=othermeatval+otherbirdval,
         totmeatqty=muttonqty+beefqty+porkqty+chickenqty,
         totmeatval=muttonval+beefval+porkval+chickenval)
nss43_selU <- nss43_selU %>% 
  mutate(year=1987, sector="Urban", hhexp=hhsize*mpce, 
         fishqty=freshfishqty+dryfishqty,
         fishval=freshfishval+dryfishval,
         muttonqty=muttonqty+goatmeatqty,
         muttonval=muttonval+goatmeatval,
         beefqty=beefqty+buffaloqty,
         beefval=beefval+buffaloval,
         othersqty=othermeatqty+otherbirdqty,
         othersval=othermeatval+otherbirdval,
         totmeatqty=muttonqty+beefqty+porkqty+chickenqty,
         totmeatval=muttonval+beefval+porkval+chickenval)

nss66_selR <- nss66_selR %>% 
  mutate(year=2009, sector="Rural",hhexp2009=hhsize*mpce2009, 
         totmeatqty=muttonqty+beefqty+porkqty+chickenqty,
         totmeatval=muttonval+beefval+porkval+chickenval)
nss66_selU <- nss66_selU %>% 
  mutate(year=2009, sector="Urban",hhexp2009=hhsize*mpce2009, 
         totmeatqty=muttonqty+beefqty+porkqty+chickenqty,
         totmeatval=muttonval+beefval+porkval+chickenval)

nss50_selR <- nss50_selR %>% 
  mutate(year=1993, sector="Rural", hhexp=hhsize*mpce, 
         fishqty=freshfishqty+dryfishqty,
         fishval=freshfishval+dryfishval,
         muttonqty=muttonqty+goatmeatqty,
         muttonval=muttonval+goatmeatval,
         beefqty=beefqty+buffaloqty,
         beefval=beefval+buffaloval,
         othersqty=othermeatqty+otherbirdqty,
         othersval=othermeatval+otherbirdval,
         totmeatqty=muttonqty+beefqty+porkqty+chickenqty,
         totmeatval=muttonval+beefval+porkval+chickenval)
nss50_selU <- nss50_selU %>% 
  mutate(year=1993, sector="Urban", hhexp=hhsize*mpce, 
         fishqty=freshfishqty+dryfishqty,
         fishval=freshfishval+dryfishval,
         muttonqty=muttonqty+goatmeatqty,
         muttonval=muttonval+goatmeatval,
         beefqty=beefqty+buffaloqty,
         beefval=beefval+buffaloval,
         othersqty=othermeatqty+otherbirdqty,
         othersval=othermeatval+otherbirdval,
         totmeatqty=muttonqty+beefqty+porkqty+chickenqty,
         totmeatval=muttonval+beefval+porkval+chickenval)


nss66 %>% ggplot(aes(log(mpce)))+geom_histogram()+facet_wrap(~sector)
nss43 %>% ggplot(aes(log(mpce)))+geom_histogram()+facet_wrap(~sector)

#changing to 1987 Rupees
#from RBI: CPI 2009 base 2001=163, calculated CPI 87 base 2001=24.5
# from calculator stack and other source (in spreadsheet) multiplying factor=0.19
nss66_selU <- nss66_selU %>%
  mutate(mpce=mpce2009*24.5/163, hhexp=hhexp2009*24.5/163) %>% 
  select(-c(mpce2009, hhexp2009))
nss66_selR <- nss66_selR %>%
  mutate(mpce=mpce2009*24.5/163, hhexp=hhexp2009*24.5/163) %>% 
  select(-c(mpce2009, hhexp2009))

library(Hmisc)
#below for splitting by deciles
#qntilecuts <- seq(0,1,by=0.1)
#splitting by mpce- 12 classes; deciles and bottom/top split into 2
qntilecuts <- c(0.00, 0.05, seq(0.1,0.9,by=0.1), 0.95, 1)
mpceclasses66R <- cut(nss66_selR$mpce,breaks = wtd.quantile(nss66_selR$mpce, weights=nss66_selR$weight, probs = qntilecuts),labels = 11:0, include.lowest = TRUE)
levels(mpceclasses66R)
mpceclasses66U <- cut(nss66_selU$mpce,breaks = wtd.quantile(nss66_selU$mpce, weights=nss66_selU$weight, probs = qntilecuts),labels = 11:0, include.lowest = TRUE)
levels(mpceclasses66U)
#wtd.quantile and weight both needed to represent population- wtd.quantile divides population up into quantiles, while weights scales survey sample to population
nss66_selR$mpcecodebyPctile <- mpceclasses66R
nss66_selU$mpcecodebyPctile <- mpceclasses66U
#issue with Rural nss43- lots of mpce=0 ; filtered out above

mpceclasses43R <- cut(nss43_selR$mpce,breaks = wtd.quantile(nss43_selR$mpce, weights=nss43_selR$weight, probs = qntilecuts), labels=11:0, include.lowest = TRUE)
levels(mpceclasses43R)
mpceclasses43U <- cut(nss43_selU$mpce,breaks = wtd.quantile(nss43_selU$mpce, weights=nss43_selU$weight, probs = qntilecuts),labels = 11:0,  include.lowest = TRUE)
levels(mpceclasses43U)
nss43_selR$mpcecodebyPctile <- mpceclasses43R
nss43_selU$mpcecodebyPctile <- mpceclasses43U

mpceclasses50R <- cut(nss50_selR$mpce,breaks = unique(wtd.quantile(nss50_selR$mpce, weights=nss50_selR$weight, probs = qntilecuts)),labels = 11:0, include.lowest = TRUE)
levels(mpceclasses50R)
mpceclasses50U <- cut(nss50_selU$mpce,breaks = wtd.quantile(nss50_selU$mpce, weights=nss50_selU$weight, probs = qntilecuts),labels = 11:0,  include.lowest = TRUE)
levels(mpceclasses50U)
nss50_selR$mpcecodebyPctile <- mpceclasses50R
nss50_selU$mpcecodebyPctile <- mpceclasses50U

#stitch rural and urban together
nss66 <- rbind(nss66_selR, nss66_selU)
nss43 <- rbind(nss43_selR, nss43_selU)
nss50 <- rbind(nss50_selR, nss50_selU)

nss43 <- nss43 %>% select(-c(freshfishqty, freshfishval, dryfishqty, dryfishval, goatmeatqty, goatmeatval, buffaloqty, buffaloval, othermeatqty, othermeatval, otherbirdqty, otherbirdval))

nss66_43 <- bind_rows(nss43,nss50,nss66)
str(nss66_43)
table(nss66_43$mpcecodebyPctile, nss66_43$year, nss66_43$sector)
rm(nss66_selR, nss66_selU, nss43_selU, nss43_selR)

#changing var names
nss66_43$cooking[nss66_43$cooking == 'Gas-Coal/oil/natural'] <- 'LPG'
nss66_43$cooking[nss66_43$cooking == 'Firewood and chips'] <- 'Firewood'
nss66_43$cooking[nss66_43$cooking == 'Dung cake'] <- 'Dung'
nss66_43$cooking[nss66_43$cooking == 'Coke, coal'] <- 'CokeCoal'
nss66_43$cooking[nss66_43$cooking == 'Gobar gas'] <- 'Gobargas'
nss66_43$cooking[nss66_43$cooking == 'No cooking arrangement'] <- 'NoCooking'
nss66_43$lighting[nss66_43$lighting == 'No lighting arrangement'] <- 'NoLighting'
nss66_43$lighting[nss66_43$lighting == 'Other oil'] <- 'Others'
nss66_43$earningtype[nss66_43$earningtype == 'Regular wage/Salary'] <- 'Regular wage'
nss66_43$earningtype[nss66_43$earningtype == 'Regular wage earner'] <- 'Regular wage'
nss66_43$earningtype[nss66_43$earningtype == 'Self-employed (in non-agriculture if rural)'] <- 'Self-employed'
nss66_43$dwellingtype[nss66_43$dwellingtype == 'Other'] <- 'Others'
nss66_43$dwellingtype[nss66_43$dwellingtype == 'No dwelling'] <- 'No dwelling unit'
nss66_43$hhgroup[nss66_43$hhgroup %in% c('Scheduled castes', 'Scheduled caste', 'Scheduled tribe', 'Scheduled tribes')] <- 'SC/ST'
nss66_43$hhgroup[nss66_43$hhgroup != 'SC/ST'] <- 'Others'
nss66_43$lighting[nss66_43$lighting == 'NoLighting'] <- 'Others'

#convert the character variables to factors, do this after name change
nss66_43[sapply(nss66_43, is.character)] <- lapply(nss66_43[sapply(nss66_43, is.character)],as.factor)
nss66_43$year <- as.factor(nss66_43$year)

table(is.na(nss66_43$weight), nss66_43$year)
table(is.na(nss66_43$hhsize)|nss66_43$hhsize==0)
nss66_43 <- nss66_43 %>% filter(!(is.na(hhsize)|hhsize==0))
write.csv(nss66_43, "nss66_43_2016_diet.csv")

nss66_43 %>% ggplot(aes(log(mpce)))+geom_histogram(binwidth=0.1)+facet_grid(sector~year)

meanqty <- nss66_43 %>% group_by(year, sector) %>%  summarise(chicken=wtd.mean(chickenqty, weights=weight),
                                                              mutton=wtd.mean(muttonqty, weights=weight),
                                                              beef=wtd.mean(beefqty, weights=weight),
                                                              pork=wtd.mean(porkqty, weights=weight))
                                     
meanqtypc <- nss66_43 %>% group_by(year, sector, mpcecodebyPctile) %>%  summarise(chickenpc=wtd.mean((chickenqty/hhsize), weights=weight),
                                                              muttonpc=wtd.mean((muttonqty/hhsize), weights=weight),
                                                              beefpc=wtd.mean((beefqty/hhsize), weights=weight),
                                                              porkpc=wtd.mean((porkqty/hhsize), weights=weight),
                                                              totmeatpc=wtd.mean((totmeatqty/hhsize), weights=weight))

meanexppc <- nss66_43 %>% group_by(year, sector) %>%  summarise(chickenpc=wtd.mean((chickenval/hhsize), weights=weight),
                                                                muttonpc=wtd.mean((muttonval/hhsize), weights=weight),
                                                                beefpc=wtd.mean((beefval/hhsize), weights=weight),
                                                                porkpc=wtd.mean((porkval/hhsize), weights=weight),
                                                                totmeatpc=wtd.mean((totmeatval/hhsize), weights=weight))

#see Jabir Ali paper- need 1993 data too

nss66_43meateaters <- nss66_43 %>%  mutate(meateater=as.factor(ifelse(totmeatqty>0,1,0)),
                                           beefeater=as.factor(ifelse(beefqty>0, 1,0)),
                                           chickeneater=as.factor(ifelse(chickenqty>0,1,0)),
                                           muttoneater=as.factor(ifelse(muttonqty>0,1,0)),
                                           porkeater=as.factor(ifelse(porkqty>0,1,0)))

nss66_43meateaterssum <- nss66_43meateaters %>% #filter(religion%in% c("Hinduism", "Islam")) %>% 
  group_by(year, sector,  meateater) %>% 
  tally(wt=weight) %>% 
  group_by(year, sector) %>% mutate(perc=n*100/sum(n)) %>% 
  filter(meateater==1)


nss66_43beefeaterssum <- nss66_43meateaters %>% filter(religion%in% c("Hinduism", "Islam")) %>% 
  group_by(year, sector, religion, beefeater) %>% 
  tally(wt=weight) %>% 
  group_by(year, sector, religion) %>%
  mutate(perc=n*100/sum(n)) %>% 
  filter(beefeater==1)
nss66_43beefeaterssum %>% ggplot(aes(year, perc, fill=religion))+geom_bar(stat = 'identity', position='dodge')+ylab('% of households in each religious group')+ggtitle('Households reporting beef/buffalo meat consumption')+facet_wrap(~sector)
#ggsave("beefsumm_religion.png")
nss66_43chickeneaterssum <- nss66_43meateaters %>% group_by(year, sector, religion, chickeneater) %>% 
  tally(wt=weight) %>% 
  group_by(year, sector) %>%
  mutate(perc=n*100/sum(n)) %>% 
  filter(chickeneater==1)
nss66_43muttoneaterssum <- nss66_43meateaters %>% group_by(year, sector, religion, muttoneater) %>% 
  tally(wt=weight) %>% 
  group_by(year, sector) %>%
  mutate(perc=n*100/sum(n)) %>% 
  filter(muttoneater==1)
nss66_43porkeaterssum <- nss66_43meateaters %>% group_by(year, sector, religion, porkeater) %>% 
  tally(wt=weight) %>% 
  group_by(year, sector) %>%
  mutate(perc=n*100/sum(n)) %>% 
  filter(porkeater==1)

#graph on kg meat
nssannualmeanqty <- nss66_43 %>% filter(!(is.na(mpcecodebyPctile)|hhsize==0)) %>% 
  group_by(year, sector, mpcecodebyPctile) %>% 
  summarise(chickenpercapita=wtd.mean((chickenqty*12/hhsize), weights=weight),
            muttonpercapita=wtd.mean((muttonqty*12/hhsize), weights=weight),
            beefpercapita=wtd.mean((beefqty*12/hhsize), weights=weight),
            porkpercapita=wtd.mean((porkqty*12/hhsize), weights=weight),
            totmeatpc=wtd.mean((totmeatqty*12/hhsize), weights=weight))
nssannualmeanqty_reshape <- nssannualmeanqty %>% select(-totmeatpc) %>% gather(meat,value, 4:7)
nssannualmeanqty_reshape %>% ggplot(aes(mpcecodebyPctile, value, fill=meat))+
  geom_bar(stat='identity')+facet_grid(year~sector)+
  ylab("kgs/year")+xlab("Expenditure group: 11=bottom 5% 0=top 5%")
#ggsave("meanannualpc.png")



