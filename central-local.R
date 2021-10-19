library(tidyverse)
library(lubridate)
library(pinyin)
library(igraph)
library(tnet)
library(intergraph)
library(statnet)
library(stargazer)

#combine the tables by row in southwest and northeast region. 
NEtotal1954$`District Bureaus` <- NA
NEtotal1954$`Local (District)` <- NA
NEtotal1954$`District Secretary` <- NA
NEtotal1954$`District Deputy Secretary` <- NA
NEtotal1954$`District SC Members` <- NA
NEtotal1954$`District Members` <- NA

#region combine.
region1956 <- rbind(CStotal1956, NCtotal1956, ECtotal1956, NWtotal1956, NEtotal1956, SWtotal1956)
#change the military NA. 
region1956$`PLA Military Field in 1949` <- region1956$`PLA Military Field in 1949` %>%
  replace_na("Civilian")
#li zhuoran case.   
region1954$`PLA Military Field in 1949`[222] <- "Northwest" 

region1956$`Name (Chinese)`[212] <- "廖志高"



#Set year started function.
Central_state_members_$`Year started` <- as.Date(as.character(Central_state_members_$`Year started`), format = "%Y")
Central_state_members_$`Year started` <- year(Central_state_members_$`Year started`)
#Set year ended. 
Central_state_members_$`Year ended` <- as.Date(as.character(Central_state_members_$`Year ended`), format = "%Y")
Central_state_members_$`Year ended` <- year(Central_state_members_$`Year ended`)

Central_state_members_$year <- mapply(seq, Central_state_members_$`Year started`, 
                                 Central_state_members_$`Year ended`, SIMPLIFY = F)

#unnest the year. 
centralpanal <- Central_state_members_ %>%
  unnest(year) %>%
  select(-`Year started`, -`Year ended`)

#select the year. 
central1956 <- centralpanal %>%
  filter(year == 1956)
#li fuchun case.
central1954 <- central1954[-86,]
#He Long case.
central1956$`civil/military in 1949`[38] <- "Northwest"
central1954$`Director/Vice Premier/Group Leader`[c(95,96,97,98,99,100,101)] <- NA
central1954$`Director/Vice Premier/Group Leader`[c(seq(from=95,to=101))] <- NA
central1954$Deputy[c(seq(from=95,to=101))] <- 1

#select the region personnel and working space. 
province1956 <- region1956 %>%
  select(c(`Name (Chinese)`, Province,`PLA Military Field in 1949`, year)) %>%
  rename("Institution" = "Province")
regional1954 <- region1954 %>%
  select(c(`Name (Chinese)`, `Regional Bureaus`, `PLA Military Field in 1949`, year)) %>%
  rename("Institution" = "Regional Bureaus")
diswork1954 <- region1954 %>%
  select(c(`Name (Chinese)`,`District Bureaus`, `PLA Military Field in 1949`, year)) %>%
  rename("Institution" = "District Bureaus")
reglist1954 <- province1954 %>%
  bind_rows(regional1954)
reglist1954 <- reglist1954 %>%
  bind_rows(diswork1954)

reglist1954 <- reglist1954[complete.cases(reglist1954),]

province1956 <- province1956 %>%
  rename("civil/military in 1949" = "PLA Military Field in 1949")

#CSwork1949 <- left_join(CSwork1949, CStotal1949[,c("Name (Chinese)","Place of Birth (Province)", "PLA Military Field in 1949")], by = "Name (Chinese)")

#combine the list of central and local cadres. 
central1956 <- central1956 %>%
  rename("Name (Chinese)" = "Members")
centralnew1956 <- central1956 %>%
  select(`Name (Chinese)`, Institution, `civil/military in 1949`, year) 
list1956 <- rbind(centralnew1956, province1956) 

#list1954 <- list1954 %>%
#  left_join(region1954[, c("Name (Chinese)", "PLA Military Field in 1949")], by = "Name (Chinese)") %>%
#  rename("civil/military in 1949" = "PLA Military Field in 1949")

list1956 <- list1956 %>%
  distinct()

#regional leaders after 1955
militaryafter1954 <- region1954 %>%
  select(`Name (Chinese)`, `PLA Military Field in 1949`) 
militaryafter1954 <- militaryafter1954 %>%
  rename("civil/military in 1949" = "PLA Military Field in 1949")

list1956 <- list1956 %>%
  left_join(militaryafter1954[, c("Name (Chinese)", "civil/military in 1949")], by = "Name (Chinese)")

#insert military background of central leaders after 1955.  
list1956$`civil/military in 1949.x`[39] <- "Third"
list1956$`civil/military in 1949`[53] <- "Civilian"

list1956 <- list1956 %>%
  select(-`civil/military in 1949.y`) %>%
  rename("civil/military in 1949" = "civil/military in 1949.x")


#rename the core leaders. 
list1953$`civil/military in 1949` <- list1953$`civil/military in 1949` %>%
  replace_na("Core")

#Arrange the names into pinyin. 
mypy <- pydic(method = "toneless", multi = F, only_first_letter = F, dic = "pinyin")
list1956 <- list1956 %>%
  mutate(Name = py(`Name (Chinese)`, sep= " ", dic = mypy)) 
list1956$Name <- str_to_title(list1956$Name) 

#change the pinyin name to differentiate the same intonation.
list1955$Name[131] <- "Wang Yii Lun" 
list1954$Name[438] <- "Wang Yii Lun"
list1956$Name[217] <- "Yang Yii Chen"
list1952$`Name (Chinese)`[103] <- "吴德"
list1955$`Name (Chinese)`[206] <- "廖志高"
list1952$Name[120] <- "Tun De"
list1956$`civil/military in 1949`[99] <- "Guerrilla"


#region1949 <- region1949 %>%
#  mutate(Name = py(`Name (Chinese)`, sep= " ", dic = mypy)) 
#region1949$Name <- str_to_title(region1949$Name)

#profile dataframe. 
profile1956 <- list1956 %>%
  select(`Name (Chinese)`, Name, `civil/military in 1949`) %>%
  distinct()

#optional check. 
profile1954trial <- profile1954 %>%
  select(Name, `Name (Chinese)`) %>%
  distinct()

profile1952 <- profile1952[-32,]

#create the bipartite network.
listii1956 <- list1956 %>%
  select(Name, Institution) %>%
  distinct()

net1956 <- graph_from_data_frame(listii1956, directed = F, vertices = NULL)
bipartite.mapping(net1956)

V(net1956)$type <- bipartite_mapping(net1956)$type
V(net1956)$color <- ifelse(V(net1956)$type, "red", "green")
V(net1956)$shape <- ifelse(V(net1956)$type, "square", "circle")
E(net1956)$color <- "black"

plot(net1956, vertex.label.cex = 0.5, 
              vertex.size = 3, 
              vertex.label.color = "black")


#bipartite centrality degree. 
#net1950degree <- degree(net1950)
#net1950bet <- betweenness(net1950)
#net1950clo <- closeness(net1950)
#cenbi1950 <- data.frame(net1950degree, net1950bet, net1950clo)


#convert bipartite to one-mode network. 
bi1956 <- as_incidence_matrix(net1956)
person1956 <- bi1956 %*% t(bi1956)
diag(person1956) <- 0
#plot the matrix. 
persongraph1951 <- graph_from_adjacency_matrix(person1951,
                                               mode = "undirected",
                                               weighted = T)

#node attribute. 
V(persongraph1951)$military <- profile1951$`civil/military in 1949`

V(persongraph1951)$color <- ifelse(V(persongraph1951)$military == "Fourth" | V(persongraph1951)$military == "Central China"| V(persongraph1951)$military == "Northeast", "brown", 
                     ifelse(V(persongraph1951)$military == "First" | V(persongraph1951)$military == "Northwest", "green",
                     ifelse(V(persongraph1951)$military == "Second" | V(persongraph1951)$military == "Southwest", "purple",
                     ifelse(V(persongraph1951)$military == "Third" | V(persongraph1951)$military == "East China", "blue",
                     ifelse(V(persongraph1951)$military == "North China", "orange", 
                     ifelse(V(persongraph1951)$military == "Civilian" | V(persongraph1951)$military == "Guerrilla", "yellow", "red"))))))

#V(persongraph1949)$size <- closeness(persongraph1949) * 100000
#E(persongraph1949)$width <-  E(persongraph1949)$weight

#layouts.
L0 = layout_with_fr(persongraph1949)
L1 = layout_with_fr(persongraph1950)
L2 = layout_with_fr(persongraph1951)
L3 = layout_with_fr(persongraph1952)
L4 = layout_with_fr(persongraph1953)
L5 = layout_with_fr(persongraph1954)
L6 = layout_with_fr(persongraph1955)
L7 = layout_with_fr(persongraph1956)


plot(persongraph1949, vertex.label = NA, layout = L0,
     vertex.label.color = "black", sub = "1949")
plot(persongraph1950, vertex.label = NA, layout = L1,
     vertex.label.color = "black", sub = "1950")
plot(persongraph1951, vertex.label = NA, layout = L2,
     vertex.label.color = "black", sub = "1951")
plot(persongraph1952, vertex.label = NA, layout = L3,
     vertex.label.color = "black", sub = "1952")

plot(persongraph1953, vertex.label = NA, layout = L4,
     vertex.label.color = "black", sub = "1953")
plot(persongraph1954, vertex.label = NA, layout = L5,
     vertex.label.color = "black", sub = "1954")
plot(persongraph1955, vertex.label = NA, layout = L6,
     vertex.label.color = "black", sub = "1955")
plot(persongraph1956, vertex.label = NA, layout = L7,
     vertex.label.color = "black", sub = "1956")


#centrality degree. 
person1956bet <- betweenness(persongraph1956)
person1956clo <- closeness(persongraph1956)
cen1956 <- data.frame(person1956bet, person1956clo)

cen1956$Name <- rownames(cen1956)
profile1956 <- profile1956 %>%
  left_join(cen1956, by = "Name")

#weighted centrality degree.
profile1956$weightbet <- profile1956$bioscale * profile1956$person1956bet
profile1956$weightclo <- profile1956$bioscale * profile1956$person1956clo


#community module. 
#delete isolated vertices. 
isodeg1949 <- which(degree(persongraph1949) <= 3)
noiso1949 <- delete.vertices(persongraph1949, isodeg1949)
plot(noiso1949, vertex.label.cex = 0.5,
     vertex.size = 3,
     vertex.label.color = "black")

#community module. 
clupg1952 <- cluster_fast_greedy(persongraph1952)
btwpg1952 <- cluster_edge_betweenness(persongraph1952)

plot(clupg1956, persongraph1956, vertex.label = NA,
    vertex.label.color = "black")
plot(btwpg1949, persongraph1949, vertex.label = NA, 
     vertex.label.color = "black")

modularity(clupg1956)
modularity(btwpg1956)
length(clupg1954)
length(btwpg1954)


#delete isolated nodes and calculate the graph centralization. 
isoclo1956 = which(degree(persongraph1956) == 0)
close1956 = delete.vertices(persongraph1956, isoclo1956)

#centralization values and comparison. 
b8 <- centr_betw(persongraph1956, directed = F)$centralization
c8 <- centr_clo(persongraph1956, mode = "all")$centralization
cm8 <- modularity(clupg1956)
bm8 <- modularity(btwpg1956)
meanbt <- mean(b1,b2,b3,b4,b5,b6,b7,b8)
meanct <- mean(c1,c2,c3,c4,c5,c6,c7,c8)
meanbm <- mean(bm1,bm2,bm3,bm4,bm5,bm6,bm7,bm8)
meancm <- mean(cm1,cm2,cm3,cm4,cm5,cm6,cm7,cm8)
btclucenratio <- meanbm/meanbt
ctclucenratio <- meancm/meanct


#isolated noded deleted centralization. 
centr_betw(close1956, directed = F)$centralization
centr_clo(close1956, mode = "all")$centralization


#coerce the clique into dataframe. 
clique1956 <- as.data.frame(cbind(V(persongraph1956)$name, clupg1956$membership))
clique1956 <- clique1956 %>%
   rename(Name = V1, clique = V2) 
profile1956 <- profile1956 %>%
  left_join(clique1956, by = "Name")

profile1956 <- profile1956 %>%
  group_by(clique) %>%
  arrange(desc(bioscale))

profile1956 <- profile1956 %>%
  arrange(clique)


profile1956 %>%
  group_by(clique)%>%
  count() %>%
  arrange(desc(n))


plot(cebpg1950, persongraph1950, vertex.label.cex = 0.5, 
     vertex.size = 3,
     vertex.label.color = "black")

plot(cwpg1950, persongraph1950, vertex.label.cex = 0.5, 
     vertex.size = 3,
     vertex.label.color = "black")


clupg1950$membership
clupg1950$modularity

#continued. 
central1956[is.na(central1956)] <- 0

region1955$`Regional Bureaus`[is.na(region1955$`Regional Bureaus`)] <- "None"
region1955$`District Bureaus`[is.na(region1955$`District Bureaus`)] <- "None"
region1956$Province[is.na(region1956$Province)] <- "None"
region1956$`Place of Birth (Province)`[is.na(region1956$`Place of Birth (Province)`)] <- "Unknown"
region1956[is.na(region1956)] <- 0 

#bioscale. 
central1956$bioscale <- ifelse(central1956$`Chair/Premier` == 1, 10, 
                               ifelse(central1956$`Director/Vice Premier/Group Leader` == 1, 9, 8))

central1956$bioscale[c(9,10)] <- 6

region1954$bioscale <- ifelse(region1954$`Regional Secretary` == 1, 7, 
                      ifelse(region1954$`Regional Deputy Secretary` == 1, 6, 
                      ifelse(region1954$`Regional SC Members`== 1, 5,
                      ifelse(region1954$`Regional Members` == 1, 4,
                      ifelse(region1954$`District Secretary` == 1, 3.5,
                      ifelse(region1954$`District Deputy Secretary` == 1, 2.5,      
                      ifelse(region1954$`District SC Members` == 1 | region1954$`District Members` == 1, 1.5,
                      ifelse(region1954$`Provincial Secretary` == 1, 3,
                      ifelse(region1954$`Provincial Deputy Secretary`== 1, 2, 1
                                                                ))))))))) 


region1956$bioscale <- ifelse(region1956$`Provincial Secretary` == 1, 3,
                       ifelse(region1956$`Provincial Deputy Secretary`== 1, 2, 1)) 


biolist1956 <- region1956 %>%
  select(`Name (Chinese)`, bioscale) %>%
  distinct() %>%
  arrange(desc(bioscale))
#delete the duplicated names with smaller bioscale value. 
biolist1956 <- biolist1956[!duplicated(biolist1956[1]),]

biotop1956 <- central1956 %>%
  select(`Name (Chinese)`, bioscale) %>%
  distinct() %>%
  arrange(desc(bioscale))
#delete the duplicated names with smaller bioscale value. 
biotop1956 <- biotop1956[!duplicated(biotop1956[1]),]


biolist1956 <- biolist1956 %>%
  bind_rows(biotop1956) %>%
  distinct() %>%
  arrange(desc(bioscale))

#delete the duplicated names with smaller bioscale value. 
biolist1956 <- biolist1956[!duplicated(biolist1956[1]),]

#delete incorrect names. 
biolist1955 <- biolist1955[-139,]

profile1951$bioscale[57] <- 3.0

#insert into the profile. 
profile1956 <- profile1956 %>%
  left_join(biolist1956, by = "Name (Chinese)")

profile1949 <- profile1949 %>%
  select(-bioscale.x) %>%
  rename(bioscale = bioscale.y)

#multiply the parameter. 
profile1956 <- profile1956 %>%
  mutate(wgt1956btw = person1956bet * bioscale) %>%
  mutate(wgt1956clo = person1956clo * bioscale) 


plot(persongraph1956, vertex.label.cex = 0.5,
     vertex.size = 3,
     vertex.label.color = "black")

#bioscale into the node attribute. 
V(persongraph1951)$bioscale <- profile1951$bioscale
V(persongraph1951)$size <- V(persongraph1951)$bioscale



#convert the military profile. 
profile1956 <- profile1956 %>%
  mutate(military = ifelse(`civil/military in 1949` == "Fourth" | `civil/military in 1949` == "Central China"| `civil/military in 1949` == "Northeast", "fourth", 
                    ifelse(`civil/military in 1949` == "First" | `civil/military in 1949` == "Northwest", "first",
                    ifelse(`civil/military in 1949` == "Second" | `civil/military in 1949` == "Southwest", "second",
                    ifelse(`civil/military in 1949` == "Third" | `civil/military in 1949` == "East China", "third",
                    ifelse(`civil/military in 1949` == "North China", "nchina", 
                    ifelse(`civil/military in 1949` == "Civilian" | `civil/military in 1949` == "Guerrilla", "civilforce", "cencadre")))))))

V(persongraph1956)$militarynew <- profile1956$military

#convert igraph to network. 
pg1949 <- asNetwork(persongraph1949)
pg1950 <- asNetwork(persongraph1950)
pg1951 <- asNetwork(persongraph1951)
pg1952 <- asNetwork(persongraph1952)
pg1953 <- asNetwork(persongraph1953)
pg1954 <- asNetwork(persongraph1954)
pg1955 <- asNetwork(persongraph1955)
pg1956 <- asNetwork(persongraph1956)

#use ergm to test the node homophily in each year from 1949 to 1956.
modiv1953 <- ergm(pg1953 ~ edges + nodefactor("militarynew") + nodematch("militarynew", diff = T)  + absdiff("bioscale") + nodecov("bioscale"))
modiii1953 <- ergm(pg1953 ~ edges + nodefactor("militarynew") + nodecov("bioscale") + absdiff("bioscale") + gwesp(.5,fixed=T))

summary(modiii1953)

plogis(coef(modiii1953)[['edges']]+coef(modiii1953)[['nodefactor.militarynew.fourth']]+coef(modiii1953)[['nodecov.bioscale']])



stargazer(modiii1949, modiv1949,
          modiii1950, modiv1950,
          modiii1951, modiv1951,
          modiii1952, modiv1952, title = "Results I, 1949-1952", 
          align = TRUE, 
          dep.var.labels = c("1949", "1950", "1951", "1952"),
          covariate.labels = c("edges", "Non-Regular Force", "PLA 1st", "PLA 4th", "PLA North China", "PLA 2nd", "PLA 3rd", 
                               "Central Colleague", "Colleague from Non-PLA", "PLA 1st colleague", "PLA 4th colleague", "PLA North China colleague", "PLA 2nd colleague", "PLA 3rd colleague",
                               "Bureaucratic Position", "Bureaucratic hierarchy"),
          font.size = "tiny",
          type = "latex",
          no.space = TRUE,
          out = "cadrenewi.latex")


stargazer(modiii1953, modiv1953,
          modiii1954, modiv1954,
          modiii1955, modiv1955,
          modiii1956, modiv1956, title = "Results II, 1953-1956", 
          align = TRUE, 
          dep.var.labels = c("1953", "1954", "1955", "1956"),
          covariate.labels = c("edges", "Non-Regular Force", "PLA 1st", "PLA 4th", "PLA North China", "PLA 2nd", "PLA 3rd", 
                               "Central Colleague", "Colleague from Non-PLA", "PLA 1st colleague", "PLA 4th colleague", "PLA North China colleague", "PLA 2nd colleague", "PLA 3rd colleague",
                               "Bureaucratic Position", "Bureaucratic hierarchy"),
          font.size = "tiny",
          type = "latex",
          no.space = TRUE,
          out = "cadrenewii.latex")



gof.modiii1954 <- gof(modiii1954)
par(mfrow = c(2,2))
plot(gof.modiii1954)



