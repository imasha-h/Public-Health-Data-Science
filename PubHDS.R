if (!require("dplyr")) {install.packages("dplyr")}
if (!require("readr")) {install.packages("readr")}
if (!require("ggplot2")) {install.packages("ggplot2")}
if (!require("tidyr")) {install.packages("tidyr")}
if (!require("reshape2")) {install.packages("reshape2")}
if (!require("corrplot")) {install.packages("corrplot")}
if (!require("cluster")) {install.packages("cluster")}
if (!require("leaflet")) {install.packages("leaflet")}
if (!require("sp")) {install.packages("sp")}

data <- fingertips_data(ProfileID = 19,AreaTypeID = 102)

indicators_auto <- filter(data, AreaType == "County & UA") %>% 
  group_by(IndicatorID) %>%
  filter(TimeperiodSortable == 20150000) %>% 
  filter(Sex == "Persons") %>%
  filter(Age == "All ages") %>%
  ungroup(IndicatorID) %>%
  select(IndicatorName, AreaName, Value) %>%
  spread(IndicatorName, Value) %>%
  as.data.frame()

rownames(indicators_auto) <- indicators_auto$AreaName
indicators_auto <- indicators_auto %>%
  select(-AreaName)

# No indicators_auto having missing values more than 20%
sum(colMeans(is.na(indicators_auto))>0.2)

median_imp <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
indicators_auto <- replace(indicators_auto, TRUE, lapply(indicators_auto, median_imp))

minmax_score <- function(x) return((x-min(x)) /(max(x)-min(x)))
indicators_auto <- apply(indicators_auto,2,minmax_score)

## Manual reversal of polarity
positive = c(11,12,14,17)
negative = setdiff(1:dim(indicators_auto)[2],positive)
indicators_auto[,negative] <- indicators_auto[,negative] * -1

corrplot(cor(indicators_auto),tl.pos = "n",type = "upper",method = "number")

indicators_auto <- indicators_auto[,c(1:2,5:19)]

##############################
## After manual selection  ##
##############################
indicators_auto <- indicators_auto[,c(5,9,11,14)]
indicators_manual <- data %>% filter(AreaType == "County & UA") %>%
  filter(IndicatorID %in% c(93088,92443,30305,22301,90366,90631,93347)) %>% #90631
  group_by(IndicatorName,AreaName) %>%
  filter(TimeperiodSortable == 20150000) %>% 
  summarise(mean_value=mean(Value,na.rm = TRUE)) %>%
  spread(IndicatorName, mean_value) %>%
  as.data.frame()
rownames(indicators_manual) <- indicators_manual$AreaName
indicators_manual <- indicators_manual %>%
  select(-AreaName)
indicators_manual <- replace(indicators_manual, TRUE, lapply(indicators_manual, median_imp))
indicators_manual <- apply(indicators_manual,2,minmax_score)
negative <- c(2,3,4,5)
indicators_manual[,negative] <- indicators_manual[,negative] * -1

indicators <- merge(indicators_auto,indicators_manual,by="row.names") %>%
  mutate(final_index_score = rowMeans(indicators[,2:11]))

## Show ranking
indicators[order(indicators$final_index_score,decreasing=TRUE),][,c(1,13)]

rownames(indicators) <- indicators$Row.names
indicators <- indicators %>%
  select(-Row.names)

# check order
# colnames(indicators)
par(mar=c(3,15,3,3)) #bottom, left, top, right
barplot(as.numeric(t(indicators["Kensington and Chelsea",1:11])),names.arg = c("Fuel Poverty","Cancer Diagnosed at Early Stage","Incidence of TB","Mortality Rate","Life Expectancy at Birth","School Readiness","% of overweight or obese adults ","Smoking Prevalence in Adults","Diabetes Diagnosis Rate","Self-Reported Wellbeing","Pop. Vaccination Coverage - MenC"),las=2,main="Kensington and Chelsea", horiz=TRUE, col=blues9)

# Fit k-means model (with 2 clusters)
clustering <- kmeans(indicators[,1:11], 2)

# Visualize the clustering
clusplot(indicators[1:11], clustering$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

cat1<- indicators[,c(3,4,5,9)]
cat2<- indicators[,c(2,7,8,11)]
cat3<- indicators[,c(1,6,10)]

cat1$mean_score <- rowMeans(cat1[,1:4])
cat2$mean_score <- rowMeans(cat2[,1:4])
cat3$mean_score <- rowMeans(cat3[,1:3])

# Show Ranking
cat1[order(cat1$mean_score, decreasing=TRUE),][5]
cat2[order(cat2$mean_score, decreasing=TRUE),][5]
cat3[order(cat3$mean_score, decreasing=TRUE),][4]

indicators[order(indicators$cat1,decreasing=TRUE),][,c(1,5)]

barplot(as.numeric(t(indicators["Kensington and Chelsea",1:4])),names.arg = c("Incidence of TB","Mortality Rate","Life Expectancy at Birth","Diabetes Diagnosis Rate"),las=2,main="Kensington and Chelsea", horiz=TRUE, col=blues9)

# Static Map
map_static <- fingertipscharts::map(data=df_overall, ons_api = ons_api, area_code = AreaCode, name_for_label = AreaName, fill = Category, stype = 'static', value = index, title = "Health Index Disaggregation by UTLA", copyright_size = 0.75)

map_static
