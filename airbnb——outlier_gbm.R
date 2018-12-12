data = read.csv('analysisData.csv',stringsAsFactors = F)
#data = read.csv('scoringData.csv',stringsAsFactors = F)
names(data)
num.NA = sort(sapply(data, function(x) { sum(is.na(x))} ))
remain.col = names(num.NA[which(num.NA<0.8*dim(data)[1])])
data.sub = data[,remain.col]
sort(sapply(data.sub, function(x) { sum(is.na(x))} ))
#data.sub$security_deposit[which(is.na(data.sub$security_deposit))] = 0 #median(data.sub$security_deposit,na.rm=T)
data.sub$cleaning_fee[which(is.na(data.sub$cleaning_fee))] = median(data.sub$cleaning_fee,na.rm=T)
#data.sub$reviews_per_month[which(is.na(data.sub$reviews_per_month))] = median(data.sub$reviews_per_month,na.rm=T)
data.sub$beds[which(is.na(data.sub$beds))] = median(data.sub$beds,na.rm=T)
data.sub$cancellation_policy[which(data.sub$cancellation_policy=='flexible')] = 1
data.sub$cancellation_policy[which(data.sub$cancellation_policy=='moderate')] = 2
data.sub$cancellation_policy[which(data.sub$cancellation_policy=='strict')] = 3
data.sub$cancellation_policy[which(data.sub$cancellation_policy=='super_strict_30')] = 4
data.sub$cancellation_policy[which(data.sub$cancellation_policy=='super_strict_60')] = 4
data.sub$cancellation_policy = as.numeric(data.sub$cancellation_policy)
data.sub$host_response_time[which(data.sub$host_response_time=='a few days or more')] = 1
data.sub$host_response_time[which(data.sub$host_response_time=='N/A')] = 0
data.sub$host_response_time[which(data.sub$host_response_time=='within a day')] = 3
data.sub$host_response_time[which(data.sub$host_response_time=='within a few hours')] = 6
data.sub$host_response_time[which(data.sub$host_response_time=='within an hour')] = 10
data.sub$host_response_time = as.numeric(data.sub$host_response_time)
data.sub$host_is_superhost[which(data.sub$host_is_superhost=='f')] = 0
data.sub$host_is_superhost[which(data.sub$host_is_superhost=='t')] = 1
data.sub$host_is_superhost = as.numeric(data.sub$host_is_superhost)
data.sub$is_location_exact[which(data.sub$is_location_exact=='f')] = 0
data.sub$is_location_exact[which(data.sub$is_location_exact=='t')] = 1
data.sub$is_location_exact = as.numeric(data.sub$is_location_exact)
data.sub$instant_bookable[which(data.sub$instant_bookable=='f')] = 0
data.sub$instant_bookable[which(data.sub$instant_bookable=='t')] = 1
data.sub$instant_bookable = as.numeric(data.sub$instant_bookable)
data.sub$property_type[which(data.sub$property_type=='Apartment')] = 1
data.sub$property_type[which(data.sub$property_type!=1)] = 0
data.sub$property_type = as.numeric(data.sub$property_type)
data.sub$is_business_travel_ready[which(data.sub$is_business_travel_ready=='t')] = 1
data.sub$is_business_travel_ready[which(data.sub$is_business_travel_ready=='f')] = 0
data.sub$is_business_travel_ready = as.numeric(data.sub$is_business_travel_ready)
data.sub$latitude = (data.sub$latitude - 40) * 1000
data.sub$longitude = (data.sub$longitude + 74.3) * 1000
data.sub$transit[which(data.sub$transit=='')] = 0
data.sub$transit[which(data.sub$transit!=0)] = 1
data.sub$transit = as.numeric(data.sub$transit)
data.sub$access[which(data.sub$access=='')] = 0
data.sub$access[which(data.sub$access!=0)] = 1
data.sub$access = as.numeric(data.sub$access)


#one hot
data.sub$neighbourhood_Bronx=0
data.sub$neighbourhood_Bronx[which(data.sub$neighbourhood_group_cleansed=='Bronx')] = 1
data.sub$neighbourhood_Brooklyn=0
data.sub$neighbourhood_Brooklyn[which(data.sub$neighbourhood_group_cleansed=='Brooklyn')] = 1
data.sub$neighbourhood_Manhattan=0
data.sub$neighbourhood_Manhattan[which(data.sub$neighbourhood_group_cleansed=='Manhattan')] = 1
data.sub$neighbourhood_Queens=0
data.sub$neighbourhood_Queens[which(data.sub$neighbourhood_group_cleansed=='Queens')] = 1
data.sub$neighbourhood_Staten=0
data.sub$neighbourhood_Staten[which(data.sub$neighbourhood_group_cleansed=='Staten Island')] = 1

data.sub$bed_airbed = 0
data.sub$bed_airbed[which(data.sub$bed_type=='Airbed')] = 1
data.sub$bed_couch = 0
data.sub$bed_couch[which(data.sub$bed_type=='Couch')] = 1
data.sub$bed_futon = 0
data.sub$bed_futon[which(data.sub$bed_type=='Futon')] = 1
data.sub$bed_sofa = 0
data.sub$bed_sofa[which(data.sub$bed_type=='Pull-out Sofa')] = 1
data.sub$bed_real = 0
data.sub$bed_real[which(data.sub$bed_type=='Real Bed')] = 1

data.sub$room_apt=0
data.sub$room_apt[which(data.sub$room_type=='Entire home/apt')] = 1
data.sub$room_private=0
data.sub$room_private[which(data.sub$room_type=='Private room')] = 1
data.sub$room_shared=0
data.sub$room_shared[which(data.sub$room_type=='Shared room')] = 1

#amenity
sum((grepl("Cat", data.sub$amenities)==T))/nrow(data.sub)
data.sub$wifi = 0
data.sub$wifi[which((grepl("Wifi", data.sub$amenities)==T) | (grepl("Internet", data.sub$amenities)==T))] = 1
data.sub$heat = 0
data.sub$heat[which(grepl("Heating", data.sub$amenities)==T)] = 1
data.sub$air = 0
data.sub$air[which(grepl("Air conditioning", data.sub$amenities)==T)] = 1
data.sub$kitcken = 0
data.sub$kitcken[which(grepl("Kitchen", data.sub$amenities)==T)] = 1
data.sub$shampoo = 0
data.sub$shampoo[which(grepl("Shampoo", data.sub$amenities)==T)] = 1
data.sub$essential = 0
data.sub$essential[which(grepl("Essentials", data.sub$amenities)==T)] = 1
data.sub$elevator = 0
data.sub$elevator[which(grepl("Elevator", data.sub$amenities)==T)] = 1
data.sub$tv = 0
data.sub$tv[which((grepl("TV", data.sub$amenities)==T) | (grepl("Cable TV", data.sub$amenities)==T))] = 1
data.sub$gym = 0
data.sub$gym[which(grepl("Gym", data.sub$amenities)==T)] = 1
data.sub$washer = 0
data.sub$washer[which(grepl("Washer", data.sub$amenities)==T)] = 1
data.sub$dryer = 0
data.sub$dryer[which(grepl("Dryer", data.sub$amenities)==T)] = 1
data.sub$fridge = 0
data.sub$fridge[which(grepl("Refrigerator", data.sub$amenities)==T)] = 1
data.sub$self_check = 0
data.sub$self_check[which(grepl("Self check-in", data.sub$amenities)==T)] = 1
data.sub$hair = 0
data.sub$hair[which(grepl("Hair dryer", data.sub$amenities)==T)] = 1
data.sub$smart = 0
data.sub$smart[which(grepl("Smart lock", data.sub$amenities)==T)] = 1
data.sub$aid = 0
data.sub$aid[which(grepl("First aid kit", data.sub$amenities)==T)] = 1
data.sub$hanger = 0
data.sub$hanger[which(grepl("Hangers", data.sub$amenities)==T)] = 1
data.sub$co = 0
data.sub$co[which(grepl("Carbon monoxide detector", data.sub$amenities)==T)] = 1

#deal with price outliers
data.sub$price[which(data$price==0)] = mean(data.sub$price)
# data.sub$price[which(data$price>312)] = mean(data.sub$price)
# data.sub$price = log(data.sub$price)

data.sub = data.sub[,c('price','cleaning_fee','beds','host_response_time',
                       'host_is_superhost','neighbourhood_Bronx','neighbourhood_Brooklyn',
                       'neighbourhood_Manhattan','neighbourhood_Queens','neighbourhood_Staten',
                       'latitude','longitude','room_apt','room_private','room_shared',
                       'accommodates','bathrooms','bedrooms','guests_included','transit','access',
                       'minimum_nights','calculated_host_listings_count','property_type',
                       'review_scores_rating','reviews_per_month','is_business_travel_ready',
                       'review_scores_cleanliness','review_scores_checkin','is_location_exact',
                       'review_scores_location','review_scores_value','cancellation_policy',
                       'availability_30','availability_60','availability_90','availability_365','bed_airbed',
                       'bed_couch','bed_futon','bed_sofa','bed_real','instant_bookable',
                       'wifi','heat','air','kitcken','shampoo','essential','tv','gym',
                       'washer','dryer','fridge','self_check','hair','smart','aid','hanger','co')]
write.csv(data.sub, 'cleaned_data.csv',row.names = F)


#split train and test
set.seed(100)
sample_row = sample(1:dim(data)[1],0.8*dim(data)[1])
train.sub = data.sub[sample_row,]
test.sub = data.sub[-sample_row,]


#model
library(gbm)
boost=gbm(price~.,data=train.sub,distribution="gaussian",interaction.depth = 10,
          n.minobsinnode=10,keep.data = TRUE,n.trees = 500,shrinkage = 0.005,n.cores=8)
test.pred = predict(boost,test.sub,n.trees = 500)
test.error = sqrt(mean((test.pred-test.sub$price)^2)); test.error
train.pred = predict(boost,train.sub,n.trees = 500)
train.error = sqrt(mean((train.pred-train.sub$price)^2)); train.error


#scoring data
scoringData=read.csv("scoringData.csv",stringsAsFactors = F)
scoringData$cleaning_fee[which(is.na(scoringData$cleaning_fee))] = median(scoringData$cleaning_fee,na.rm=T)
scoringData$reviews_per_month[which(is.na(scoringData$reviews_per_month))] = median(scoringData$reviews_per_month,na.rm=T)
scoringData$beds[which(is.na(scoringData$beds))] = median(scoringData$beds,na.rm=T)
scoringData$cancellation_policy[which(scoringData$cancellation_policy=='flexible')] = 1
scoringData$cancellation_policy[which(scoringData$cancellation_policy=='moderate')] = 2
scoringData$cancellation_policy[which(scoringData$cancellation_policy=='strict')] = 3
scoringData$cancellation_policy[which(scoringData$cancellation_policy=='super_strict_30')] = 4
scoringData$cancellation_policy[which(scoringData$cancellation_policy=='super_strict_60')] = 4
scoringData$cancellation_policy = as.numeric(scoringData$cancellation_policy)
scoringData$host_response_time[which(scoringData$host_response_time=='a few days or more')] = 1
scoringData$host_response_time[which(scoringData$host_response_time=='N/A')] = 0
scoringData$host_response_time[which(scoringData$host_response_time=='within a day')] = 3
scoringData$host_response_time[which(scoringData$host_response_time=='within a few hours')] = 6
scoringData$host_response_time[which(scoringData$host_response_time=='within an hour')] = 10
scoringData$host_response_time = as.numeric(scoringData$host_response_time)
scoringData$host_is_superhost[which(scoringData$host_is_superhost=='f')] = 0
scoringData$host_is_superhost[which(scoringData$host_is_superhost=='t')] = 1
scoringData$host_is_superhost = as.numeric(scoringData$host_is_superhost)
scoringData$is_location_exact[which(scoringData$is_location_exact=='f')] = 0
scoringData$is_location_exact[which(scoringData$is_location_exact=='t')] = 1
scoringData$is_location_exact = as.numeric(scoringData$is_location_exact)
scoringData$instant_bookable[which(scoringData$instant_bookable=='f')] = 0
scoringData$instant_bookable[which(scoringData$instant_bookable=='t')] = 1
scoringData$instant_bookable = as.numeric(scoringData$instant_bookable)
scoringData$property_type[which(scoringData$property_type=='Apartment')] = 1
scoringData$property_type[which(scoringData$property_type!='Apartment')] = 0
scoringData$property_type = as.numeric(scoringData$property_type)
scoringData$is_business_travel_ready[which(scoringData$is_business_travel_ready=='t')] = 1
scoringData$is_business_travel_ready[which(scoringData$is_business_travel_ready=='f')] = 0
scoringData$is_business_travel_ready = as.numeric(scoringData$is_business_travel_ready)
scoringData$latitude = (scoringData$latitude - 40) * 1000
scoringData$longitude = (scoringData$longitude + 74.3) * 1000
scoringData$transit[which(scoringData$transit=='')] = 0
scoringData$transit[which(scoringData$transit!=0)] = 1
scoringData$transit = as.numeric(scoringData$transit)
scoringData$access[which(scoringData$access=='')] = 0
scoringData$access[which(scoringData$access!=0)] = 1
scoringData$access = as.numeric(scoringData$access)
#one hot
scoringData$neighbourhood_Bronx=0
scoringData$neighbourhood_Bronx[which(scoringData$neighbourhood_group_cleansed=='Bronx')] = 1
scoringData$neighbourhood_Brooklyn=0
scoringData$neighbourhood_Brooklyn[which(scoringData$neighbourhood_group_cleansed=='Brooklyn')] = 1
scoringData$neighbourhood_Manhattan=0
scoringData$neighbourhood_Manhattan[which(scoringData$neighbourhood_group_cleansed=='Manhattan')] = 1
scoringData$neighbourhood_Queens=0
scoringData$neighbourhood_Queens[which(scoringData$neighbourhood_group_cleansed=='Queens')] = 1
scoringData$neighbourhood_Staten=0
scoringData$neighbourhood_Staten[which(scoringData$neighbourhood_group_cleansed=='Staten Island')] = 1

scoringData$bed_airbed = 0
scoringData$bed_airbed[which(scoringData$bed_type=='Airbed')] = 1
scoringData$bed_couch = 0
scoringData$bed_couch[which(scoringData$bed_type=='Couch')] = 1
scoringData$bed_futon = 0
scoringData$bed_futon[which(scoringData$bed_type=='Futon')] = 1
scoringData$bed_sofa = 0
scoringData$bed_sofa[which(scoringData$bed_type=='Pull-out Sofa')] = 1
scoringData$bed_real = 0
scoringData$bed_real[which(scoringData$bed_type=='Real Bed')] = 1

scoringData$room_apt=0
scoringData$room_apt[which(scoringData$room_type=='Entire home/apt')] = 1
scoringData$room_private=0
scoringData$room_private[which(scoringData$room_type=='Private room')] = 1
scoringData$room_shared=0
scoringData$room_shared[which(scoringData$room_type=='Shared room')] = 1

#amenity
scoringData$wifi = 0
scoringData$wifi[which((grepl("Wifi", scoringData$amenities)==T) | (grepl("Internet", scoringData$amenities)==T))] = 1
scoringData$heat = 0
scoringData$heat[which(grepl("Heating", scoringData$amenities)==T)] = 1
scoringData$air = 0
scoringData$air[which(grepl("Air conditioning", scoringData$amenities)==T)] = 1
scoringData$kitcken = 0
scoringData$kitcken[which(grepl("Kitchen", scoringData$amenities)==T)] = 1
scoringData$shampoo = 0
scoringData$shampoo[which(grepl("Shampoo", scoringData$amenities)==T)] = 1
scoringData$essential = 0
scoringData$essential[which(grepl("Essentials", scoringData$amenities)==T)] = 1
scoringData$elevator = 0
scoringData$elevator[which(grepl("Elevator", scoringData$amenities)==T)] = 1
scoringData$tv = 0
scoringData$tv[which((grepl("TV", scoringData$amenities)==T) | (grepl("Cable TV", scoringData$amenities)==T))] = 1
scoringData$gym = 0
scoringData$gym[which(grepl("Gym", scoringData$amenities)==T)] = 1
scoringData$washer = 0
scoringData$washer[which(grepl("Washer", scoringData$amenities)==T)] = 1
scoringData$dryer = 0
scoringData$dryer[which(grepl("Dryer", scoringData$amenities)==T)] = 1
scoringData$fridge = 0
scoringData$fridge[which(grepl("Refrigerator", scoringData$amenities)==T)] = 1
scoringData$self_check = 0
scoringData$self_check[which(grepl("Self check-in", scoringData$amenities)==T)] = 1
scoringData$hair = 0
scoringData$hair[which(grepl("Hair dryer", scoringData$amenities)==T)] = 1
scoringData$smart = 0
scoringData$smart[which(grepl("Smart lock", scoringData$amenities)==T)] = 1
scoringData$aid = 0
scoringData$aid[which(grepl("First aid kit", scoringData$amenities)==T)] = 1
scoringData$hanger = 0
scoringData$hanger[which(grepl("Hangers", scoringData$amenities)==T)] = 1
scoringData$co = 0
scoringData$co[which(grepl("Carbon monoxide detector", scoringData$amenities)==T)] = 1

scoringData.sub = scoringData[,c('cleaning_fee','beds','host_response_time',
                                 'host_is_superhost','neighbourhood_Bronx','neighbourhood_Brooklyn',
                                 'neighbourhood_Manhattan','neighbourhood_Queens','neighbourhood_Staten',
                                 'latitude','longitude','room_apt','room_private','room_shared',
                                 'accommodates','bathrooms','bedrooms','guests_included','transit','access',
                                 'minimum_nights','calculated_host_listings_count','property_type',
                                 'review_scores_rating','reviews_per_month','is_business_travel_ready',
                                 'review_scores_cleanliness','review_scores_checkin','is_location_exact',
                                 'review_scores_location','review_scores_value','cancellation_policy',
                                 'availability_30','availability_60','availability_90','availability_365','bed_airbed',
                                 'bed_couch','bed_futon','bed_sofa','bed_real','instant_bookable',
                                 'wifi','heat','air','kitcken','shampoo','essential','tv','gym',
                                 'washer','dryer','fridge','self_check','hair','smart','aid','hanger','co')]
write.csv(scoringData.sub, 'cleaned_score_data.csv',row.names = F)

# construct submision from predictions
boost=gbm(price~.,data=data.sub,distribution="gaussian",interaction.depth = 10,
          n.minobsinnode=10,keep.data = TRUE,n.trees = 5000,shrinkage = 0.005,n.cores=8)

train.pred = predict(boost,data.sub,n.trees = 5000)
sqrt(mean((train.pred-data.sub$price)^2))
test.pred = predict(boost,scoringData.sub,n.trees = 5000)

submissionFile = data.frame(id = scoringData$id, price = test.pred) 
write.csv(submissionFile, 'airbnb5.csv',row.names = F)

