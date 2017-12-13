##ST2006 Course Assignement -- Dump Truck R Code
##Author - Devin Connolly (15315916)

##First need to define functions for random time generations we will need - Loading Time, Weighing Time, Travel Time

#Function to generate Travel time, passed a random number [0,1]
TravelTime <- function(rand) {
  if(rand < 0.4) {
    time = 40
  }
  else if (rand < 0.7) {
    time = 60
  }
  else if (rand < 0.9) {
    time = 80
  }
  else {
    time = 100
  }
  return(time)
}

#Function to generate Weighing time, passed a randon number [0,1]
WeighingTime <- function(rand) {
  if (rand < 0.7) {
    time = 12
  }
  else {
    time = 16
  }
  return(time)
}

#Function to generate Loading time, passed a random number [0,1]
LoadingTime <- function(rand) {
  if (rand < 0.3) {
    time = 5
  }
  else if (rand < 0.8) {
    time = 10
  }
  else {
    time = 15
  }
  return(time)
}

##Initialise some necessary variables and constants
time = 0
endTime  = 1000
numberTrucks = 6

##Total cumulative statistic trackers
totalLoadingTime = 0
totalWeighingTime = 0
totalTravelTime = 0
totalDeliveries = 0

##Trackers for max length of the queues
maxLoadingQ = 0 
maxWeighingQ = 0

##Trackers for utilisation of the weigher and loader
weighingEmpty = 0
loadingEmpty = 0

##Trackers on where the trucks are
trucksTravelling = 0 #Can be determined as 1 - (all other)
trucksLoading = 0 #L(t)
trucksWeighing = 0 #W(t)
loadingQ = 0 #LQ(t)
weighingQ = 0 #WQ(t)

##Initial Conditions
loadingQ = 3
trucksLoading = 2
trucksWeighing = 1

#Initialising the Future Event List, with the 4 possible events that can occur
FEList = data.frame(times = c(TravelTime(runif(1,0,1)),LoadingTime(runif(1,0,1)),WeighingTime(runif(1,0,1)), endTime ), event = c('ArrivalLoaderQ','EndLoading','EndWeighing', 'End'))
FEList = FEList[order(FEList[,1]),]

#Removes the initial events on the list, as we want to generate events for the two trucks in the loader
#and the one getting weighed (the initial starting conditions, t=0, as per the slides)
FEList <- FEList[-c(1, 2, 3), ]


#Need to start the simulation with a certain event, so generate events for the two EL and one EW
NewEvent = data.frame(times=c(time + LoadingTime(runif(1,0,1))),event = c('EndLoading'))
FEList = rbind(FEList, NewEvent) ##Add the new events to the Future Event List
NewEvent = data.frame(times=c(time + LoadingTime(runif(1,0,1))),event = c('EndLoading'))
FEList = rbind(FEList, NewEvent)
NewEvent = data.frame(times=c(time + WeighingTime(runif(1,0,1))),event = c('EndWeighing'))
FEList = rbind(FEList, NewEvent)

FEList = FEList[order(FEList[,1]),] ##Order the event list so those happening first occur first
event = FEList[1,2] ##set the event to be the next event occuring (position 1 in list)

bothLoadersUsed = 0
while (event != 'End') {
  FEList = FEList[order(FEList[,1]),]
  event = FEList[1,2]
  oldTime = time
  time = FEList[1,1] ##set the time to be the time of the next event
  if (trucksWeighing == 0) { ##an extra bit of code to attempt to track empty loaders and scale
    weighingEmpty = weighingEmpty + (time-oldTime)
  }
  if (trucksLoading == 0) {
    loadingEmpty = loadingEmpty + (time-oldTime)
  }
  
  FEList <- FEList[-c(1), ] ##remove the event being accounted for from the list
  print(c(noquote(c('Main Time: ', time))))
  
  if (event == 'ArrivalLoaderQ') {
    totalDeliveries = totalDeliveries + 1 ##travelling has complete, so increment total deliveries
    trucksTravelling = trucksTravelling - 1 ##one less truck is now travelling
    if (trucksLoading < 2) {
      if (loadingQ == 0) { ##space free on loader and no trucks in queue
       trucksLoading = trucksLoading + 1
       NewEvent = data.frame(times=c(time + LoadingTime(runif(1,0,1))),event = c('EndLoading'))
       NewEventTime = NewEvent[1,1] - time ##create a new event for end loading
       totalLoadingTime = totalLoadingTime + NewEventTime ##add the loading time of the truck just loaded to total loading time
       FEList = rbind(FEList, NewEvent) ##add this event to the list
      }
      else { ##space on loaders but the queue isnt empty
        trucksLoading = trucksLoading + 1 ##add another truck to loader
        NewEvent = data.frame(times=c(time + LoadingTime(runif(1,0,1))),event = c('EndLoading'))
        NewEventTime = NewEvent[1,1] - time ##new event for end loading
        totalLoadingTime = totalLoadingTime + NewEventTime ##increase total loading time by time of this loading
        FEList = rbind(FEList, NewEvent) ##add event to list
      }
    }
    else { ##both loaders are full
      loadingQ = loadingQ + 1 ##increase queue
      if (loadingQ > maxLoadingQ) {
        maxLoadingQ = loadingQ ##save a new max queue length if applicable (extra)
      }
    }
  }
  
  else if (event == 'EndLoading') {
    
    if (trucksWeighing > 0) {         ##scales are occupied
      weighingQ = weighingQ + 1       ##incrememnt weighing queue
      if (weighingQ > maxWeighingQ) { ##again checking for max Weighing queue (extra)
        maxWeighingQ = weighingQ
      }
    }
    else if (weighingQ == 0) { ##scales are free and there is no queue
      trucksWeighing = 1
      NewEvent = data.frame(times=c(time + WeighingTime(runif(1,0,1))),event = c('EndWeighing'))
      NewEventTime = NewEvent[1,1] - time ##new end weighing event created and added to list
      totalWeighingTime = totalWeighingTime + NewEventTime
      FEList = rbind(FEList, NewEvent)
    }
    else { ##scales are free and there is a queue, queue length stays the same
      NewEvent = data.frame(times=c(time + WeighingTime(runif(1,0,1))),event = c('EndWeighing'))
      NewEventTime = NewEvent[1,1] - time ##new end weighing event created and added to list
      totalWeighingTime = totalWeighingTime + NewEventTime
      FEList = rbind(FEList, NewEvent)
    }
    
    if (loadingQ == 0) {        ##check if loading queue is empty
      if (trucksLoading == 2) { ##account for truck leaving the loader
        trucksLoading = 1
      }
      else {
        trucksLoading = 0
      }
      
    }
    else { ##replace the truck that left with one from loading queue
      loadingQ = loadingQ - 1
      NewEvent = data.frame(times=c(time + LoadingTime(runif(1,0,1))),event = c('EndLoading'))
      NewEventTime = NewEvent[1,1] - time
      totalLoadingTime = totalLoadingTime + NewEventTime
      FEList = rbind(FEList, NewEvent)
    }
  }
  
  else if (event == 'EndWeighing') {
    NewEvent = data.frame(times=c(time + TravelTime(runif(1,0,1))),event = c('ArrivalLoaderQ'))
    NewEventTime = NewEvent[1,1] - time ##generate event for arrival at loader queue (travel)
    totalTravelTime = totalTravelTime + NewEventTime ##add to total travel time
    trucksTravelling = trucksTravelling + 1 ##incremement number of trucks travelling
    FEList = rbind(FEList, NewEvent)
    
    if (weighingQ == 0) { ##account for truck leaving weighing queue
      trucksWeighing = 0
    }
    else { ##replace the truck with one from the queue
      weighingQ = weighingQ - 1
      NewEvent = data.frame(times=c(time + WeighingTime(runif(1,0,1))),event = c('EndWeighing'))
      NewEventTime = NewEvent[1,1] - time ##new event for end weighing for replacement
      totalWeighingTime = totalWeighingTime + NewEventTime
      FEList = rbind(FEList, NewEvent)
    }
  }
  
  else if (event == 'End') { ##account for event times that wont be completed (time after end time of 1000)
    length = nrow(FEList)    ##as this would skew the total travel/weighing/loading times
    for (i in 1:length) {    ##cycle through the future events yet to complete
     if (FEList[i,2] == 'ArrivalLoaderQ') {
        totalTravelTime = totalTravelTime - (FEList[i,1] - time)
     }
     else if(FEList[i,2] == 'EndLoading') {
        totalLoadingTime =  totalLoadingTime - (FEList[i,1] - time)
     }
     else if(FEList[i,2] == 'EndWeighing') {
        totalWeighingTime =  totalWeighingTime - (FEList[i,1] - time)
     }
    }
  }
}
print(c(noquote(c('Main Time: ', time))))
print(c(noquote(c('Total Deliveries: ', totalDeliveries))))
print(c(noquote(c('Total Loading Time: ', totalLoadingTime))))
print(c(noquote(c('Total Weighing Time: ', totalWeighingTime))))
print(c(noquote(c('Total Travel Time: ', totalTravelTime))))
print(c(noquote(c('Trucks Loading: ', trucksLoading))))
print(c(noquote(c('Trucks Weighing: ', trucksWeighing))))
print(c(noquote(c('Trucks Travelling: ', trucksTravelling))))
print(c(noquote(c('Trucks in Weighing or Loading Queue: ', loadingQ + weighingQ))))

