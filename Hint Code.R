#This is a quick hint/sample that you can reference

#Define the function
get_saturation = function(deviation_p50 = 0,
                          po2 = 26, 
                          temperature = 37,
                          ph = 7.4,
                          pco2 = 40){
  constant_list = c(-8532.229, 2121.401, -67.07399, 935960.9, -31346.26, 2396.167, -67.10441)
  pco2_correction_factor = 0.43429 * log(40.0/pco2)
  virtual_po2 = po2 * 10.0^(0.024 * (37.0 - temperature) + 0.4 * (ph - 7.4) + 0.06 * pco2_correction_factor)
  virtual_po2 = 26.8 * virtual_po2 / (26.8 + deviation_p50)
  if(virtual_po2 <= 10){
    saturation = 0.003683 * virtual_po2 + 0.000584 * virtual_po2^2
  }else{
    saturation = (virtual_po2*(virtual_po2*(virtual_po2*(virtual_po2+ constant_list[3])+constant_list[2])+constant_list[1]))/
      (virtual_po2*(virtual_po2*(virtual_po2*(virtual_po2+constant_list[7])+constant_list[6])+constant_list[5])+constant_list[4])
  }
  saturation = saturation * 100
  return(saturation)
}

#Import the data
po2 = c(58, 50, 48, 54, 24)
sat = c(90.3, 85.2, 83.8, 84.4, 41.5)

#Set a range of deviation of P50
#What to be noticed here is that the deviation of P50 is the difference of the real P50 value and 26.8
lower_bond = -20
upper_bond = 20

#Generate a series of trial values of deviation p50

trial_p50 = seq(lower_bond, upper_bond, 0.001) #Arithmetic series with difference 0.01

#Try plug in the values one by onee
SSQ = vector() #Assign a vector as the container of all the 4000 SSQ values
m = 1 #Assign a index sorting number
for(dp50 in trial_p50){
  
  #Get the difference between the predicted SAT and the measured SAT for each PO2
  difference = vector()
  n = 1
  for(trial_po2 in po2){
    difference[n] = sat[n] - get_saturation(deviation_p50 = dp50,
                                            po2 = trial_po2)
    n = n + 1 #This is a iteration number used for vector index sorting
  }
  
  #Now you have a vector difference that has all the difference between measured SAT and predicted SAT
  #Note that the values can either be positive or negative
  #You can then find the SSQ by using
  SSQ[m] = sum(difference^2)
  m = m + 1
  
}

#Get out of the loop, the vector SSQ is filled with 4000 values
#You can check it by using:
print(SSQ)

#What you can also do is to plot it with the associating trial P50
#plot(trial_p50, SSQ, type = "l")
#Note that trial P50 is the difference between the trial values and 26.8

#We can also find the mininum values using
trial_p50[which.min(SSQ)]

#And then the real P50 best fit that we are looking at is
real_p50 = 26.8 + trial_p50[which.min(SSQ)]

#We can then plot the P50 values Vs. SSQ
plot(x = 26.8 + trial_p50, y = SSQ, type = "l", xlab = "Trial_P50")
#Add a vertical line to it indicating the best P50
abline(v = real_p50)
#Add legend to the plot
legend(x = 30, y = 3000, paste0("The best-fit P50 is: ", real_p50, " mmHg"))




