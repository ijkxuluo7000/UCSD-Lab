#The result is accurate the the code is concise. Good job!

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

difference = vector()

for(i in 1:5){
  difference[i] = get_saturation(po2 = po2[i])
}

difference
#[1] 90.16837 85.14841 83.42614 87.99066 43.56537

SSQ = sum(difference^2)

ssqList = vector()

# p50 = c(26.00,
#         28.20,
#         31.70,
#         28.00,
#         29.40,
#         27.40,
#         26.50,
#         29.20,
#         28.20,
#         28.40,
#         27.90,
#         28.70,
#         27.50,
#         29.20,
#         27.70,
#         26.90,
#         28.90,
#         29.90,
#         27.50)

#Another way to quickly iterate through trial P50 values is
from = -5
to = 10
step = 0.01
p50 = seq(from, to, step)

p50 = p50[order(p50)]
m = 1

#The reason why at first you do not observe a parabola
#is because the p50 input of function get_saturation()
#is actually the deviation of P50 from 26.8. Thus by 
#Your original range of input, you are essentially trying
#to find the real P50 in between a wrong range.

#Simple solution would be to increase the range.

for (d_p50 in p50){
  pdiff = vector()
  n = 1
  for (d_po2 in po2){
    pdiff[n] = sat[n] - get_saturation(deviation_p50 = d_p50, po2 = d_po2)
    n = n + 1
  }
  ssqList[m] = sum(pdiff^2)
  m = m+1
}

#Nested loop well done!!!!!!!

ssqList[1 : m-1] # Change the round brackets to square brackets
#Try to think why here it should be m-1 instead of m to avoid any NA output
#because m=m+1 is after I set ssq to ssqList[m]

#plot(seq(from, to, step), ssqList, type = "l")

real_p50 = 26.8 + p50[which.min(ssqList)]#real p50 value
print(real_p50)  ######## Good JOb!!!!  #######
plot(x = 26.8+p50,y = ssqList,xlab = "p50", type = "l", main = paste("The real P50 is", real_p50))
abline(v = 27.46, col = "blue")

#[1] 31980.8