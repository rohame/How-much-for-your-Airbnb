data1 = read.csv('result_new features3.csv',stringsAsFactors = F)
data2 = read.csv('result_new features4.csv',stringsAsFactors = F)
data3 = read.csv('result_new features5.csv',stringsAsFactors = F)
data4 = read.csv('result_new features6.csv',stringsAsFactors = F)
data5 = read.csv('result_new features7.csv',stringsAsFactors = F)
data6 = read.csv('result2.csv',stringsAsFactors = F)
data7 = read.csv('result3.csv',stringsAsFactors = F)
data8 = read.csv('result_new features8.csv',stringsAsFactors = F)

  
score=c(53.64084,52.75962,52.99071,52.83629,52.84098,53.44047,52.76051,51.84845)

pred = (data1$price/score[1]+data2$price/score[2]+data3$price/score[3]+data4$price/score[4]+
  data5$price/score[5]+data6$price/score[6]+data7$price/score[7]+data8$price/score[8])/(1/score[1]+1/score[2]+
                                                                                          1/score[3]+1/score[4]+
                                                                                          1/score[5]+1/score[6]+
                                                                                          1/score[7]+1/score[8])
pred = (data1$price+data2$price+data3$price+data4$price+data5$price+data6$price+data7$price+data8$price)/8

result = cbind(id = data1$id,price=pred)
write.csv(result,'average2.csv',row.names = F)
