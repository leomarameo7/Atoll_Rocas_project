# This function computes percent variation with respect of the Status quo scenrio (baseline)
# of the mean, the 95% percentil trial and 5 % percentil trial. It works with the name of data frame, 
#and the names of columns that you want to compute percent variation 

percent_dt_general = function(col_name1, col_name2,base_ind){
   new_dt = 0
   for (i in c(1:3)) new_dt = rbind(new_dt, 
                                    ((col_name1[base_ind+i] - col_name2[base_ind])/col_name2[base_ind])*100)
   new_dt
}

ids_baseline = c(1)
x = 1
for (i in c(1:15)) {x = x+4
ids_baseline[i+1] = x
}
