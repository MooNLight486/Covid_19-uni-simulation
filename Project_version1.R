all_list = list()
people_in_kitchen = 10 #10
rate = 1 #0.7
penalty_rate = 1 #0.35
quarantine_rate = 0 #0.2
total_days = 30 #30
infected_day_1 = 15 #15
vaccinated_day_1 = 20 #35
vaccination_rate_penalty = 0.06 #0.06
rate_of_death = 0.001 #0.009 according to UK stats // get rid of
people_total = readline(prompt = "Enter number of people in sim ")
people_total = strtoi(people_total)
daily_list = list()

create_list = function(){
  j = 1
  k = 0
  z = 1
  people_infected = sample(people_total, infected_day_1, replace = FALSE)
  for (i in 1:people_total){
    if (k == people_in_kitchen){
      j = j + 1
      k = 0
    }
    k = k + 1
    check = FALSE
    if (z <= infected_day_1){
      for (a in 1:infected_day_1){
        if (people_infected[a] == i){
          check = TRUE
          z = z + 1
          person = list(status = "I", kitchen = j, vaccine = 0)
        }
      }
    }
    else{
      person = list(status = "S", kitchen = j, vaccine = 0)
    }
    if (!check){
      person = list(status = "S", kitchen = j, vaccine = 0)
    }
    daily_list = append(daily_list, list(person))
  }
  return(daily_list)
}

susceptible_to_infected = function(day_list, number){
  total_prob = 0
  count = 0
  kitchen_x = day_list[[number]]$kitchen
  for (i in 1:people_total){
    if (i != number){
      if (day_list[[i]]$status == "I"){
        count = count + 1
        if (day_list[[i]]$kitchen == kitchen_x){
          total_prob = total_prob + rate
        }
        else total_prob = total_prob + penalty_rate
      }
    }
  }
  if (count == 0){
    return (0)
  }
  else{
    if (day_list[[number]]$vaccine == 1){
      total_prob = total_prob * vaccination_rate_penalty
    }
    total_prob = total_prob / count
    result = rbinom(1,1,total_prob)
    return(result)
  }
}

new_day = function(previous_day, day_number){
  newday = previous_day
  for (i in 1:people_total){
    if (previous_day[[i]]$status == "S"){
      result = susceptible_to_infected(previous_day, i)
      if (result == 1){
        newday[[i]]$status = "I"
      }
    }
    else if (previous_day[[i]]$status == "I"){
        result1 = infected_to_quarantine(day_number, i)
        if (result1 == 1){
          newday[[i]]$status = "Q"
        }
        else{
          result = death_prob(day_number,i)
          if (result == 1){
            newday[[i]]$status = "D"
          }
          else {
            result = infected_to_recovered(day_number, i)
            if (result == 1){
              newday[[i]]$status = "R"
            }
          }
        }
    }
    else if (previous_day[[i]]$status == "Q"){
      result = death_prob(day_number,i)
      if (result == 1){
        newday[[i]]$status = "D"
      }
      else {
        result = infected_to_recovered(day_number, i)
        if (result == 1){
          newday[[i]]$status = "R"
        }
      }
    }
  }
  return (newday)
}

infected_to_quarantine = function(day_number, i){
  if (day_number >= 3){
    result = 0
    if (all_list[[day_number-2]][[i]]$status == "S"){
      result = rbinom(1,1,quarantine_rate)
    }
    return (result)
  }
  return (0)
}

infected_to_recovered = function(day_number, i){
  result = 0
  if (day_number >= 4){
    j = 1
    days = 1
    given_day = all_list[[j]]
    while (given_day[[i]]$status == "S"){
      j = j + 1
      given_day = all_list[[j]]
    }
    while ((given_day[[i]]$status == "I" || given_day[[i]]$status == "Q")  && j < day_number){
      days = days + 1
      given_day = all_list[[j]]
      j = j + 1
    }
    prob = recover_prob(days)
    result = rbinom(1,1,prob)
    return (result)
  }
  return (0)
}

death_prob = function(day_number, i){
  result = 0
  if (day_number>=4){
    j = 1
    days = 1
    given_day = all_list[[j]]
    while (given_day[[i]]$status == "S"){
      j = j + 1
      given_day = all_list[[j]]
    }
    while ((given_day[[i]]$status == "I" || given_day[[i]]$status == "Q")  && j < day_number){
      days = days + 1
      given_day = all_list[[j]]
      j = j + 1
    }
    if (days >= 3){
      result = rbinom(1,1,rate_of_death)
    }
    return (result)
  }
  return (0)
}

recover_prob = function(days){ #p(2) = 0, p(14) = 1
  if (days <= 2){
    return (0)
  }
  else if (days >= 14){
    return (1)
  }
  else {
    result = days / 12 - 1/6
    return (result)
  }
  return (-1)
}

vaccines_day_1 = function(daily_list){
  startlist = daily_list
  people_vaccinated = sample(people_total, vaccinated_day_1, replace = FALSE)
  for (i in 1:vaccinated_day_1){
    x = people_vaccinated[i]
    startlist[[x]]$vaccine = 1
  }
  return (startlist)
}

daily_list = create_list()
if (vaccinated_day_1 != 0){
  daily_list = vaccines_day_1(daily_list)
}
all_list = append(all_list, list(daily_list))


for (i in 1:(total_days-1)){
  newday = new_day(all_list[[i]], i+1)
  all_list = append(all_list, list(newday))
}

# count people by their states any day
# counts = 0
# counts2 = 0
# counts3 = 0
# counts4 = 0
# counts5 = 0
# t = 1
# for (i in 1:people_total){
#   if (all_list[[t]][[i]]$status == "S"){
#     counts = counts + 1
#   }
# }
# for (i in 1:people_total){
#   if (all_list[[t]][[i]]$status == "I"){
#     counts2 = counts2 + 1
#   }
# }
# for (i in 1:people_total){
#   if (all_list[[t]][[i]]$status == "Q"){
#     counts3 = counts3 + 1
#   }
# }
# for (i in 1:people_total){
#   if (all_list[[t]][[i]]$status == "R"){
#     counts4 = counts4 + 1
#   }
# }
# for (i in 1:people_total){
#   if (all_list[[t]][[i]]$status == "D"){
#     counts5 = counts5 + 1
#   }
# }
# print(counts)
# print(counts2)
# print(counts3)
# print(counts4)
# print(counts5)

susceptible_list_no_vaccine = list()
for (t in 1:total_days){
  count = 0
  for (i in 1:people_total){
    if (all_list[[t]][[i]]$status == "S" && all_list[[t]][[i]]$vaccine == 0){
      count = count + 1
    }
  }
  susceptible_list_no_vaccine = append(susceptible_list_no_vaccine, count)
}

susceptible_list_vaccine = list()
for (t in 1:total_days){
  count = 0
  for (i in 1:people_total){
    if (all_list[[t]][[i]]$status == "S" && all_list[[t]][[i]]$vaccine == 1){
      count = count + 1
    }
  }
  susceptible_list_vaccine = append(susceptible_list_vaccine, count)
}

infected_list = list()
for (t in 1:total_days){
  count = 0
  for (i in 1:people_total){
    if (all_list[[t]][[i]]$status == "I"){
      count = count + 1
    }
  }
  infected_list = append(infected_list, count)
}

quarantined_list = list()
for (t in 1:total_days){
  count = 0
  for (i in 1:people_total){
    if (all_list[[t]][[i]]$status == "Q"){
      count = count + 1
    }
  }
  quarantined_list = append(quarantined_list, count)
}

recovered_list = list()
for (t in 1:total_days){
  count = 0
  for (i in 1:people_total){
    if (all_list[[t]][[i]]$status == "R"){
      count = count + 1
    }
  }
  recovered_list = append(recovered_list, count)
}

deaths_list = list()
for (t in 1:total_days){
  count = 0
  for (i in 1:people_total){
    if (all_list[[t]][[i]]$status == "D"){
      count = count + 1
    }
  }
  deaths_list = append(deaths_list, count)
}


plot(unlist(susceptible_list_no_vaccine), col = "green", type = "o", xlab = "Day", ylab = "Number of people", ylim = c(0, people_total))
lines(unlist(susceptible_list_vaccine), col = "purple", type = "o", xlab = '', ylab = '', xaxt ='n', yaxt = 'n')
lines(unlist(infected_list), col = "red", type = "o", xlab = '', ylab = '', xaxt ='n', yaxt = 'n')
lines(unlist(deaths_list), col = "black", type = "o", xlab = '', ylab = '', xaxt ='n', yaxt = 'n')
lines(unlist(recovered_list), col = "blue", type = "o", xlab = '', ylab = '', xaxt ='n', yaxt = 'n')
lines(unlist(quarantined_list), col = "orange", type = "o", xlab = '', ylab = '', xaxt ='n', yaxt = 'n')

t = total_days
print(susceptible_list_no_vaccine[t])
print(susceptible_list_vaccine[t]) #ctrl shift c
print(infected_list[t])
print(quarantined_list[t])
print(recovered_list[t])
print(deaths_list[t])

# see how many people and who is vaccinated day 1
# counts5 = 0
# for (i in 1:people_total){
#   if (all_list[[1]][[i]]$vaccine == 1){
#     print(i)
#     counts5 = counts5 + 1
#   }
# }
# print (counts5)

# see who is infected day 1
# for (i in 1:people_total){
#   if (all_list[[1]][[i]]$status == "I"){
#     print(i)
#   }
# }
