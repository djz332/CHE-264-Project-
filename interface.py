import numpy as np
import subprocess
import sys
import os
import time

subprocess.call(["f2py", "-c", "-m", "spread_algorithm", "algorithm_subs.f90"])

time.sleep(3)

import spread_algorithm
 
population_size = 1170000
population_density = 300.0 #people/km^2
lattice_length = int((np.sqrt(float(population_size)/float(population_density)))*100) #lattice sites are 10 meters apart

household_size = 3
num_households = int(float(population_size)/float(household_size))

patient_0 = 3000 #initial infected
start_type = 2 # start in the begining or the middle of pandemic, affects how infected are initalized
quarantine_day = 5 # 50 percentile symptom onset, incubation period in days
vaccination_rate = 0
transferability = 0.2 #infection rate

fatality_rate = 0.002
percent_critical = 0.05
avg_recovery = 14 # days from symptoms onset
recovery_span = 5 # assuming normal distribution of mild case recovery
critical_high = 42
critical_low = 21
mobility = 100 #lattice points 
curve_ratio = 0.06 # new cases per old cases each day/important fitting parameter  
daily_moves = 2 #total moves per day for chances of new interactions
static = 0.0 #not move
num_stats = 3

coord = np.ones((lattice_length,lattice_length))
distribution = np.empty([population_size,2]) # home base assignment/movement tracker
household = np.empty([num_households,2]) #home base location
population_stats = np.empty([population_size,num_stats])

###########################################################################

for i in range(population_size):
  
	R = np.random.randint(1000)
	if R <= vaccination_rate*1000:
	
		vacc_status = 1 #vaccinated

	else: 
		
		vacc_status = 0
	
	R_critical = np.random.randint(100)
	if R_critical < int(100*percent_critical):
	
		recovery = int(np.random.randint(low = int(critical_low), high = int(critical_high)))

	else:

		span = np.random.randint(low = -recovery_span, high = recovery_span)
		recovery = int(avg_recovery + span)
        	
	population_stats[i,:] = [0, recovery, vacc_status]	

taken = np.zeros([lattice_length, lattice_length]) #taken locations
for i in range(num_households):

        Rx = np.random.randint(lattice_length)
        Ry = np.random.randint(lattice_length)
        while taken[Rx][Ry] == 1:
                Rx = np.random.randint(lattice_length)
                Ry = np.random.randint(lattice_length)

        household[i,0] = int(Rx+1)
        household[i,1] = int(Ry+1)
        taken[Rx][Ry] = 1


for i in range(population_size):

        R = np.random.randint(num_households)
        distribution[i][0] = household[R][0]
        distribution[i][1] = household[R][1]


for i in range(patient_0):
        
	R = np.random.randint(population_size)
        while (population_stats[R,0] >= 1):
		R = np.random.randint(int(population_size))
	
	if start_type == 1: 

		start_status = np.random.randint(low = 2, high = int(quarantine_day))

	if start_type == 2:
	
		start_status = np.random.randint(low = 2, high = int(population_stats[i,1] + quarantine_day - 1))

 	population_stats[R,0] = int(start_status + 1)

##############################################################

spread_algorithm.algorithm.population_size = population_size
spread_algorithm.algorithm.lattice_length = lattice_length
spread_algorithm.algorithm.patient_0 = patient_0
spread_algorithm.algorithm.quarantine_day = quarantine_day
spread_algorithm.algorithm.transferability = transferability
spread_algorithm.algorithm.fatality_rate = fatality_rate
spread_algorithm.algorithm.mobility = mobility
#spread_algorithm.algorithm.curve_ratio = curve_ratio
spread_algorithm.algorithm.daily_moves = daily_moves
spread_algorithm.algorithm.static =static
spread_algorithm.algorithm.home = distribution
spread_algorithm.algorithm.tracer_distribution = distribution
spread_algorithm.algorithm.coord = coord
spread_algorithm.algorithm.reset_coord = coord
spread_algorithm.algorithm.population_statistics = population_stats

spread_algorithm.algorithm.open_file()

#sys.exit()
        	
#spread_algorithm.algorithm.dynamics(population_size, lattice_length, patient_0, quarantine_day, transferability, fatality_rate, mobility, daily_moves, static)

spread_algorithm.algorithm.dynamics()
print "successful dynamics"

spread_algorithm.algorithm.close_file()

#################################################################
	
sys.exit()
