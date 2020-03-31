import numpy as np
import subprocess
import sys
import os

subprocess.call(["f2py", "-c", "-m", "spread_algorithm", "algorithm_subs.f90"])

import spread_algorithm
 
population_size = 3000000
population_density = 1200 #people/km^2
lattice_length = int((np.sqrt(population_size/population_density))*100) #lattice sites are 10 meters apart

patient_0 = 1 #initial infected
quarantine_day = 7
vaccination_rate = 0
transferability = 0.3 #infection rate

percent_youth = 0.25
percent_adult = 0.5
percent_senior = 1 - (percent_youth + percent_adult)

youth_death_chance = 0.01
adult_death_chance = 0.05
senior_death_chance = 0.1

youth_recovery = 9 #days
adult_recovery = 14
senior_recovery = 17

youth_mobility = 200 #lattice points 
adult_mobility = 500
senior_mobility = 100

youth_static = 0.5 #not move
adult_static = 0.25
senior_static  = 0.5

coord = np.ones((lattice_length,lattice_length))
distribution = np.empty([population_size,2])
population_stats = np.empty([population_size,6])


youth_count = 0
adult_count = 0
for i in range(population_size):
  
	R = np.random.randint(1000)
	if R <= vaccination_rate*1000:
	
		vaccination_status = 1 #vaccinated

	else: 
		
		vaccination_status = 0

	if youth_count < int(percent_youth*population_size):
        	
		population_stats[i,:] = [0, vaccination_status, youth_death_chance, youth_recovery, youth_mobility, youth_static]
		youth_count += 1

        elif adult_count < int(percent_adult*population_size):

		population_stats[i,:] = [0, vaccination_status, adult_death_chance, adult_recovery, adult_mobility, adult_static]
 		adult_count += 1
	
	else: 
	
		population_stats[i,:] = [0, vaccination_status, senior_death_chance, senior_recovery, senior_mobility, senior_static]


for i in range(population_size):

	Rx = np.random.randint(lattice_length)
        Ry = np.random.randint(lattice_length)
	distribution[i][0] = (Rx + 1)
        distribution[i][1] = (Ry + 1)

#print distribution[:,0]
#print distribution[:,1]
#sys.exit()

for i in range(patient_0):
        
	R = np.random.randint(population_size)
        while (population_stats[R,0] == 1):
		R = np.random.randint(population_size)

 	population_stats[R,0] = 1


spread_algorithm.algorithm.population_size = population_size
spread_algorithm.algorithm.lattice_length = lattice_length
spread_algorithm.algorithm.quarantine_day = quarantine_day
spread_algorithm.algorithm.patient_0 = patient_0
spread_algorithm.algorithm.transferability = transferability
spread_algorithm.algorithm.home = distribution
spread_algorithm.algorithm.tracer_distribution = distribution
spread_algorithm.algorithm.coord = coord
spread_algorithm.algorithm.reset_coord = coord
spread_algorithm.algorithm.population_statistics = population_stats

#print spread_algorithm.algorithm.population_size
#print spread_algorithm.algorithm.lattice_length
#print spread_algorithm.algorithm.quarantine_day
#print spread_algorithm.algorithm.transferability 
#print spread_algorithm.algorithm.home 
#print spread_algorithm.algorithm.tracer_distribution
#print spread_algorithm.algorithm.coord 
#print spread_algorithm.algorithm.reset_coord 
#print spread_algorithm.algorithm.population_statistics


spread_algorithm.algorithm.open_file()
print "Record file opened"

#sys.exit()
spread_algorithm.algorithm.dynamics()
print "successful dynamics"

spread_algorithm.algorithm.close_file()
print "Run succesful"

sys.exit()
