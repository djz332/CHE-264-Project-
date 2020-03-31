import numpy as np
import subprocess
import sys
import os

subprocess.call(["f2py", "-c", "-m", "spread_algorithm_GE", "algorithm_subs_GE.f90"])

import spread_algorithm_GE
 
population_size = 1000000
population_density = 1000 #people/km^2
lattice_length = int((np.sqrt(population_size/population_density))*100) #lattice sites are 10 meters apart

household_size = 2.5
num_households = int(population_size/household_size)
num_schools = 1000
num_businesses = 100000
percent_enrolled = 0.9
percent_employed = 0.9

patient_0 = 1 #initial infected
quarantine_day = 7
vaccination_rate = 0
transferability = 0.08 #infection rate

percent_youth = 0.25
percent_adult = 0.5
percent_senior = 1 - (percent_youth + percent_adult)

youth_death_chance = 0.01
adult_death_chance = 0.05
senior_death_chance = 0.1

youth_recovery = 9 #days
adult_recovery = 14
senior_recovery = 17

youth_mobility = 25 #lattice points 
adult_mobility = 50
senior_mobility = 10

youth_static = 0.5 #not move
adult_static = 0.25
senior_static  = 0.5

coord = np.ones((lattice_length,lattice_length))
population_stats = np.empty([population_size,6])
demographics = np.empty([population_size,1]) #age group and household
household = np.empty([num_households,2]) #house hold location
business = np.empty([num_businesses,2]) #business location
school =  np.empty([num_schools,2]) #school location
distribution = np.empty([population_size,2]) #location
destination = np.empty([population_size,2]) #school or business location


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
                demographics[i] = 1
		youth_count += 1

        elif adult_count < int(percent_adult*population_size):

		population_stats[i,:] = [0, vaccination_status, adult_death_chance, adult_recovery, adult_mobility, adult_static]
                demographics[i] = 2
 		adult_count += 1
	
	else: 
	
		population_stats[i,:] = [0, vaccination_status, senior_death_chance, senior_recovery, senior_mobility, senior_static]
                demographics[i,0] = 3


taken = np.zeros([lattice_length, lattice_length]) #taken locations
for i in range(num_households):

	Rx = np.random.randint(lattice_length)
        Ry = np.random.randint(lattice_length)
        while taken[Rx][Ry] == 1:
        	Rx = np.random.randint(lattice_length)
	        Ry = np.random.randint(lattice_length)

        household[i,0] = (Rx+1)
        household[i,1] = (Ry+1)
        taken[Rx][Ry] = 1


for i in range(num_businesses):

	Rx = np.random.randint(lattice_length)
        Ry = np.random.randint(lattice_length)
        while taken[Rx][Ry] == 1:
                Rx = np.random.randint(lattice_length)
                Ry = np.random.randint(lattice_length)

        business[i,0] = (Rx+1)
        business[i,1] = (Ry+1)
        taken[Rx][Ry] = 1


for i in range(num_schools):

        Rx = np.random.randint(lattice_length)
        Ry = np.random.randint(lattice_length)
        while taken[Rx][Ry] == 1:
                Rx = np.random.randint(lattice_length)
                Ry = np.random.randint(lattice_length)

        school[i,0] = (Rx+1)
        school[i,1] = (Ry+1)
        taken[Rx][Ry] = 1
	

school_pop = np.zeros([num_schools])
for i in range(population_size):
	
	R = np.random.randint(num_households)
	distribution[i][0] = household[R][0]
        distribution[i][1] = household[R][1]

	if demographics[i] == 1:
			
		enrolled_chance = float(np.random.randint(1000))/float(1000)
		
		if enrolled_chance < percent_enrolled: 
                         
                        condensed_list = []
                        closest = 2*lattice_length
                        closest_school = []
                        for j in range(20):

				random_school = np.random.randint(num_schools)
				distance = np.sqrt((distribution[i][0]-school[random_school][0])**2+(distribution[i][1]-school[random_school][1])**2)
				if distance <= closest:
					
					closest_school.append(random_school)
					closest = distance
			
			closest_school.reverse()
			destination[i][0] = school[closest_school[0]][0]
			destination[i][1] = school[closest_school[0]][1]
                        school_pop[closest_school[0]] =  school_pop[closest_school[0]] + 1
		
		else:

			destination[i][0] = distribution[i][0]
			destination[i][1] = distribution[i][1]		
				 

	elif demographics[i] == 2: 

		employed_chance = float(np.random.randint(1000))/float(1000)
		
		if employed_chance < percent_employed:
	
			condensed_list = []
                        closest = 2*lattice_length
                        closest_business = []
                        for j in range(20):

                                random_business = np.random.randint(num_businesses)
                                distance = np.sqrt((distribution[i][0]-business[random_business][0])**2+(distribution[i][1]-business[random_business][1])**2)
                                if distance <= closest:

                                        closest_business.append(random_business)
                                        closest = distance

                        closest_business.reverse()
                        destination[i][0] = business[closest_business[0]][0]
                        destination[i][1] = business[closest_business[0]][1]


		else:
			
			destination[i][0] = distribution[i][0]
                        destination[i][1] = distribution[i][1]

	elif demographics[i] == 3:

		
		destination[i][0] = distribution[i][0]
                destination[i][1] = distribution[i][1]
	

for i in range(num_schools):

	print school_pop[i]

for i in range(patient_0):
        
	R = np.random.randint(population_size)
        while (population_stats[R,0] == 1):
		R = np.random.randint(population_size)

 	population_stats[R,0] = 1

spread_algorithm_GE.algorithm_ge.population_size = population_size
spread_algorithm_GE.algorithm_ge.demographics = demographics
spread_algorithm_GE.algorithm_ge.lattice_length = lattice_length
spread_algorithm_GE.algorithm_ge.quarantine_day = quarantine_day
spread_algorithm_GE.algorithm_ge.patient_0 = patient_0
spread_algorithm_GE.algorithm_ge.transferability = transferability
spread_algorithm_GE.algorithm_ge.home = distribution
spread_algorithm_GE.algorithm_ge.tracer_distribution = distribution
spread_algorithm_GE.algorithm_ge.destination = destination
spread_algorithm_GE.algorithm_ge.coord = coord
spread_algorithm_GE.algorithm_ge.reset_coord = coord
spread_algorithm_GE.algorithm_ge.population_statistics = population_stats

spread_algorithm_GE.algorithm_ge.open_file()
print "Record file opened"

#sys.exit()
spread_algorithm_GE.algorithm_ge.dynamics()
print "successful dynamics"

spread_algorithm_GE.algorithm_ge.close_file()
print "Run succesful"

sys.exit()
