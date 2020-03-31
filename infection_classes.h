/*  
@file: infection_classes.h
@author: Omar Aly
<omar.aly@utexas.edu>
@version 1.0
@section LICENSE
Copyright 2019, Omar Aly

Permission is hereby granted, free of charge, to any person obtaining a 
copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
IN THE SOFTWARE.
*/



#include <iostream>
#include <random>
#include <vector>
#include <string>
#include <memory>

using std::rand;
using std::vector;
using std::string;
using std::cout;
using std::to_string;

/*
    Person is a class that describes a person in a population 
    that is currently in the midst of a disease
*/

class person{
    private:
    /* 
    measure of person's infection status: >0 = sick with that many days left in illness
                                           0 = not sick, but vulnerable to infection
                                          -1 = not sick, immune due to past illness
                                          -2 = vaccinated, will never get sick
    */
        int disease_status;
    public:
    /* 
        This constructor initializes the object based on the person's 
        infection/disease status, which is set vulnerable by default
    */
        person(int disease_status=0):
            disease_status(disease_status){};

    // class methods

    /* 
       string_status returns a string describing the status of the person.
       it has the default bool argument print_status, which if set to true,
       will also print the status out to console. pretty good for troubleshooting
    */
        string status_string(bool print_status=false){
            string status;
            switch (disease_status){
                case -2:
                    status = "This person is vaccinated and cannot be infected.";
                    break;
                case -1:
                    status = "This person was infected but is now immune to further ones.";
                    break;
                case  0:
                    status = "This person was never infected and is vulnerable.";
                    break;
                default:
                    status = "This person is infected for this many more days: " + to_string(disease_status);
                    break;
            } //switch case
            if(print_status)
                cout << status << '\n';
            return status;
        }//method

        /* 
        The update method will update the infection status of the person
        if they are sick, dropping it by a day. If they have 1 day left of
        being sick, it will set their disease status to -1 to make them recovered
        and immune.
        */

        void update(){
            if (disease_status > 1)
                disease_status--;
            else if (disease_status == 1)
                disease_status -= 2;
        }//method

        /*
          The infect method takes the length of time 
          the person should be sick as the argument
          and makes the person sick for that long
        */ 

        void infect(int infection_length){
            disease_status = infection_length;
        }

        /* 
            the is_vulnerable method returns a bool
            describing whether or not someone is 
            able to be infected by the disease
        */

        bool is_vulnerable(){
            return (disease_status == 0);
        }//method

        /* 
            the is_infected method returns a bool
            describing if someone is already sick
            with the disease
        */
        bool is_infected(){
            return (disease_status > 0);
        }

        /*
            The get_status method returns the integer
            describing the infection/disease status of
            the person (disease_status)

        */

        int get_status(){
            return disease_status;
        }//method

};//class

/* 
The population class contains all people within the community being simulated. 
It also holds relevant parameters and methods on infection and vaccination rates.
*/ 

class population{
    private:
        vector<person> people;  //vector of the people in the population of the sim

        int population_count;   // number of people in the population, or people.size()
        
        int infection_length;   //how long each infected person will be sick for

        /*
            spread_type indicates whether the virus spreads via random spread,
            geographically spread,  or a hybrid half-half approach for 
            spread_type = 1,2, and 3, respectively
        */

        int spread_type; 

        /*
            spread_count is many people each person has a chance to infect daily.
            It must be an even number for spread type 2, and it must be
            divisible by 4 for spread type 3!
        */

        int spread_count; 

        float infection_chance;     //chance of an interaction causing an infection
        float percent_vaccinated;   //percent or fraction of population that's vaccinated (roughly)

        /*
            symbol _ key is the key of the symbols to print to describe the population stats:
            vaccinated = #, immune due to illness = -, vulnerable = ?, infected = +
        */

        string symbol_key = "#-?+"; 


    public:
    /* 
        This constructor initializes the object based on the various
        population parameters, i.e. the population count, the
        probability of infecting someone per contact instance,
        the fraction of the population that is vaccinated,
        how long each infection will last,
        the method by which the disease spreads,
        and the number of people that each infected person
        comes in contact with
    */
        population(int number_of_people = 50000, float infectivity = 0.5, float vaccinated_fraction = 0.0,
                    int infection_time = 6, int spreading = 1, int number_can_spread = 4){
            spread_type = spreading;
            infection_chance = infectivity;
            population_count = number_of_people;
            infection_length = infection_time;
            spread_count = number_can_spread;
            percent_vaccinated = vaccinated_fraction;
            people.reserve(population_count); // quickly reserving the vector for the slight speedup over push_back

            /* 
               This loop generates each person. It checks to see if they're 
               vaccinated or not based on the chance specified at the
               start of the program and generates that person. It keeps
               going until every person in the population is defined.
            */

            for(int index = 0; index < population_count; index++){
                bool is_vaccinated = (1.0*rand()/RAND_MAX < percent_vaccinated);
                if (is_vaccinated)
                    people[index] = person(-2);
                else
                    people[index] = person(0);
            }//for  loop
        }//constructor

        // class methods
        
        /*
            The count_infected method iterates over the population to 
            calculate how many of them are infected. This could be deprecated
            by making the infect_random method as well as some other methods
            iterate a new private member of the class called number_infected
            or something. Regardless, this method returns an integer of how
            many people in the population are infected.
        */

    int count_infected(){
        int infected_count = 0;
        for(int index = 0; index < population_count; index++){
            if(people[index].is_infected())
                infected_count++;
        }//for loop
        return infected_count;
        }//method

        /*
            The count_vulnerable method does the same thing as the count_infected
            method but for vulnerable people in the population instead of infected ones.
        */

        int count_vulnerable() { //needed to optimize vaccine% based on % transmission chance
            int vulnerable_count = 0;
            for(int index = 0; index < population_count; index++){
                if(people[index].is_vulnerable())
                    vulnerable_count++;
            }//for loop
            return vulnerable_count;
        }

        /*
            population_breakdown is a method to print a string 
            showing the status of each population member
            based on the symbols in symbol_key, which can be
            edited if need be. It returns those symbols in a 
            consecutive string WITHOUT any newlines. 
            It takes the optional argument print_status, 
            which if true, will allow the final string
            to be printed out.
        */

        string population_breakdown(bool print_status=false){
            string breakdown = "";
            for(int index = 0; index < population_count; index++){
                switch (people[index].get_status()){
                    case -2:
                        breakdown += symbol_key[0];
                        break;
                    case -1:
                        breakdown += symbol_key[1];
                        break;
                    case 0:
                        breakdown += symbol_key[2];
                        break;
                    default:
                        breakdown += symbol_key[3];
                        break;
                }//switch case
            }//for loop
            if (print_status)
                cout << breakdown;
            return breakdown;
        }//method

        //infection methods:

        /* 
            The infect_random() method randomly chooses a vulnerable member of the population
            and infects them with the disease. It can be called multiple times to increase
            or decrease the initial number of infected people, changing the initial
            conditions of the system.
        */

        void infect_random(){
            int index  = rand() % population_count;
            
            /* 
                note that we need to check the edge cases: we unluckily try 
                to infect the same person twice or we try to infect 
                someone vaccinated by accident.
            */

            while(!people[index].is_vulnerable()){
                index  = rand() % population_count;
            } //while loop
            people[index].infect(infection_length);
        }//method

        /*
            The infect_spreading method takes an integer argument corresponding
            to the index of an infected person in the people vector. It will
            roll a random index within the population other than that person
            and check to see if that other person can be infected.
            If they can, it rolls to check if an infection occurs, and if it does
            it calls upon the newly-infected person's .infect() method to infect them.
            It will keep trying to do this until it's rolled the number of people 
            that each infected person can contact on a daily basis, spread_count
        */

        void infect_spreading(int index){
            
            for(int infection_tries = 0; infection_tries < spread_count; infection_tries++){

                /*
                    we CANNOT allow an index that's the same as the current one, so we need to check
                    to ensure that it's 6 random other people a day
                */

                int random_index = rand() % population_count;
                while (random_index == index){
                    random_index = rand() % population_count;
                }//while loop
            if(people[random_index].is_vulnerable()){
                float infection_roll = 1.0 * rand() / RAND_MAX;
                if (infection_roll < infection_chance) 
                    people[random_index].infect(infection_length);
             }//if conditional
            } //for loop
        } //method
        
        /* 
            The infect_geographical method is similar to the infect_spreading method.
            It takes an integer that is the index of an infected person in the people
            vector, and it will attempt to infect the spread_count/2 people ahead and 
            spread_count/2 people behind them in the people vector. If someone
            is at the start or end of the people vector , then it will automatically
            consider the nth person adjacent to them from the other end. For example,
            if someone is at index 0 in a population of 150, it will attempt to infect
            the people at indices 148, 149, 1, and 2.
        */

        void infect_geographical(int index){

            for(int infection_tries = -spread_count/2; infection_tries <= spread_count/2; 
                    infection_tries++){
                if (infection_tries == 0) // this is to skip over trying to infect the already-infected person; just saves some cycles.
                    continue;
                int neighbor_index = index + infection_tries;
                /*
                     This is where that idea of making the vector of infected people
                     sort of cyclical comes into play.
                */

                if (neighbor_index < 0)
                    neighbor_index += population_count;
                else if (neighbor_index >= population_count)
                    neighbor_index -= population_count;
                
                if(people[neighbor_index].is_vulnerable()){
                    float infection_roll = 1.0 * rand() / RAND_MAX;
                    if (infection_roll < infection_chance)
                        people[neighbor_index].infect(infection_length);
                }//if conditional
            }//for loop
        }//method

        /* 
            The infect_hybrid method is a 50/50 combination of infect_spread
            and infect_geographical method. Similarly, it takes the index of an
            infected person. It attempts to infect the spread_count/4 people ahead 
            and behind it. Then, it attempts to randomly infect spread_count/2 people
            in the population to account for people meeting both random people and 
            the same people on a daily basis.
        */

        void infect_hybrid(int index){
            for(int infection_tries = -spread_count/4; infection_tries <= spread_count/4; 
                infection_tries++){
                if (infection_tries == 0)
                    continue;
                int neighbor_index = index + infection_tries;

                if (neighbor_index < 0)
                    neighbor_index += population_count;
                else if (neighbor_index >= population_count)
                    neighbor_index -= population_count;

                if(people[neighbor_index].is_vulnerable()){
                    float infection_roll = 1.0 * rand() / RAND_MAX;
                    if (infection_roll < infection_chance)
                        people[neighbor_index].infect(infection_length);
                }//if conditional

                // and now we do it for the random people/interactions:
                int random_index = rand() % population_count;
                while (random_index == index){
                    random_index = rand() % population_count;
                }//while loop
                if(people[random_index].is_vulnerable()){
                    float infection_roll = 1.0 * rand() / RAND_MAX;
                    if (infection_roll < infection_chance)
                        people[random_index].infect(infection_length);
                }//if conditional

                }//for loop
        }//method




        /*
            The update method will advance the simulation by a day. It will first 
        */
        void update(){
            for (int index = 0; index < population_count; index++){
                if (people[index].get_status() <= (infection_length-1) && people[index].get_status() >= 1){
                    switch(spread_type){
                        case 1:
                            infect_spreading(index);
                            break;
                        case 2:
                            infect_geographical(index);
                            break;
                        case 3:
                            infect_hybrid(index);
                            break;
                    } //switch case
                }//if conditional

                /*
                    If performance is critical, consider calling the person.update() method
                    inside the if conditional. Otherwise, it will attempt to call it for uninfected
                    people and waste cycles. This was done in case this header is built upon later
                    for more advanced epidemiological models, like SEIRS.
                */
                
            }//for loop

            for (int index = 0; index < population_count; index++){
		people[index].update();
	    }
            
        }//method

}; //class



