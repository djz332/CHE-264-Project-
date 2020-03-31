module algorithm
implicit none

integer(kind = 4) :: population_size, lattice_length, quarantine_day, patient_0 ! inputs
integer(kind = 4), allocatable, dimension(:,:) :: home, tracer_distribution ! inputs
integer(kind = 4) :: displacement, displacement_x, displacement_y, num_infected, death_count&
,prev_infected, new_cases, susceptible_count, recovered_count, counter = 0
integer(kind = 4) :: I, J, k, x, y
REAL(kind = 4) :: RAND, RAND2, RAND_OUTCOME, RAND_TRANSFER, ANGLE_RADIAN, PI = 4.0*atan(1.0)
REAL(kind =4) :: transferability !inputs
REAL(Kind = 4), allocatable, dimension(:,:) :: population_statistics, coord, reset_coord ! inputs 

contains

subroutine open_file()

   open(unit = 1, file = 'record_file.csv')
   open(unit = 2, file = 'trajectory.csv')
   !print*, "coord: ", shape(coord), "stats: ", shape(population_statistics), "distribution: ", shape(tracer_distribution)

end subroutine open_file

 
subroutine dynamics() 

num_infected = patient_0
new_cases = patient_0

DO WHILE (num_infected .GT. 0)
 
 counter = counter + 1
 print*, "DAY: ", counter

 TRACER_DISTRIBUTION(:,:) = HOME(:,:)
 
 DO k = 1,12
  
  DO I =  1,POPULATION_SIZE

     IF (NINT(POPULATION_STATISTICS(i,1)) .GE. (-1) .AND. POPULATION_STATISTICS(i,1) .LE. (quarantine_day)) THEN
        
        CALL RANDOM_NUMBER(RAND)
        CALL RANDOM_NUMBER(RAND2)
        
        IF (RAND .GT. POPULATION_STATISTICS(i,6)) THEN
           
           DISPLACEMENT = NINT(RAND*POPULATION_STATISTICS(i,5)) 
           ANGLE_RADIAN = 2*PI*RAND2
           DISPLACEMENT_X = NINT(DISPLACEMENT*COS(ANGLE_RADIAN))
           DISPLACEMENT_Y = NINT(DISPLACEMENT*SIN(ANGLE_RADIAN))

           if (TRACER_DISTRIBUTION(i,1) + DISPLACEMENT_X .ge. lattice_length&
.or. TRACER_DISTRIBUTION(i,1) + DISPLACEMENT_X .lt. 1&
 .or. TRACER_DISTRIBUTION(i,2) + DISPLACEMENT_Y .ge. lattice_Length &
.or. TRACER_DISTRIBUTION(i,2) + DISPLACEMENT_Y .lt. 1) then

              TRACER_DISTRIBUTION(i,1) = home(i,1)
              TRACER_DISTRIBUTION(i,2) = home(i,2)
        
           else
              
              TRACER_DISTRIBUTION(i,1) = TRACER_DISTRIBUTION(i,1) + DISPlACEMENT_X
              TRACER_DISTRIBUTION(i,2) = TRACER_DISTRIBUTION(i,2) + DISPLACEMENT_Y
              
              if(mod(I,1000) .eq. 0 .and. k .eq. 12) then

                 write(2,*) I, TRACER_DISTRIBUTION(i,1), TRACER_DISTRIBUTION(i,2)
                 
              end if

           end if

           if (NINT(POPULATION_STATISTICS(i,1)) .GE. 1) then
               
              coord(TRACER_DISTRIBUTION(i,1),TRACER_DISTRIBUTION(i,2)) =&
  coord(TRACER_DISTRIBUTION(i,1),TRACER_DISTRIBUTION(i,2))*(1 - transferability) !stores chance of not getting infected
        
           end if 
            
        END IF

     END IF
 
  END DO
 
  DO I  = 1,POPULATION_SIZE
     
     if (NINT(POPULATION_STATISTICS(I,1)) .EQ. 0 .AND.  coord(TRACER_DISTRIBUTION(I,1),TRACER_DISTRIBUTION(I,2)) .LT. 1) then
        
        CALL RANDOM_NUMBER(RAND_TRANSFER)  
       
        if (RAND_TRANSFER .GT.  coord(TRACER_DISTRIBUTION(I,1),TRACER_DISTRIBUTION(I,2))) then
           
           POPULATION_STATISTICS(I,1) = 1 ! INFECTED
           !print*, 'infected'
           
        end if

     end if  
  
  END DO
  
  COORD(:,:) = RESET_COORD(:,:) !all ones 
  print*, "   HOUR: ", k

 END DO

 prev_infected = 0
 death_count = 0
 new_cases = 0
 susceptible_count = 0
 recovered_count = 0
 DO I = 1,POPULATION_SIZE

    if (NINT(POPULATION_STATISTICS(i,1)) .EQ. NINT(POPULATION_STATISTICS(i,4))) then

       CALL RANDOM_NUMBER(RAND_OUTCOME)
       
       if (RAND_OUTCOME .LE. POPULATION_STATISTICS(i,3)) then
          POPULATION_STATISTICS(i,1) = -2 !dead
          !print*, 'dead'

       else

          POPULATION_STATISTICS(i,1) = -1 !recovered
          !print*, "recovered"

       end if

    end if

    if (NINT(POPULATION_STATISTICS(i,1)) .GT. 1) then
 
       prev_infected = prev_infected + 1
       POPULATION_STATISTICS(i,1) = POPULATION_STATISTICS(i,1) + 1

    else if(NINT(POPULATION_STATISTICS(i,1)) .EQ. 1) then

       new_cases = new_cases + 1
       POPULATION_STATISTICS(i,1) = POPULATION_STATISTICS(i,1) + 1

    else if (NINT(POPULATION_STATISTICS(i,1)) .EQ. -2) then

       death_count = death_count + 1

    else if (NINT(POPULATION_STATISTICS(i,1)) .EQ. 0) then

       susceptible_count = susceptible_count + 1

    else if (NINT(POPULATION_STATISTICS(i,1)) .EQ. -1) then

       recovered_count = recovered_count + 1
     
    end if

 END DO

 num_infected = prev_infected + new_cases

 write(1,*) counter, num_infected, susceptible_count, recovered_count, death_count, new_cases

 TRACER_DISTRIBUTION(:,:) = HOME(:,:)

END DO

end subroutine dynamics 


subroutine close_file()

  close(1)
  close(2)

end subroutine close_file


end module algorithm
