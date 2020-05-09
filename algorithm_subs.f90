module algorithm
implicit none

integer(kind = 4), allocatable, dimension(:,:) :: home, tracer_distribution, population_statistics ! inputs
integer(kind = 4) :: displacement, displacement_x, displacement_y, active_cases, death_count&
,prev_cases, quarantined_cases, new_cases, susceptible_count, recovered_count, counter
integer(kind = 4) :: I, J, k, x, y
REAL(kind = 4) :: RAND, RAND1, RAND2, RAND_OUTCOME, RAND_TRANSFER, ANGLE_RADIAN, PI = 4.0*atan(1.0)
REAL(Kind = 4), allocatable, dimension(:,:) :: coord, reset_coord ! inputs 
integer(kind = 4) :: population_size, lattice_length, patient_0, quarantine_day, mobility, daily_moves
real(kind = 4) :: transferability, fatality_rate, static, curve_ratio

contains

subroutine open_file()

   open(unit = 1, file = 'record_file.csv')
   open(unit = 2, file = 'trajectory.csv')
   write(1,*) "#", " ","Day", " ", "new case", " ", "infected", " ", "susceptible", " ", "recovered", " ", "deaths"   
   
end subroutine open_file

 
!subroutine dynamics(population_size, lattice_length, patient_0, quarantine_day, transferability, fatality_rate&
!, mobility, daily_moves, static) 

!integer(kind = 4), intent(in) :: population_size, lattice_length, patient_0, quarantine_day, mobility, daily_moves
!real(kind = 4), intent(in) :: transferability, fatality_rate, static 

subroutine dynamics()

active_cases = patient_0
susceptible_count = population_size - active_cases
counter = 0

DO WHILE (active_cases .GT. 0)
 
 counter = counter + 1
 print*, "DAY: ", counter
 
 !daily_moves = NINT(curve_ratio/((dble(susceptible_count)/dble(lattice_length**2))*transferability)) 

 DO k = 1,daily_moves
  
  DO I =  1,POPULATION_SIZE
     
     IF ((POPULATION_STATISTICS(i,1)) .GT. (-1) .AND. POPULATION_STATISTICS(i,1) .LE. (quarantine_day)) THEN
        
        CALL RANDOM_NUMBER(RAND)
        CALL RANDOM_NUMBER(RAND1)
        CALL RANDOM_NUMBER(RAND2)
        
        IF (RAND .GE. static) THEN
           
           DISPLACEMENT = NINT(RAND1*mobility) 
           ANGLE_RADIAN = 2*PI*RAND2
           if (DISPLACEMENT .eq. 0) then
    
              DISPLACEMENT = 1

           end if
           DISPLACEMENT_X = NINT(DISPLACEMENT*COS(ANGLE_RADIAN))
           DISPLACEMENT_Y = NINT(DISPLACEMENT*SIN(ANGLE_RADIAN))

           if (TRACER_DISTRIBUTION(i,1) + DISPLACEMENT_X .ge. lattice_length&
.or. TRACER_DISTRIBUTION(i,1) + DISPLACEMENT_X .lt. 1&
 .or. TRACER_DISTRIBUTION(i,2) + DISPLACEMENT_Y .ge. lattice_Length &
.or. TRACER_DISTRIBUTION(i,2) + DISPLACEMENT_Y .lt. 1) then

              DISPLACEMENT_X = 0
              DISPLACEMENT_Y = 0
        
           else
              
              TRACER_DISTRIBUTION(i,1) = TRACER_DISTRIBUTION(i,1) + DISPlACEMENT_X
              TRACER_DISTRIBUTION(i,2) = TRACER_DISTRIBUTION(i,2) + DISPLACEMENT_Y
              
              if(mod(I,1000) .eq. 0 .and. k .eq. daily_moves) then

                 write(2,*) I, TRACER_DISTRIBUTION(i,1), TRACER_DISTRIBUTION(i,2)
                 
              end if

           end if

        END IF

     END IF ! move condition 

     IF (k .eq. daily_moves) then

        TRACER_DISTRIBUTION(i,:) = HOME(i,:) ! for group effects/family clusters purposes

     END IF
      
     IF ((POPULATION_STATISTICS(i,1)) .GT. 1 .AND. (POPULATION_STATISTICS(i,1)) .LE. (quarantine_day)) then

        coord(TRACER_DISTRIBUTION(i,1),TRACER_DISTRIBUTION(i,2)) =&
coord(TRACER_DISTRIBUTION(i,1),TRACER_DISTRIBUTION(i,2))*(1 - transferability) 
!stores chance of not getting infected
        
     END IF !set up infection grid hotspots
 
  END DO ! population loop
 
  DO I  = 1,POPULATION_SIZE
     
     if ((POPULATION_STATISTICS(I,1)) .EQ. 0 .AND.  coord(TRACER_DISTRIBUTION(I,1),TRACER_DISTRIBUTION(I,2)) .LT. 1) then
        
        CALL RANDOM_NUMBER(RAND_TRANSFER)  
       
        if (RAND_TRANSFER .GT.  coord(TRACER_DISTRIBUTION(I,1),TRACER_DISTRIBUTION(I,2))) then
           
           POPULATION_STATISTICS(I,1) = 1 ! INFECTED
           !print*, 'infected'
           
        end if

     end if  
  
  END DO ! infection loop
  
  COORD(:,:) = RESET_COORD(:,:) !all ones 
  print*, "   Move: ", k

 END DO ! single day loop

 prev_cases = 0
 quarantined_cases = 0
 death_count = 0
 new_cases = 0
 active_cases = 0
 susceptible_count = 0
 recovered_count = 0
 DO I = 1,POPULATION_SIZE

    if ((POPULATION_STATISTICS(i,1)) .GE. (POPULATION_STATISTICS(i,2) + quarantine_day)) then

       CALL RANDOM_NUMBER(RAND_OUTCOME)
       
       if (RAND_OUTCOME .LE. fatality_rate) then
         
          POPULATION_STATISTICS(i,1) = -2 !dead
          !print*, 'dead'

       else

          POPULATION_STATISTICS(i,1) = -1 !recovered
          !print*, "recovered"

       end if

    end if

    if ((POPULATION_STATISTICS(i,1)) .GT. 1 .AND. (POPULATION_STATISTICS(i,1)) .LE. quarantine_day) then
 
       prev_cases = prev_cases + 1
       POPULATION_STATISTICS(i,1) = POPULATION_STATISTICS(i,1) + 1

    else if((POPULATION_STATISTICS(i,1)) .EQ. 1) then

       new_cases = new_cases + 1
       POPULATION_STATISTICS(i,1) = POPULATION_STATISTICS(i,1) + 1

    else if ((POPULATION_STATISTICS(i,1)) .GT. quarantine_day) then

       quarantined_cases = quarantined_cases + 1
       POPULATION_STATISTICS(i,1) = POPULATION_STATISTICS(i,1) + 1

    else if ((POPULATION_STATISTICS(i,1)) .EQ. -2) then

       death_count = death_count + 1

    else if ((POPULATION_STATISTICS(i,1)) .EQ. 0) then

       susceptible_count = susceptible_count + 1

    else if ((POPULATION_STATISTICS(i,1)) .EQ. -1) then

       recovered_count = recovered_count + 1
     
    end if
    !print*, population_statistics(i,1), population_statistics(i,2)

 END DO ! end population loop (outcomes, and statistics count)

 active_cases = prev_cases + new_cases + quarantined_cases

 write(1,*) counter, new_cases, prev_cases, active_cases, susceptible_count, recovered_count, death_count

 TRACER_DISTRIBUTION(:,:) = HOME(:,:) ! reset to home positions each day

END DO ! infection cycle while loop

end subroutine dynamics 


subroutine close_file()

  close(1)
  close(2)

end subroutine close_file


end module algorithm
