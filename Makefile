# Make file (Use EXECUTABLE or EXECUTABLE) 
FC			    =	gfortran
#FFLAGS		  =	-O3 -Wall -Wextra -g -fopenmp -std=legacy 
FFLAGS		  =	-O3
#LINALG		  =	-L/usr/lib -lblas -llapack
SRC			    =	$(wildcard *.f90)
OBJ			    =	${SRC:.f90=.o}					# Obj files: file1.o file2.o ... filen.o
EXECUTABLE	=	Run.x

#default:
#	@echo "compiling Bisection Program using" $(FC)

%.o: %.f90								# compile without linking
	$(FC) $(FFLAGS) -o $@ -c $<	# $(LINALG)

%mod: %.f90					  	# for mod file
	$(FC) $(FFLAGS) -o $@ -c $<	# $(LINALG)

$(EXECUTABLE): $(OBJ)						# final executable is 
	@echo "-------------------------------"
	@echo "Creating executable for program"
	@echo "-------------------------------"
	@$(FC) $(FFLAGS) -o $@ $(OBJ) $(LINALG)
	@echo " "
	@echo "executable file = " $(EXECUTABLE)
	@echo " "
	@echo "-------------------------------"
clean:
	@rm -f *.o *.mod $(EXECUTABLE)

