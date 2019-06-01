SUBROUTINE lecture_fichier

USE module_VF


OPEN(2,FORM='FORMATTED',FILE='data.txt')

	READ(2,*)L
	READ(2,*)A
	READ(2,*)alpha_a
	READ(2,*)alpha_b
	READ(2,*)Ny
	READ(2,*)Nx
	READ(2,*)CFL
	READ(2,*)r
	READ(2,*)To
	READ(2,*)Ta
	READ(2,*)Tb
	READ(2,*)Uo
	READ(2,*)Tf

 CLOSE (2)

END SUBROUTINE lecture_fichier
