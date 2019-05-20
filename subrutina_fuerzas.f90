SUBROUTINE campoelectrico(a,b)
USE charged_particles
USE vector_functions
  IMPLICIT NONE
  REAL,DIMENSION(3)::r
  r=ABS(a%pos-b%pos)
  a%Felec_d=(K*a%q*b%q)/r**2
END SUBROUTINE campoelectrico
