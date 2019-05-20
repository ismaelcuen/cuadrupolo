SUBROUTINE campoelectrico(a,b)
USE charged_particles
USE vector_functions
  IMPLICIT NONE
  REAL,DIMENSION(3)::a,b
  REAL,DIMENSION(3)::r
  r=ABS(a%pos-b%pos)
  a%Felec_d=(K*a%q_d*b%q_d)/r**2
  a%Eelec_d=a%Felec_d/a%q_d
END SUBROUTINE campoelectrico
