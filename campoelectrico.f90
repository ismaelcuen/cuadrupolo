SUBROUTINE campoelectrico(a,b)
USE charged_particles
USE vector_functions
  IMPLICIT NONE
  REAL,DIMENSION(3)::a,b
  antiproton%Eelec=( k*proton%q / mag(sim%pos2)**2)*runit
  antiproton%Felec = antiproton%q*antiproton%Eelec
END SUBROUTINE campoelectrico
