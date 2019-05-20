SUBROUTINE campoelectrico(a,b)
USE charged_particles
USE vector_functions
  IMPLICIT NONE
  REAL,DIMENSION(3)::a,b
  protonmovil%Eelect=0.0_d
  sim%pos2 = a%pos-b%pos !!Calculando el vector r=r2-r1
  runit=sim%pos2/mag(sim%pos2)
  !!Calculando la interacción eléctrica.
  a%Eelec=( k*a%q / mag(sim%pos2)**2)*runit
  a%Felec = a%Felec+a%q*a%Eelec
END SUBROUTINE campoelectrico
