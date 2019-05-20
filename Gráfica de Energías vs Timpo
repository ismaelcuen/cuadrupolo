SUBROUTINE eKU(proton3%vel,proton3%mass,t)
  USE charged_objects
  USE vector_functions
  IMPLICIT NONE
  !K: Energía Cinética
  !U: Energía Potencial
  !proton3%mass: Masa
  !a: Aceleración
  !r: Radio
  !v: Velocidad
  !i: Contador
  !_________________________________________________________
  REAL,INTENT(IN)::proton3%vel,proton3%mass
  REAL::a,r
  OPEN(1,FILE="Graph",STATUS="UNKNOWN")
     K=(1/2)*proton3%mass*proton3%vel**2
     U=proton3%mass*a*r
     WRITE(1,*)t,U,K,U+K
     CLOSE(1)
   END SUBROUTINE eKU
