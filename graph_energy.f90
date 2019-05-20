SUBROUTINE eKU(a,b)
  USE charged_objects
  USE vector_functions
  IMPLICIT NONE
  !K: Energía Cinética
  !U: Energía Potencial
  !proton3%mass: Masa
  !ac: Aceleración
  !r: Radio
  !v: Velocidad
  !i: Contador
  !a: Variable dummy para el protonmovil enviado del programa principal
  !-------------------------------------------
  REAL::ac,r,K,U
  TYPE(charged_particle),INTENT(IN)::a
  REAL,INTENT::b
  OPEN(UNIT=1,FILE="Graph",STATUS="UNKNOWN",ACTION="WRITE")
  a%vel=a%mom/a%mass
  ac=a%Felec/a%mass
  K=(1/2)*a%mass*a%vel**2
  U=a%mass*ac*r
  WRITE(1,*)b,U,K,U+K
  CLOSE(1)
END SUBROUTINE eKU
