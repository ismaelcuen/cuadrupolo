PROGRAM testing_fields
USE charged_objects
USE vector_functions
IMPLICIT NONE

!!Definiendo mis objetos masivos
TYPE(charged_particles) :: antiproton1,antiproton2 !!Objeto tipo partícula cargada: antiprotón
TYPE(charged_particles) :: proton1,proton2,protonmovil !!Objeto tipo partícula
!!cargada: protón estacionario
TYPE(data) :: sim !!Objeto con información de la simulación

!!Definiendo variables locales
REAL(d) :: t !!Instante de tiempo en cada aproximación
REAL(d) :: scale=1E9 !!Escalar longitudes en los plots
REAL ( d ), DIMENSION ( 3 ) :: runit !!Vector Unitario
INTEGER :: unit0 !!Unidad para archivo de salida
INTEGER::i,j
TYPE(charged_particles),DIMENSION(4)::particulas !Arreglo contenedor de las 4 partículas.
particulas(1)=proton1
particulas(2)=proton2
particulas(3)=antiproton1
particulas(4)=antiproton2
DO i=1,4
IF(i=1 .OR. i=2)THEN
particulas(i)%mass= 1.7E-27_d
particulas(i)%q= 1.6E-19_d
particulas(i)%vel= vector(0.0_d,0.0_d,0.0_d)
particulas(i)%pos= vector(0.0_d,0.0_d,0.0_d)
ELSE IF(i=3 .OR. i=4)THEN
particulas(i)%mass= 1.7E-27_d
particulas(i)%q= -1.6E-19_d
particulas(i)%vel= vector(0.0_d,0.0_d,0.0_d)
IF(i=3)THEN
particulas(i)%pos= vector(0.0_d,0.5E-8_d,0.0_d)
ELSE IF(i=4)THEN
particulas(i)%pos= vector(0.0_d,-0.5-8_d,0.0_d)
END IF
END IF
END DO
!!Asignando valores físicos a la partícula en movimiento (MKS)
!antiproton%mass = 1.7E-27_d !!Masa del antiprotón
!antiproton%q = -1.6E-19_d !!carga del antiprotón
!!Asignando valores físicos a la partícula fija (MKS)
protonmovil%mass = 1.7E-27_d !!Masa del protón
protonmovil%q = 1.6E-19_d !!Carga del protón
!Construyendo el vector de velocidad inicial del antiprotón.
!antiproton%vel = vector(5E1_d,8E2_d,5E1_d)
!Construyendo el vector de posición inicial del antiprotón.
!antiproton%pos = vector(0.5E-8_d,-1E-8_d,0.0_d)
!Vector de velocidad del protón (Fijo en esta aproximación)
protonmovil%vel = vector(5E1_d,8E2_d,5E1_d)
!Vector de posición del protón (Fijo en esta aproximación)
protonmovil%pos = vector(0.7E-9_d,0.0_d,0.0_d)
!!Construyendo el vector de Momento lineal inicial del antiprotón.
protonmovil%mom = protonmovil%mass*protonmovil%vel
protonmovil%Eelec = 0.0_d
!!Ajustando el tamaño de paso (Cantidad de veces en las que se
!!actualizará la información del sistema).
sim%N_step = 5000 !!Usaré sólo 5000 pasos.
sim%ttot = 2E-11_d !!En este ejemplo se realizará la
!!evolución durante unos segundos
!sim%ttot = sim%ttot/t0 !!Traduciendo a unidades naturales.
sim%dt = sim%ttot/sim%N_step !!Calculando el ancho de paso
!!Abriendo el archivo de texto
OPEN(NEWUNIT=unit0,FILE="Sim_ejemplo",STATUS="UNKNOWN",ACCESS="APPEND")
t=0 !!La simulación inicia al tiempo t=0
!Repetir el proceso hasta que se cumpla la condición
DO
IF(t > sim%ttot ) EXIT
DO j=1,4
CALL campoelectrico(protonmovil,particulas(j))
END DO
!!Actualizando el momento lineal
protonmovil%mom = protonmovil%mom + protonmovil%Felec * sim%dt
!!Actualizando la posición
protonmovil%pos = protonmovil%pos + protonmovil%mom / protonmovil%mass * sim%dt
WRITE(unit0,*) t , protonmovil%pos*scale !!Escribiendo en el archivo
CALL eKU(protonmovil,t)
t = t + sim%dt !!Actualizando el tiempo
END DO

CLOSE ( unit0 )
!!Este archivo sirve para la información del sol
OPEN ( NEWUNIT = unit0 , FILE = "proton" , STATUS = "UNKNOWN" )
WRITE ( unit0 , * ) proton%pos*scale
CLOSE ( unit0 )
END PROGRAM testing_fields
