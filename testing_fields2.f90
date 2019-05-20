PROGRAM testing_fields
USE charged_objects
USE vector_functions
IMPLICIT NONE

!!Definiendo mis objetos masivos
TYPE(charged_particles) :: antiproton !!Objeto tipo partícula cargada: antiprotón
TYPE(charged_particles) :: proton !!Objeto tipo partícula
!!cargada: protón estacionario
TYPE(data) :: sim !!Objeto con información de la simulación

!!Definiendo variables locales
REAL(d) :: t !!Instante de tiempo en cada aproximación
REAL(d) :: scale=1E9 !!Escalar longitudes en los plots
REAL ( d ), DIMENSION ( 3 ) :: runit !!Vector Unitario
INTEGER :: unit0 !!Unidad para archivo de salida
REAL(d),DIMENSION(4)::particulas !Arreglo contenedor de las 4 partículas.
!!Asignando valores físicos a la partícula en movimiento (MKS)
antiproton%mass = 1.7E-27_d !!Masa del antiprotón
antiproton%q = -1.6E-19_d !!carga del antiprotón
!!Asignando valores físicos a la partícula fija (MKS)
proton%mass = 1.7E-27_d !!Masa del protón
proton%q = 1.6E-19_d !!Carga del protón
!Construyendo el vector de velocidad inicial del antiprotón.
antiproton%vel = vector(5E1_d,8E2_d,5E1_d)
!Construyendo el vector de posición inicial del antiprotón.
antiproton%pos = vector(0.5E-8_d,-1E-8_d,0.0_d)
!Vector de velocidad del protón (Fijo en esta aproximación)
proton%vel = 0.0_d
!Vector de posición del protón (Fijo en esta aproximación)
proton%pos = vector(-0.5E-8_d,0.0_d,0.0_d)
!!Construyendo el vector de Momento lineal inicial del antiprotón.
antiproton%mom = antiproton%mass*antiproton%vel
antiproton%Eelec = 0.0_d
!!Ajustando el tamaño de paso (Cantidad de veces en las que se
!!actualizará la información del sistema).
sim%N_step = 5000 !!Usaré sólo 5000 pasos.
sim%ttot = 2E-11_d !!En este ejemplo se realizará la
!!evolución durante unos segundos
!sim%ttot = sim%ttot/t0 !!Traduciendo a unidades naturales.
sim%dt = sim%ttot/sim%N_step !!Calculando el ancho de paso
particulas(1)=proton1
particulas(2)=proton2
particulas(3)=antiproton1
particulas(4)=antiproton2
!!Abriendo el archivo de texto
OPEN(NEWUNIT=unit0,FILE="Sim_ejemplo",STATUS="UNKNOWN",ACCESS="APPEND")
t=0 !!La simulación inicia al tiempo t=0
!Repetir el proceso hasta que se cumpla la condición
DO
IF(t > sim%ttot ) EXIT
sim%pos2 = antiproton%pos-proton%pos !!Calculando el vector r=r2-r1
runit=sim%pos2/mag(sim%pos2)
!!Calculando la interacción eléctrica.
protonmovil%Eelect=0.0_d
DO 1,4
CALL campoelectrico(protonmovil,particulas(i))
protonmovil%Eelec=protonmovil%Eelec+protonmovil%Eelec
END DO
!!Actualizando el momento lineal
antiproton%mom = antiproton%mom + antiproton%Felec * sim%dt
!!Actualizando la posición
antiproton%pos = antiproton%pos + antiproton%mom / antiproton%mass * sim%dt
WRITE(unit0,*) t , antiproton%pos*scale !!Escribiendo en el archivo
t = t + sim%dt !!Actualizando el tiempo
END DO

CLOSE ( unit0 )
!!Este archivo sirve para la información del sol
OPEN ( NEWUNIT = unit0 , FILE = "proton" , STATUS = "UNKNOWN" )
WRITE ( unit0 , * ) proton%pos*scale
CLOSE ( unit0 )
END PROGRAM testing_fields
