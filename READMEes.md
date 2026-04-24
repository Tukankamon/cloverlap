[README in 🇬🇧](./README.md)

---

[!IMPORTANT]
> Este readme no es el principal y seguramente esté desactualizado, para la versión más actual posible ver [el readme en inglés](./README.md)

Este programa toma una lista detallada de posibles clases de universidad en el formato especificado en el [csv de ejemplo](./example.csv). Calcula el horario más óptimo de clases dependiendo de: el descanso mínimo tanto entre exámenes como en clases, cuantas clases (máximo y mínimo) te quieres apuntar y optimiza para devolver las clases con menos "tiempo muerto" entre cada una de ellas. El resultado final es un horario con clases y exámenes que no se pisan unos con otros y aprovecha tu tiempo al máximo para que no te tires 4 horas esperando en la universidad matando el tiempo por que vives lo suficientemente lejos que no te da tiempo a ir y volver

# Syntaxis para el CSV
Seguir las cabeceras que se muestran en el ejemplo. Las fechas de clases son del formato: "diaDeSemana horaInicio horaFin" y las de exámenes: "fechaExacta horaInicio horaFin"

Los horarios vacíos no se tienen en cuenta

las fechas para los examenes se pueden añadir en el formato: {dia} <horaInicion> <horaFine>. Las horas de comienzo y fin son opcionales y tendrán el valor por defecto de medio día. Ambas opciones se muestran en el ejemplo

Lineas empezadas con '#' serán ignoradas y contarán como comentarios

# Uso
Esto sólo está en inglés por ahora lamentablemente
```IO
Usage: cloverlap [-i|--input FILENAME] [-v|--verbose] [--class-rest INTEGER] 
                 [--exam-rest INTEGER] [-M|--max-classes INTEGER] 
                 [-m|--min-classes INTEGER] [-s|--semester INT] [-l|--loosen]

  Compute the most optimal class schedule from a list of courses

Available options:
  -i,--input FILENAME      File name (not path) of the csv file to parse)
                           (default: "example.csv")
  -v,--verbose             Whether to enable verbose mode when printing out data
  --class-rest INTEGER     How much time in minutes minimum in between classes
                           (default: 10)
  --exam-rest INTEGER      How much time in days minimum in between exams. A
                           value of 0 means allowing exams on the same day
                           (default: 1)
  -M,--max-classes INTEGER Maximum classes to sign up for, this includes those
                           that wont be attended to but will self study to go to
                           the exam (default: 8)
  -m,--min-classes INTEGER Absolute minimum amount of classes to attend
                           (default: 5)
  -s,--semester INT        Semester to analyze (1 or 2)
  -l,--loosen              If the program fails with the given restrictions, it
                           will lower them until it finds a match
  -h,--help                Show this help text
```

# Nix (flakes)
El programa se puede ejecutar mediante Nix:
```nix
nix run github:Tukankamon/cloverlap#<paquete>
```
La opción ```nix <paquete>``` es para elegir entre la versión en la terminal y la version del servidor web. El paquete por defecto es el de la terminal

Para descargártelo permanentemente añádelo a las entradas de tu flake

# Http
El frontend manda JSON conteniendo el csv y los argumentos (Llamados ArgInput en el backend)

El backend manda de vuelta el siguente struct en forma de JSON:
```haskell
data Response = Response
	{ title :: String
	, calendar :: Schedule
	, exams :: [Day]
	} deriving (Show, Generic)
instance ToJSON Response
```

# Cloverlap?
Viene de class + overlap en inglés. A veces se reduce sólo a clover (trébol)
