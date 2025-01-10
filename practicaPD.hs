-----------------------------------------------------------------------
-- Práctica PD
-- Miguel Ángel Molina de la Rosa
-- DNI : 05989024P
-----------------------------------------------------------------------

-----------------------------------------------------------------------
-- Definiciones de tipos (forma posicional)
-----------------------------------------------------------------------
-- Hemos definido cada data con parámetros posicionales en vez de
-- usar sintaxis de registros. Así, podemos leerlos
-- con notación tipo 'Pregunta 3 1 1.0' directamente.

-- data Pregunta(Int, Int, Double)
--   1) numero_posibilidades
--   2) respuesta_correcta
--   3) valor
data Pregunta = Pregunta Int Int Double
  deriving (Show, Read, Eq)
--   Esta definición crea el tipo 'Pregunta' con tres campos posicionales:
--   Int (número de posibilidades),
--   Int (respuesta correcta) 
--   Double (valor de la pregunta).
--   deriving (Show, Read, Eq)' nos permite imprimirlo, leerlo y compararlo.

-- A continuación, defino funciones "getter" (accesores) para extraer cada campo.
numero_posibilidades :: Pregunta -> Int
numero_posibilidades (Pregunta np _ _) = np
--  Devuelve el primer campo (np), o sea el número de posibilidades.

respuesta_correcta :: Pregunta -> Int
respuesta_correcta (Pregunta _ rc _) = rc
--  Devuelve el segundo campo (rc), la respuesta correcta.

valor :: Pregunta -> Double
valor (Pregunta _ _ v) = v
--  Devuelve el tercer campo (v), el valor de la pregunta.


-- data Test([Pregunta], [(Int, [Int])])
--   1) lista de preguntas
--   2) lista de modelos (pares (idModelo, reordenación))
data Test = Test [Pregunta] [(Int, [Int])]
  deriving (Show, Read, Eq)
--    Test tiene dos campos posicionales: 
--   - una lista de Pregunta
--   - una lista de tuplas (idModelo, [índices de reordenación]).

-- Getters para Test
preguntasTest :: Test -> [Pregunta]
preguntasTest (Test ps _) = ps
-- Devuelve la lista de preguntas del Test.

modelosTest :: Test -> [(Int, [Int])]
modelosTest (Test _ ms) = ms
-- Devuelve la lista de modelos (id, reordenación).


-- data Contestacion
-- Representa una pregunta contestada o en blanco
data Contestacion
  = Contestada Int  --  Contestada con un 'Int' que indica la respuesta elegida
  | EnBlanco        --  No se contestó la pregunta
  deriving (Show, Read, Eq)


-- data RespuestaTest(String, Int, [Contestacion])
--   1) idAlumno (nombre o DNI)
--   2) modelo (id de la reordenación)
--   3) lista de Contestaciones
data RespuestaTest = RespuestaTest String Int [Contestacion]
  deriving (Show, Read, Eq)
--  Un RespuestaTest asocia un alumno, un modelo de test y sus contestaciones.

-- Getters para RespuestaTest
idAlumnoRT :: RespuestaTest -> String
idAlumnoRT (RespuestaTest i _ _) = i
-- Devuelve el id del alumno (String).

modeloRT :: RespuestaTest -> Int
modeloRT (RespuestaTest _ m _) = m
-- Devuelve el id del modelo.

respuestasRT :: RespuestaTest -> [Contestacion]
respuestasRT (RespuestaTest _ _ rs) = rs
-- Devuelve la lista de contestaciones (Contestacion).


-- data Correcion(String, Double, Double)
--   1) idEstudiante
--   2) puntuacion (puntuación total del test)
--   3) nota (sobre 10)
data Correcion = Correccion String Double Double
  deriving (Show, Read, Eq)
--   Una 'Correcion' indica el resultado de corregir un test para un alumno:
--   (id, puntuación, notaEscalada).

-- Getters para Correcion
idEstudiante :: Correcion -> String
idEstudiante (Correccion i _ _) = i
--   Devuelve el id del estudiante.

puntuacion :: Correcion -> Double
puntuacion (Correccion _ p _) = p
--   Devuelve la puntuación total (aciertos - penalizaciones).

nota :: Correcion -> Double
nota (Correccion _ _ n) = n
--  Devuelve la nota, que es la puntuación escalada a 10.


-- data Estadisticas(Double, Double, Int, Int, Int, Int,
--                   [(Int,Int,Int)], [(Double,Double,Double)],
--                   Int, Int, Int, Int)
-- Es un tipo que agrupa diversos datos estadísticos 
-- sobre las respuestas de un conjunto de alumnos a un Test:
--   1) mediaPuntuacion
--   2) mediaRespondidas
--   3) numeroSuspensos
--   4) numeroAprobados
--   5) numeroNotables
--   6) numSobresalientes
--   7) frecuenciaAbsoluta (por cada pregunta: (#aciertos,#fallos,#enBlanco))
--   8) frecuenciaRelativa (por cada pregunta: (prop.aciertos, prop.fallos, prop.blanco))
--   9) mejorPregunta (índice con mayor # de aciertos)
--  10) peorPregunta  (índice con menor # de aciertos)
--  11) masBlanqueada (índice con mayor # de blancos)
--  12) menosBlanqueada (índice con menor # de blancos)
data Estadisticas = Estadisticas
    Double 
    Double
    Int
    Int
    Int
    Int
    [(Int, Int, Int)]
    [(Double, Double, Double)]
    Int
    Int
    Int
    Int
  deriving (Show, Read, Eq)

-- Getters para Estadisticas 
mediaPuntuacionE :: Estadisticas -> Double
mediaPuntuacionE (Estadisticas mp _ _ _ _ _ _ _ _ _ _ _) = mp

mediaRespondidasE :: Estadisticas -> Double
mediaRespondidasE (Estadisticas _ mr _ _ _ _ _ _ _ _ _ _) = mr

numeroSuspensosE :: Estadisticas -> Int
numeroSuspensosE (Estadisticas _ _ s _ _ _ _ _ _ _ _ _) = s

numeroAprobadosE :: Estadisticas -> Int
numeroAprobadosE (Estadisticas _ _ _ a _ _ _ _ _ _ _ _) = a

numeroNotablesE :: Estadisticas -> Int
numeroNotablesE (Estadisticas _ _ _ _ n _ _ _ _ _ _ _) = n

numSobresalientesE :: Estadisticas -> Int
numSobresalientesE (Estadisticas _ _ _ _ _ ns _ _ _ _ _ _) = ns

frecuenciaAbsolutaE :: Estadisticas -> [(Int, Int, Int)]
frecuenciaAbsolutaE (Estadisticas _ _ _ _ _ _ fa _ _ _ _ _) = fa

frecuenciaRelativaE :: Estadisticas -> [(Double, Double, Double)]
frecuenciaRelativaE (Estadisticas _ _ _ _ _ _ _ fr _ _ _ _) = fr

mejorPreguntaE :: Estadisticas -> Int
mejorPreguntaE (Estadisticas _ _ _ _ _ _ _ _ mp _ _ _) = mp

peorPreguntaE :: Estadisticas -> Int
peorPreguntaE (Estadisticas _ _ _ _ _ _ _ _ _ pp _ _) = pp

masBlanqueadaE :: Estadisticas -> Int
masBlanqueadaE (Estadisticas _ _ _ _ _ _ _ _ _ _ mb _) = mb

menosBlanqueadaE :: Estadisticas -> Int
menosBlanqueadaE (Estadisticas _ _ _ _ _ _ _ _ _ _ _ mbb) = mbb


-----------------------------------------------------------------------
-- Función principal corrige
-- Dado un Test y una RespuestaTest, construye un valor de tipo 'Correcion'
-- que contiene (idAlumno, puntuacionTotal, notaSobre10).
-----------------------------------------------------------------------
corrige :: Test -> RespuestaTest -> Correcion
corrige test rTest =
  let 
      -- Usamos la función buscaModelo para ver qué reordenación
      -- de preguntas corresponde al modelo de este alumno.
      reordenacion         = buscaModelo (modeloRT rTest) (modelosTest test)
      
      -- Obtenemos la lista de preguntas en el orden en que el alumno las ha visto.
      preguntasReordenadas = map (\i -> preguntasTest test !! i) reordenacion
      
      -- Calculamos la puntuación total a partir de la lista
      -- (pregunta, contestacion).
      total                = sumaPuntuaciones preguntasReordenadas (respuestasRT rTest)
      
      -- Calculamos la nota final: escalamos 'total' a un rango de [0..10].
      nPregs               = fromIntegral (length (preguntasTest test))
      notaFinal            = if nPregs == 0
                                then 0
                                else (total / nPregs) * 10
  in Correccion (idAlumnoRT rTest) total notaFinal
  --  Devolvemos Correccion idAlumno puntuacion nota.


-------------------------------------------------------------------------------
-- Funcion buscaModelo
-- Dado un id de modelo (Int) y una lista de (idModelo, reordenación),
-- devolvemos la reordenación de preguntas asociada a ese modelo.
-------------------------------------------------------------------------------
buscaModelo :: Int -> [(Int, [Int])] -> [Int]
buscaModelo _ [] = []
buscaModelo modelo ((m,ord):rs)
  | modelo == m = ord   -- Si coincide el idModelo con modelo,
  | otherwise   = buscaModelo modelo rs
  -- si no coincide, seguimos buscando en la lista restante.


-----------------------------------------------------------------------
-- Función auxiliar 'sumaPuntuaciones'
-- Dada la lista de preguntas ya en su orden y la lista de contestaciones,
-- sumamos la puntuación de cada pregunta.
-----------------------------------------------------------------------
sumaPuntuaciones :: [Pregunta] -> [Contestacion] -> Double
sumaPuntuaciones [] _          = 0
sumaPuntuaciones _  []         = 0
sumaPuntuaciones (p:ps) (c:cs) =
  puntuacionPregunta p c + sumaPuntuaciones ps cs
-- Si hay pregunta y contestación, calculamos 'puntuacionPregunta' y
-- luego seguimos recursivamente con el resto.


-----------------------------------------------------------------------
-- Función auxiliar: 'puntuacionPregunta'
-- Dada una Pregunta y una Contestacion:
--   - Si está en blanco, 0 puntos.
--   - Si acierta, añade 'valor p'.
--   - Si falla, resta valor p / (nPosibilidades - 1).
-----------------------------------------------------------------------
puntuacionPregunta :: Pregunta -> Contestacion -> Double
puntuacionPregunta p cont =
  case cont of
    EnBlanco     -> 0
    Contestada r ->
      if r == respuesta_correcta p
        then valor p
        else -(valor p / fromIntegral (numero_posibilidades p - 1))


-----------------------------------------------------------------------
-- Función estadisticas
-- Dado un Test y la lista de RespuestaTest de todos los alumnos:
--   - Calcula la media de puntuaciones, la media de preguntas respondidas...
--   - Calcula el número de suspensos, aprobados, notables y sobresalientes.
--   - Calcula las frecuencias de aciertos, errores, blancos por pregunta.
--   - Identifica cuál pregunta fue la mejor/peor/más/menos en blanco.
-----------------------------------------------------------------------
estadisticas :: Test -> [RespuestaTest] -> Estadisticas
estadisticas test rs =
  let 
      -- Obtenemos la corrección individual para cada alumno.
      correcs       = map (corrige test) rs
      
      -- De cada Correcion sacamos la puntuación para hacer la media.
      puntuacionesT = map puntuacion correcs
      
      -- Calculamos cuántas preguntas se han contestado
      -- (no están en blanco) en cada RespuestaTest.
      respondidas   = map (fromIntegral . length . filter (/= EnBlanco) . respuestasRT) rs
      
      -- Número total de alumnos (para dividir luego y sacar medias).
      totalAlumnos  = length rs
      
      -- Contamos suspensos, aprobados, notables, sobresalientes.
      suspensos = length (filter (\x -> nota x < 5) correcs)
      aprobados = length (filter (\x -> nota x >= 5 && nota x < 7) correcs)
      notables  = length (filter (\x -> nota x >= 7 && nota x < 9) correcs)
      sobres    = length (filter (\x -> nota x >= 9) correcs)

      -- Calculamos la media de puntuaciones.
      mediaPunt = if totalAlumnos == 0
                    then 0
                    else sum puntuacionesT / fromIntegral totalAlumnos

      -- Calculamos la media de preguntas respondidas (no en blanco).
      mediaResp = if totalAlumnos == 0
                    then 0
                    else sum respondidas / fromIntegral totalAlumnos

      -- Calculamos frecuencias absolutas (correctas, erróneas, blancas)
      frecAbs = calculaFrecuencias test rs

      -- Frecuencias relativas: porcentaje de correctas, erróneas y blancas
      -- en cada pregunta.
      frecRel = 
        let toRel x = if totalAlumnos == 0
                        then 0
                        else fromIntegral x / fromIntegral totalAlumnos
        in map (\(c,e,b) -> (toRel c, toRel e, toRel b)) frecAbs

      -- Determinamos qué pregunta es la mejor, peor, la más y menos en blanco.
      mejorPreg    = mejorPreguntaFrec frecAbs
      peorPreg     = peorPreguntaFrec frecAbs
      masBlanquead = preguntaMasEnBlanco frecAbs
      menosBlanc   = preguntaMenosEnBlanco frecAbs

  in Estadisticas
       mediaPunt
       mediaResp
       suspensos
       aprobados
       notables
       sobres
       frecAbs
       frecRel
       mejorPreg
       peorPreg
       masBlanquead
       menosBlanc


-----------------------------------------------------------------------
-- Función auxiliar: calculaFrecuencias
-- Devuelve una lista con las frecuencias (correctas, erróneas, enBlanco)
-- para cada pregunta del 'Test' en su orden original (base).
-----------------------------------------------------------------------
calculaFrecuencias :: Test -> [RespuestaTest] -> [(Int, Int, Int)]
calculaFrecuencias test rs =
  let 
      -- Número de preguntas en el Test.
      numPregs = length (preguntasTest test)

      -- inicial es una lista de (0,0,0) con la misma longitud que 'numPregs'.
      inicial  = replicate numPregs (0,0,0)
  in 
      -- Usamos foldl para recorrer todas las RespuestaTest y
      -- actualizar la tupla (correctas, erróneas, blancas) acumulada.
      foldl (acumulaFrecuencias test) inicial rs

-- acumulaFrecuencias:
-- - Para cada alumno, descubrimos la reordenación que usó.
-- - Obtenemos las preguntas reordenadas.
-- - Y combinamos con sus Contestaciones y la tupla acumulada para
--   recalcular (correctas, erróneas, blancas) con zipWith3.
acumulaFrecuencias :: Test -> [(Int, Int, Int)] -> RespuestaTest -> [(Int, Int, Int)]
acumulaFrecuencias test acc rTest =
  let 
      reord = buscaModelo (modeloRT rTest) (modelosTest test)
      pregReord = map (preguntasTest test !!) reord
  in zipWith3 cuentaFrecuencias pregReord (respuestasRT rTest) acc

-- cuentaFrecuencias:
--   Dado (Pregunta, Contestacion) y la tupla (c, e, b), actualizamos 
--   el número de correctas, erróneas o blancas.
cuentaFrecuencias :: Pregunta -> Contestacion -> (Int,Int,Int) -> (Int,Int,Int)
cuentaFrecuencias pregunta cont (c, e, b) =
  case cont of
    EnBlanco     -> (c, e, b+1)
    Contestada r ->
      if r == respuesta_correcta pregunta
        then (c+1, e, b)
        else (c, e+1, b)


-----------------------------------------------------------------------
-- Funciones auxiliares: mejorPregunta, peorPregunta, etc.
-- Cada una recorre la lista de tuplas (c,e,b) y localiza el índice 
-- correspondiente según un criterio (mayor acierto, menor acierto, etc.)
-----------------------------------------------------------------------
mejorPreguntaFrec :: [(Int, Int, Int)] -> Int
mejorPreguntaFrec [] = -1
mejorPreguntaFrec frecs =
  let (_, bestIdx) = foldl compararMejor (-1, 0) (zip frecs [0..])
  in bestIdx
  where
    compararMejor (maxVal, maxIdx) ((c,_,_), idx) =
      if c > maxVal then (c, idx) else (maxVal, maxIdx)

peorPreguntaFrec :: [(Int, Int, Int)] -> Int
peorPreguntaFrec [] = -1
peorPreguntaFrec frecs =
  let (_, worstIdx) = foldl compararPeor (maxBound, 0) (zip frecs [0..])
  in worstIdx
  where
    compararPeor (minVal, minIdx) ((c,_,_), idx) =
      if c < minVal then (c, idx) else (minVal, minIdx)

preguntaMasEnBlanco :: [(Int, Int, Int)] -> Int
preguntaMasEnBlanco [] = -1
preguntaMasEnBlanco frecs =
  let (_, mbIdx) = foldl compararMas (0, 0) (zip frecs [0..])
  in mbIdx
  where
    compararMas (maxVal, maxIdx) ((_,_,b), idx) =
      if b > maxVal then (b, idx) else (maxVal, maxIdx)

preguntaMenosEnBlanco :: [(Int, Int, Int)] -> Int
preguntaMenosEnBlanco [] = -1
preguntaMenosEnBlanco frecs =
  let (_, mbIdx) = foldl compararMenos (maxBound, -1) (zip frecs [0..])
  in mbIdx
  where
    compararMenos (minVal, minIdx) ((_,_,b), idx) =
      if b < minVal then (b, idx) else (minVal, minIdx)


-----------------------------------------------------------------------
-- Función main
-- 1. Lee un Test en una sola línea)
-- 2. Lee una lista de RespuestaTest en una sola línea (también posicional).
-- 3. Muestra las correcciones para cada alumno y las estadísticas globales.
-----------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Introduce un Test."
  putStrLn "Por ejemplo, en una sola línea:"
  putStrLn "  Test [Pregunta 3 1 1.0, Pregunta 4 4 0.5] [(1,[0,1]), (2,[1,0])]"
  testStr <- getLine
  let testLeido = read testStr :: Test
  putStrLn ("\nHas leído el siguiente Test:\n" ++ show testLeido)

  putStrLn "\nIntroduce una lista de RespuestaTest."
  putStrLn "Por ejemplo, en una sola línea:\n\
           \  [ RespuestaTest \"Juan\" 1 [Contestada 1, EnBlanco],\
           \    RespuestaTest \"Ana\" 1 [Contestada 1, Contestada 2] ]"
  respStr <- getLine
  let respuestasLeidas = read respStr :: [RespuestaTest]
  putStrLn ("\nHas leído las siguientes RespuestaTest:\n" ++ show respuestasLeidas)

  -- Calculamos la corrección individual de cada alumno:
  let correcciones = map (corrige testLeido) respuestasLeidas
  putStrLn "\n=== Correcciones individuales ==="
  mapM_ print correcciones

  -- Calculamos y mostramos las estadísticas globales:
  let estad = estadisticas testLeido respuestasLeidas
  putStrLn "\n=== Estadísticas globales ==="
  print estad

-----------------------------------------------------------------------
-- FIN DE LA PRÁCTICA
-----------------------------------------------------------------------
