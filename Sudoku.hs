module Main where
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Char
import Data.Set (fromList)

---------- Definir los tipos de datos globales ----------

-- MainMenu (No se requieren de mas parámetros para este menú)
-- GenMenu1 / Opción seleccionada (4 -> 4x4, 6 -> 6x6, 9 -> 9x9, 0 -> Ninguna)
-- GenMenu2 / Sudoku generado
-- CreatMenu1 / Opción seleccionada (4 -> 4x4, 6 -> 6x6, 9 -> 9x9, 0 -> Ninguna)
-- CreatMenu2 / Sudoku que está siendo creado / Celda seleccionada
-- SolveMenu / Tablero inicial (Para colorear celdas de gris) / Solución obtenida / Menú de retorno
-- PlayMenu / Tablero inicial (Celdas que no se pueden cambiar) / Tablero donde se juega / Celda seleccionada
--      / Tiempo transcurrido / Si el juego está activo (True) o terminado (False) / Menú de retorno
data GameState = MainMenu | GenMenu1 Int | GenMenu2 Sudoku | CreatMenu1 Int | CreatMenu2 Sudoku Selected
    | SolveMenu Sudoku (Maybe Sudoku) GameState | PlayMenu Sudoku Sudoku Selected Float Bool GameState

-- Nothing: No hay celda seleccionada
-- Just (x,y): Si se tiene seleccionada la celda en la pos (x,y).
type Selected = Maybe (Int, Int)

-- (Tamaño del Tablero, Tablero: lista con las n filas)
type Sudoku = (Int, [[Int]])

-- (Coordenada x, Coordenada y, Ancho, Alto)
type Bounds = (Float, Float, Float, Float)

---------- Creación de los botones ----------
-- Especificamos las dimensiones de cada botón.

button_genmen :: Bounds
button_genmen = (-350,-50,300,75)
button_creatmen :: Bounds
button_creatmen = (50,-50,300,75)
button_ret :: Bounds
button_ret = (300,-200,150,50)
button_4x4 :: Bounds
button_4x4 = (-400,50,200,75)
button_6x6 :: Bounds
button_6x6 = (-100,50,200,75)
button_9x9 :: Bounds
button_9x9 = (200,50,200,75)
button_gen :: Bounds
button_gen = (-150,-100,300,75)
button_new :: Bounds
button_new = (170,200,230,75)
button_solv :: Bounds
button_solv = (170,75,230,75)
button_play :: Bounds
button_play = (170,-50,230,75)
error_msg :: Bounds
error_msg = (-225,50,500,100)
sudo_grid :: Bounds
sudo_grid = (-470,270,540,540)

---------- Definir colores ----------
-- Definimos colores dados sus valores RGBA.

zongzi :: Color
zongzi = makeColor 0.7875 0.9 0.675 1 -- verde oscuro
juniper :: Color
juniper = makeColor 0.9 1 0.8 1 -- verde claro
smoliv :: Color
smoliv = makeColor 0.7 1 0.7 1 -- verde de checkear
gyoza :: Color
gyoza = makeColor 1 1 0.9 1 -- amarillo claro
sky :: Color
sky = makeColor 0.85 0.95 1 1 -- azul de verdad
flag :: Color
flag = makeColor 1 0.6 0.6 1 -- rojo
greay :: Color
greay = makeColor 0.85 0.85 0.85 1 -- grey? gray?

---------- Interfaz ----------

-- Método principal del programa que crea una ventana gráfica.
-- Se le especifica el tamaño, título, color de fondo, estado inicial y funciones
-- para pintar figuras en la pantalla, tratar eventos y transcurrir el tiempo.
main :: IO ()
main =
    playIO (InWindow "Sudoku" (1000, 600) (0,0)) gyoza 10 MainMenu
    printWorld handleEvents elapseTime

-- Método que devuelve una imagen para mostrar por pantalla dado un estado de juego.
printWorld :: GameState -> IO Picture
printWorld MainMenu = return $
    translate (-213.5) 50 (text "Sudoku") <>
    button button_genmen zongzi "Generar" 104 <>
    button button_creatmen zongzi "Crear" 118.5
printWorld (GenMenu1 n) = return $
    translate (-248.5) 150 (scale 0.5 0.5 $ text "Generar Sudoku") <>
    button button_4x4 (if n == 4 then zongzi else juniper) "4x4" 79 <>
    button button_6x6 (if n == 6 then zongzi else juniper) "6x6" 80 <>
    button button_9x9 (if n == 9 then zongzi else juniper) "9x9" 80 <>
    button button_gen zongzi "Generar" 104 <>
    button button_ret zongzi "Volver" 40
printWorld (CreatMenu1 n) = return $
    translate (-212) 150 (scale 0.5 0.5 $ text "Crear Sudoku") <>
    button button_4x4 (if n == 4 then zongzi else juniper) "4x4" 79 <>
    button button_6x6 (if n == 6 then zongzi else juniper) "6x6" 80 <>
    button button_9x9 (if n == 9 then zongzi else juniper) "9x9" 80 <>
    button button_gen zongzi "Crear" 118.5 <>
    button button_ret zongzi "Volver" 40
printWorld (GenMenu2 sudo) = return $
    paintSudoku sudo Nothing Nothing False <>
    button button_new zongzi "Otro tablero" 41.5 <>
    button button_solv zongzi "Resolver" 67 <>
    button button_play zongzi "Jugar nivel" 48.5 <>
    button button_ret zongzi "Volver" 40
printWorld (CreatMenu2 sudo sel) = return $
    paintSudoku sudo sel Nothing False <>
    button button_new zongzi "Nuevo tablero" 30 <>
    button button_solv zongzi "Resolver" 67 <>
    button button_play zongzi "Jugar nivel" 48.5 <>
    button button_ret zongzi "Volver" 40
printWorld (SolveMenu _ Nothing _) = return $
    button error_msg flag "No se ha encontrado resultado" 48 <>
    button button_ret zongzi "Volver" 40
printWorld (SolveMenu init (Just sudo) _) = return $
    paintSudoku sudo Nothing (Just init) False <>
    button button_ret zongzi "Volver" 40
printWorld (PlayMenu init sudo sel val run _) = return $
    paintSudoku sudo sel (Just init) (not run) <>
    button button_new zongzi ("Tiempo: " ++ show val ++ "s") 15 <>
    button button_solv zongzi "Comprobar" 49.5 <>
    button button_play zongzi "Reiniciar" 66 <>
    button button_ret zongzi "Volver" 40

-- Método que dado un evento y un estado de juego retorna otro.
handleEvents :: Event -> GameState -> IO GameState
handleEvents e MainMenu = mainMenuEvent e
handleEvents e (GenMenu1 n) = genMenu1Event e n
handleEvents e (CreatMenu1 n) = creatMenu1Event e n
handleEvents e (GenMenu2 sudo) = genMenu2Event e sudo
handleEvents e (CreatMenu2 sudo sel) = creatMenu2Event e sudo sel
handleEvents e (SolveMenu init sudo ret) = solveMenuEvent e init sudo ret
handleEvents e (PlayMenu init sudo sel time run ret) = playMenuEvent e init sudo sel time run ret

-- Método que por cada frame se actualiza el tiempo de juego transcurrido.
elapseTime :: Float -> GameState -> IO GameState
elapseTime inc (PlayMenu init sudo sel time True ret) = return $ PlayMenu init sudo sel (incTime time inc) True ret
elapseTime _ s = return s

-- POST: Tiempo tras cada frame, con solo 1 decimal.
incTime :: Float -> Float -> Float
incTime time inc = (/10) $ fromIntegral $ round $ (10*) $ time + inc

-- Método que trata los eventos manejados del menú principal.
mainMenuEvent :: Event -> IO GameState
mainMenuEvent (EventKey (MouseButton LeftButton) Up _ (x,y))
    | inBounds (x,y) button_genmen = genMenu1 0
    | inBounds (x,y) button_creatmen = creatMenu1 0
    | otherwise = return MainMenu
mainMenuEvent _ = return MainMenu

-- Método que trata los eventos manejados del menú de generar.
genMenu1Event :: Event -> Int -> IO GameState
genMenu1Event (EventKey (MouseButton LeftButton) Up _ (x,y)) n
    | inBounds (x,y) button_4x4 = genMenu1 4
    | inBounds (x,y) button_6x6 = genMenu1 6
    | inBounds (x,y) button_9x9 = genMenu1 9
    | inBounds (x,y) button_gen && n /= 0 = genMenu2 n
    | inBounds (x,y) button_ret = return MainMenu
    | otherwise = genMenu1 n
genMenu1Event _ n = genMenu1 n

-- Método que trata los eventos manejados del menú de crear.
creatMenu1Event :: Event -> Int -> IO GameState
creatMenu1Event (EventKey (MouseButton LeftButton) Up _ (x,y)) n
    | inBounds (x,y) button_4x4 = creatMenu1 4
    | inBounds (x,y) button_6x6 = creatMenu1 6
    | inBounds (x,y) button_9x9 = creatMenu1 9
    | inBounds (x,y) button_gen && n /= 0 = creatMenu2 n
    | inBounds (x,y) button_ret = return MainMenu
    | otherwise = creatMenu1 n
creatMenu1Event _ n = creatMenu1 n

-- Método que trata los eventos manejados del menú de generar 2.
genMenu2Event :: Event -> Sudoku -> IO GameState
genMenu2Event (EventKey (MouseButton LeftButton) Up _ (x,y)) (n, sudo)
    | inBounds (x,y) button_new = genMenu2 n
    | inBounds (x,y) button_solv = solvMenu (n, sudo) $ GenMenu2 (n, sudo)
    | inBounds (x,y) button_play = playMenu (n, sudo) False $ GenMenu2 (n, sudo)
    | inBounds (x,y) button_ret = genMenu1 n
    | otherwise = return $ GenMenu2 (n, sudo)
genMenu2Event _ sudo = return $ GenMenu2 sudo

-- Método que trata los eventos manejados del menú de crear 2.
creatMenu2Event :: Event -> Sudoku -> Selected -> IO GameState
creatMenu2Event (EventKey (MouseButton LeftButton) Up _ (x,y)) (n, sudo) sel
    | inBounds (x,y) button_new = creatMenu2 n
    | inBounds (x,y) button_solv = solvMenu (n, sudo) $ CreatMenu2 (n, sudo) Nothing
    | inBounds (x,y) button_play = playMenu (n, sudo) True $ CreatMenu2 (n, sudo) Nothing
    | inBounds (x,y) button_ret = creatMenu1 n
    | inBounds (x,y) sudo_grid = return $ CreatMenu2 (n, sudo) $ getClickedCell (n, sudo) True (x,y)
    | otherwise = return $ CreatMenu2 (n, sudo) Nothing
creatMenu2Event (EventKey (Char c) Down _ (_,_)) sudo sel = return $ CreatMenu2 (setValue c sel sudo) sel
creatMenu2Event (EventKey (SpecialKey KeyBackspace) Down _ (_,_)) sudo sel = return $ CreatMenu2 (setValue '0' sel sudo) sel
creatMenu2Event (EventKey (SpecialKey KeyDelete) Down _ (_,_)) sudo sel = return $ CreatMenu2 (setValue '0' sel sudo) sel
creatMenu2Event _ sudo sel = return $ CreatMenu2 sudo sel

-- Método que trata los eventos manejados de la interfaz de Resolver.
solveMenuEvent :: Event -> Sudoku -> Maybe Sudoku -> GameState -> IO GameState
solveMenuEvent (EventKey (MouseButton LeftButton) Up _ (x,y)) init sudo ret
    | inBounds (x,y) button_ret = return ret
    | otherwise = return $ SolveMenu init sudo ret
solveMenuEvent _ init sudo ret = return $ SolveMenu init sudo ret

-- Método que trata los eventos manejados de la interfaz de Jugar.
playMenuEvent :: Event -> Sudoku -> Sudoku -> Selected -> Float -> Bool -> GameState -> IO GameState
playMenuEvent (EventKey (MouseButton LeftButton) Up _ (x,y)) init sudo sel time run ret
    | inBounds (x,y) button_solv = return $ PlayMenu init sudo Nothing time False ret
    | inBounds (x,y) button_play = return $ PlayMenu init init Nothing 0 True ret
    | inBounds (x,y) button_ret = return ret
    | inBounds (x,y) sudo_grid && run = return $ PlayMenu init sudo (getClickedCell init False (x,y)) time run ret
    | otherwise = return $ PlayMenu init sudo sel time run ret
playMenuEvent (EventKey (Char c) Down _ (_,_)) init sudo sel time True ret =
    return $ PlayMenu init (setValue c sel sudo) sel time True ret
playMenuEvent (EventKey (SpecialKey KeyBackspace) Down _ (_,_)) init sudo sel time True ret =
    return $ PlayMenu init (setValue '0' sel sudo) sel time True ret
playMenuEvent (EventKey (SpecialKey KeyDelete) Down _ (_,_)) init sudo sel time True ret =
    return $ PlayMenu init (setValue '0' sel sudo) sel time True ret
playMenuEvent _ init sudo sel time run ret = return $ PlayMenu init sudo sel time run ret

---------- Auxiliares de Interfaz ----------

-- POST: Devolver un estado de GenMenu1 nuevo.
genMenu1 :: Int -> IO GameState
genMenu1 n = return $ GenMenu1 n

-- POST: Devolver un estado de CreatMenu1 nuevo.
creatMenu1 :: Int -> IO GameState
creatMenu1 n = return $ CreatMenu1 n

-- POST: Devolver un estado de genMenu2 nuevo.
genMenu2 :: Int -> IO GameState
genMenu2 n = do { sudo <- generate n ; return $ GenMenu2 sudo }

-- POST: Devolver un estado de creatMenu2 nuevo.
creatMenu2 :: Int -> IO GameState
creatMenu2 n = return $ CreatMenu2 (create n) Nothing

-- POST: Devolver un estado de solvMenu nuevo.
solvMenu :: Sudoku -> GameState -> IO GameState
solvMenu sudo back = return $ SolveMenu sudo (solve sudo) back

-- POST: Devolver un estado de playMenu nuevo.
playMenu :: Sudoku -> Bool -> GameState -> IO GameState
playMenu sudo False back = return $ PlayMenu sudo sudo Nothing 0 True back
playMenu sudo True back = case solve sudo of
    Nothing -> return $ SolveMenu sudo Nothing back
    _ -> playMenu sudo False back

-- POST: Crea un rectángulo dado unas coordenadas de origen y el ancho y alto, al que se le añade un texto,
--       dándole un desplazamiento horizontal y una escala para que se ajuste al tamaño del rectángulo.
rectangle :: Bounds -> Color -> String -> Float -> Float ->  Picture
rectangle (x,y,w,h) col txt off scl = let size = 0.2 * scl in
    color col (polygon [(x,y),(x+w,y),(x+w,y-h),(x,y-h)]) <>
    color black (line [(x,y),(x+w,y),(x+w,y-h),(x,y-h),(x,y)]) <>
    translate (x+off) (y-h/2) (scale size size $ translate 0 (-50) $ text txt)

-- POST: Crea rectángulos con el color especificado y con texto en el interior.
button :: Bounds -> Color -> String -> Float -> Picture  --límite del rectángulo, color, texto, offset (para centrar)
button bnds col txt off = rectangle bnds col txt off 1

-- POST: True si la zona clickeada está dentro de los bounds del botón.
inBounds :: (Float, Float) -> Bounds -> Bool
inBounds (a,b) (x,y,w,h) = a >= x && b <= y && a <= x+w && b >= y-h

-- POST: Devuelve una imagen (Picture) con el tablero Sudoku.
paintSudoku :: Sudoku -> Selected -> Maybe Sudoku -> Bool -> Picture
paintSudoku (n, sudo) sel init done = paintSudokuField n sudo $ colorSudoku (n, sudo) sel init done

-- POST: Determina el color de cada casilla del Sudoku.
colorSudoku :: Sudoku -> Selected -> Maybe Sudoku -> Bool -> [[Color]]
colorSudoku sudo sel init done = flip colorInit init $ flip colorSelc sel $ colorSolu sudo done

-- POST: Matriz nxn con el color de cada casilla del tablero Sudoku.
-- NOTA: Blanco si no tiene valor, rojo si está mal y verde en caso contrario.
colorSolu :: Sudoku -> Bool -> [[Color]]
colorSolu (n, _) False = createMat n white
colorSolu (n, sudo) True = let grps = getGrpsMat n sudo in
    map (map (\(r,c) -> colorCell (r,c) (sudo !! r) (map (!! c) sudo) (grps !! pos2grp (r,c) n)))
        [[(r,c) | c <- [0..n-1]] | r <- [0..n-1]]

-- POST: Determina el color que tiene que tener cada casilla.
-- NOTA: Blanco si no tiene valor, rojo si está mal y verde en caso contrario.
colorCell :: (Int, Int) -> [Int] -> [Int] -> [Int] -> Color
colorCell (r,c) row col grp = let val = getValArr c row in
    if val == 0 then white
        else if colorCheck val row && colorCheck val col && colorCheck val grp
            then smoliv else flag

-- POST: True si el valor de la celda es correcta (no se repite ni en fila ni columna ni región).
colorCheck :: Int -> [Int] -> Bool
colorCheck n arr = foldl (\total val -> if val == n then total + 1 else total) 0 arr == 1

-- POST: Colorea la casilla selecionada de color azul.
colorSelc :: [[Color]] -> Selected -> [[Color]]
colorSelc col Nothing = col
colorSelc col (Just (x,y)) = setValMat sky (y,x) col

-- POST: Colorea las casillas iniciales de color gris.
colorInit :: [[Color]] -> Maybe Sudoku -> [[Color]]
colorInit col Nothing = col
colorInit col (Just (_, sudo)) =
    zipWith (zipWith (\c n -> if n == 0 then c else greay)) col sudo

-- POST: Devuelve la imagen del tablero dado los valores de las celdas y sus colores.
paintSudokuField :: Int -> [[Int]] -> [[Color]] -> Picture
paintSudokuField s sudo cols = foldr (<>) blank $
    map (\(x,y) -> paintSudokuCell s (x,y) (getValMat (y,x) sudo) (getValMat (y,x) cols))
        [(x,y) | x <- [0..s-1], y <- [0..s-1]]

-- PRE: Tamaño, posición, valor y color.
-- POST: Devuelve la imagen de una celda del tablero dado su valor y color.
paintSudokuCell :: Int -> (Int, Int) -> Int -> Color -> Picture
paintSudokuCell s pos val col = rectangle (getCellBounds s pos) col
    (if val == 0 then "" else show val) (200.0 / fromIntegral s) (9.0 / fromIntegral s)

-- POST: Devuelve los bounds de una celda dado la posición de la celda y el tamaño del tablero.
-- NOTA: A mayor dimensión del tablero, menor tamaño de las celdas.
getCellBounds :: Int -> (Int, Int) -> Bounds
getCellBounds n (x,y) = let (r,c) = size2dim n ; s = (540 `div` n) in (
    fromIntegral (-470+x*s + (x `div` r - 1) * 3),
    fromIntegral (270-y*s - (y `div` c - 1) * 3),
    fromIntegral s, fromIntegral s)

-- POST: Devuelve los bounds de una celda dados la posición de la celda y el tamaño del tablero.
-- NOTA: Con el valor Bool a True, puedes selecionar cualquier casilla, si no, solo las que no son pistas.
getClickedCell :: Sudoku -> Bool -> (Float, Float) -> Selected
getClickedCell (n, sudo) change (x,y) = let (c,r) = (round (x+470) * n `div` 540, round (270-y) * n `div` 540) in
    if change || 0 == getValMat (r,c) sudo then Just (c,r) else Nothing

-- POST: Pone en la casilla seleccionada el valor introducido al Sudoku.
-- NOTA: Se pasa de Char a Int utilizando código ASCII.
setValue :: Char -> Selected -> Sudoku -> Sudoku
setValue _ Nothing sudo = sudo
setValue c (Just (x,y)) (n, sudo) = let val = ord c - 48 in
    (n, if val >= 0 && val <= n then setValMat val (y,x) sudo else sudo)

---------- Métodos genéricos ----------

-- PRE: Un elemento y una lista del tipo del elento, cuyo tipo derive Equals.
-- POST: True si el elemento está en la lista.
member :: (Eq a) => a -> [a] -> Bool
member _ [] = False
member x (y:r) = x == y || member x r

-- PRE: Lista de un tipo que derive Equals.
-- POST: Devuelve la lista sin duplicados.
removeDups :: (Eq a) => [a] -> [a]
removeDups [] = []
removeDups (x:xr)
    | member x xr = removeDups xr
    | otherwise = x : removeDups xr

-- PRE: Número positivo n y elemento x.
-- POST: Crea una matriz nxn rellenado con el elemento x.
createMat :: Int -> a -> [[a]]
createMat n x = createArr n (createArr n x)

-- PRE: Número positivo n y elemento x.
-- POST: Crea una lista con el elemento x repetido n-veces.
createArr :: Int -> a -> [a]
createArr n x = [x | _ <- [1..n]]

-- PRE: Posición (x,y) dentro del rango de la matriz.
-- POST: Cambia el valor de la posición (x,y) de la matriz por n.
setValMat :: a -> (Int, Int) -> [[a]] -> [[a]]
setValMat n (x,y) m = setValArr (setValArr n y (getValArr x m)) x m

-- PRE: Posición i menor que la longitud de la lista.
-- POST: Cambia el valor de la posición i de la lista por n.
setValArr :: a -> Int -> [a] -> [a]
setValArr n 0 (x:r) = n : r
setValArr n i (x:r) = x : setValArr n (i-1) r

-- PRE: Posición (x,y) dentro del rango de la matriz.
-- POST: Obtiene el valor de la posición (x,y) de la matriz.
getValMat :: (Int, Int) -> [[a]] -> a
getValMat (x,y) m = getValArr y (getValArr x m)

-- PRE: Posición i menor que la longitud de la lista.
-- POST: Obtiene el valor de la posición i de la lista.
getValArr :: Int -> [a] -> a
getValArr 0 (x:r) = x
getValArr i (x:r) = getValArr (i-1) r

-- PRE: Lista de pares de elementos e y posición (x,y) dentro del rango de la matriz.
-- POST: Cambia el valor de la posición (x,y) de la matriz por e, para todos los elementos dados.
setArrMat :: [(a, (Int, Int))] -> [[a]] -> [[a]]
setArrMat [] mat = mat
setArrMat ((e,p):r) mat = setArrMat r $ setValMat e p mat

-- PRE: Número positivo n, que es la dimensión de la matriz m.
-- POST: Devuelve una lista de listas con los elementos que pertenecen a cada grupo.
getGrpsMat :: Int -> [[a]] -> [[a]]
getGrpsMat n mat = let (r,c) = size2dim n in
    map (\g -> let (gr,gc) = grp2pos g n in concatMap (!! gc) (fracture c (map (fracture r) mat) !! gr)) [0..n-1]

-- PRE: Número positivo n.
-- POST: Parte la lista dada en sublistas de n elementos cada una y devuelve una lista que las contiene.
fracture :: Int -> [a] -> [[a]]
fracture _ [] = []
fracture n list = take n list : fracture n (drop n list)

-- PRE: Dimensión del tablero.
-- POST: Devuelve la dimensión de las regiones de cada tablero.
size2dim :: Int -> (Int, Int)
size2dim 4 = (2,2)
size2dim 6 = (3,2)
size2dim 9 = (3,3)

-- PRE: Celda (y=fila, x=columna) y dimensión del tablero.
-- POST: Devuelve el grupo, de 0 hasta (n-1), en el que se encuentra la celda.
pos2grp :: (Int, Int) -> Int -> Int
pos2grp (y,x) size = let (r,c) = size2dim size in -- r: nº de filas, c: nº de columnas
    x `div` r + y `div` c * c

-- PRE: Número del grupo y tamaño del tablero.
-- POST: Devuelve la coordenada relativa de la región.
grp2pos :: Int -> Int -> (Int, Int)
grp2pos grp size = let (_,c) = size2dim size in (grp `div` c, grp `mod` c)

---------- Creator ----------

-- PRE: Dimensión del Sudoku.
-- POST: Crea un tablero de la dimensión dada inicializado a 0's.
create :: Int -> Sudoku
create n = (n, createMat n 0)

---------- Solver ----------

-- Solver / Dimensión / Tablero de posibles valores que puede tomar las celdas
--      / Matriz de posibles combinaciones de las celdas de cada fila
--      / Matriz de posibles combinaciones de las celdas de cada columna
--      / Matriz de posibles combinaciones de las celdas de cada grupo
data Solver = Solver Int [[[Int]]] [[[Int]]] [[[Int]]] [[[Int]]]

-- PRE: Sudoku de dimensón n a resolver.
-- POST: Devuelve una solución (genérica si tiene múltiples) o Nothing si no existe.
-- NOTA: Crea un objeto Solver con tablero de posibilidades inicialmente a la lista [1..n] para
--       celdas sin valor inicial, y la lista [val] para celdas con val como valor inicial,
--       y todas las posibles combinaciones de todas las filas, columnas y grupos.
solve :: Sudoku -> Maybe Sudoku
solve (n, sudo) = do
    let field = map (map (\val -> if val == 0 then [1..n] else [val])) sudo
    let combs = [[solCombs (solGetModel y x n field) | x <- [0..n-1]] | y <- [1..3]] -- y=1 (filas), y=2 (columnas), y=3 (regiones)
    solStart $ Solver n field (combs !! 0) (combs !! 1) (combs !! 2)

-- PRE: Lista de posibles valores de un conjunto de celdas (combinaciones).
-- POST: Devuelve todas las posibles combinaciones de valores de las celdas dadas.
solCombs :: [[Int]] -> [[Int]]
solCombs = flip solCombsAux []

-- Auxiliar de solCombs con parámetro de acumulación.
-- PRE: Lista parámetro de acumulación inicialmente vacía.
solCombsAux :: [[Int]] -> [Int] -> [[Int]]
solCombsAux [] comb = [comb]
solCombsAux ([]:rows) comb = []
solCombsAux ((val:row):rows) comb
    | member val comb = solCombsAux (row:rows) comb
    | otherwise = solCombsAux rows (comb ++ [val]) ++ solCombsAux (row:rows) comb

-- PRE: Un objeto Solver nuevo.
-- POST: La solución del Sudoku, o Nothing si no tiene.
-- NOTA: Comienza realizando la primera iteración que es especial.
solStart :: Solver -> Maybe Sudoku
solStart s1 = do
    (_, s2) <- solSyncField 1 0 False s1
    (_, s3) <- solSyncField 2 0 False s2
    (_, s4) <- solSyncField 3 0 False s3
    solLoop s4

-- PRE: Un objeto Solver. 
-- POST: La solución del Sudoku, o Nothing si no tiene.
-- NOTA: Itera sincronzando el tablero de posibilidades con las matrices de combinaciones:
--       Reduce las posibilidades de cada celda y elimina posibles combinaciones a cada iteración.
--       Si no se realizan cambios en una iteración, retorna el resultado obtenido hasta el momento.
solLoop :: Solver -> Maybe Sudoku
solLoop s1 = do
    (e1, s2) <- solSyncAll 1 s1
    (e2, s3) <- solSyncAll 2 s2
    (e3, s4) <- solSyncAll 3 s3
    if e1 || e2 || e3 then solLoop s4 else return $ solRetSudo s4

-- PRE: Un objeto Solver.
-- POST: Transforma su tablero de posibilidades en un Sudoku.
-- NOTA: Si la celda tiene una sola posibilidad, se asigna ese valor en el sudoku, si no se asigna 0.
solRetSudo :: Solver -> Sudoku
solRetSudo (Solver n field _ _ _) =
    (n, map (map (\cell -> if length cell == 1 then head cell else 0)) field)

-- PRE: Tipo de conjunto a sincronizar y objeto Solver.
-- POST: Nothing si ha determinado que no existe solución, o un par con un Bool que
--       especifica si ha habido cambios y el objeto Solver actualizado si los hubo.
solSyncAll :: Int -> Solver -> Maybe (Bool, Solver)
solSyncAll t s1 = do
    let (edit, s2) = solSyncModel t 0 False s1
    solSyncField t 0 edit s2

-- PRE: Tipo de conjunto a sincronizar, dos parámetros de acumulación (0 y False) y objeto Solver.
-- POST: Un par con un Bool que especifica si ha habido cambios y el objeto Solver actualizado si los hubo.
-- NOTA: La sincronización es del tipo de descarte de combinaciones inválidas.
solSyncModel :: Int -> Int -> Bool -> Solver -> (Bool, Solver)
solSyncModel t x edit (Solver n field rows cols grps) = if x == n
    then (edit, Solver n field rows cols grps)
    else let
        model = solGetModel t x n field
        old = solGetCombs t x (Solver n field rows cols grps)
        new = solSync old model
        redit = edit || length old /= length new
        newSol = solSetCombs t x new $ Solver n field rows cols grps
        in solSyncModel t (x+1) redit newSol

-- PRE: Tipo de conjunto a sincronizar, dos parámetros de acumulación (0 y False) y objeto Solver.
-- POST: Nothing si ha determinado que no existe solución, o un par con un Bool que
--       especifica si ha habido cambios y el objeto Solver actualizado si los hubo.
-- NOTA: La sincronización es del tipo de reducción de posibles valores de la tabla de posibilidades.
-- NOTA: Se determina que no hay solución cuando solModel retorna Nothing debido a que no existen
--       posibles combinaciones de valores para las celdas de algún conjunto.
solSyncField :: Int -> Int -> Bool -> Solver -> Maybe (Bool, Solver)
solSyncField t x edit (Solver n field rows cols grps) = if x == n
    then Just (edit, Solver n field rows cols grps)
    else do
        let oldModel = solGetModel t x n field
        newModel <- solModel n $ solGetCombs t x $ Solver n field rows cols grps
        let redit = edit || map fromList oldModel /= map fromList newModel
        let newField = solSetModel t x n newModel field
        solSyncField t (x+1) redit $ Solver n newField rows cols grps

-- PRE: Lista de combinaciones de valores de un conjunto y modelo de las celdas del conjunto.
-- POST: Lista de combinaciones reducida tras descartar las combinaciones inválidas que no siguen el modelo.
solSync :: [[Int]] -> [[Int]] -> [[Int]]
solSync [] _ = []
solSync (comb:combs) model
    | solMatch comb model = comb : solSync combs model
    | otherwise = solSync combs model

-- PRE: Combinación de valores de un conjunto y modelo de las celdas del conjunto.
-- POST: Determina si la combinación sigue o no el modelo (es válida o no).
solMatch :: [Int] -> [[Int]] -> Bool
solMatch [] [] = True
solMatch (x:xr) (c:cr) = member x c && solMatch xr cr

-- PRE: Tamaño del tablero y lista de combinaciones de valores de un conjunto.
-- POST: Devuelve el modelo determinado por las combinaciones, o Nothing si no hay combinaciones.
solModel :: Int -> [[Int]] -> Maybe [[Int]]
solModel 0 combs = if null combs then Nothing else Just []
solModel n combs = solModel (n-1) combs >>= (\l -> return $ l ++ [removeDups $ map (!! (n-1)) combs])

-- PRE: Tipo de conjunto (1 -> fila, 2 -> columna, 3 -> grupo), número del conjunto y objeto Solver.
-- POST: Lista de combinaciones de las celdas del conjunto especificado.
solGetCombs :: Int -> Int -> Solver -> [[Int]]
solGetCombs 1 row (Solver _ _ rows _ _) = rows !! row
solGetCombs 2 col (Solver _ _ _ cols _) = cols !! col
solGetCombs 3 grp (Solver _ _ _ _ grps) = grps !! grp

-- PRE: Tipo de conjunto (1 -> fila, 2 -> columna, 3 -> grupo), número del conjunto,
--      lista de combinaciones de las celdas del conjunto especificado, y objeto Solver.
-- POST: Objeto Solver actualizado tras añadirle la lista de combinaciones dada.
solSetCombs :: Int -> Int -> [[Int]] -> Solver -> Solver
solSetCombs 1 row combs (Solver n field rows cols grps) = Solver n field (setValArr combs row rows) cols grps
solSetCombs 2 col combs (Solver n field rows cols grps) = Solver n field rows (setValArr combs col cols) grps
solSetCombs 3 grp combs (Solver n field rows cols grps) = Solver n field rows cols (setValArr combs grp grps)

-- PRE: Tipo de conjunto (1 -> fila, 2 -> columna, 3 -> grupo), número del conjunto,
--      dimensión del tablero y tabla de posibilidades.
-- POST: Lista de posibilidades (modelo) de las celdas del conjunto especificado.
solGetModel :: Int -> Int -> Int -> [[[Int]]] -> [[Int]]
solGetModel 1 row _ field = field !! row
solGetModel 2 col _ field = map (!! col) field
solGetModel 3 grp n field = getGrpsMat n field !! grp

-- PRE: Tipo de conjunto (1 -> fila, 2 -> columna, 3 -> grupo), número del conjunto, dimensión del tablero,
--      lista de posibilidades (modelo) de las celdas del conjunto especificado y tabla de posibilidades.
-- POST: Tabla de posibilidades actualizada tras añadirla la lista de posibilidades.
solSetModel :: Int -> Int -> Int -> [[Int]] -> [[[Int]]] -> [[[Int]]]
solSetModel 1 row n model field = setArrMat (zip model [(row,col) | col <- [0..n-1]]) field
solSetModel 2 col n model field = setArrMat (zip model [(row,col) | row <- [0..n-1]]) field
solSetModel 3 grp n model field = let (r,c) = grp2pos grp n; (w,h) = size2dim n in -- (3,2)
    setArrMat (zip model [(row+r*h,col+c*w) | row <- [0..h-1], col <- [0..w-1]]) field

---------- Generator ----------

-- Gen / Dimensión / Tablero (inicialmente a 0's)
--      / Valores que faltan por añadir (inicialmente n 1's, n 2's... n n's)
--      / Matriz de fila/valor que representa si el valor está introducido en la fila
--      / Matriz de columna/valor que representa si el valor está introducido en la columna
--      / Matriz de grupo/valor que representa si el valor está introducido en el grupo
data Generator = Gen Int [[Int]] [Int] [[Bool]] [[Bool]] [[Bool]]

-- PRE: Dimensión del Sudoku.
-- POST: Genera un tablero de la dimensión dada y devuelve un Sudoku con un ~40% de pistas.
-- NOTA: Crea un objeto Generator con tablero inicialmente a 0, matrices booleanas inicialmente
--       a False, y los valores que faltan inicialmente a n 1's, n 2's,... n n's.
generate :: Int -> IO Sudoku
generate n = let n2 = fromIntegral $ n * n in do
    sudo <- genField $ Gen n (createMat n 0) [1 + (x-1) `div` n | x <- [1..n*n]]
        (createMat n False) (createMat n False) (createMat n False)
    holes <- randomRIO (ceiling $ 0.55 * n2, ceiling $ 0.65 * n2)
    genHoles sudo holes

-- PRE: Un objeto Generator nuevo.
-- POST: Intenta crear un Sudoku. Si este falla, lo reintenta, si no, devuelve el Sudoku generado.
genField :: Generator -> IO Sudoku
genField g = do
    mbsudo <- genLoop g
    case mbsudo of
        Nothing -> genField g
        Just sudo -> return sudo

-- PRE: Sudoku y número de pistas a quitar.
-- POST: Sudoku con n agujeros.
genHoles :: Sudoku -> Int -> IO Sudoku
genHoles sudo 0 = return sudo
genHoles (n, sudo) k = do
    x <- randomRIO (0, n-1)
    y <- randomRIO (0, n-1)
    if 0 /= getValMat (x,y) sudo
        then genHoles (n, setValMat 0 (x,y) sudo) (k-1)
        else genHoles (n, sudo) k

-- PRE: Un objeto Generator. 
-- POST: El tablero generado, o Nothing si no se ha podido generar un tablero completo.
-- NOTA: Itera añadiendo valores al tablero.
genLoop :: Generator -> IO (Maybe Sudoku)
genLoop (Gen n sudo [] _ _ _) = return $ Just (n, sudo)
genLoop (Gen n sudo vals rows cols grps) = do
    mbpos <- genFindPos (Gen n sudo vals rows cols grps)
    case mbpos of
        Nothing -> return Nothing
        Just pos -> genLoop $ genSetValue pos (Gen n sudo vals rows cols grps)

-- PRE: Un objeto Generator.
-- POST: La posición donde introducir el próximo valor al tablero, o Nothing si no se puede introducir.
-- NOTA: Recorre aleatoriamente las casillas del tablero hasta dar con una válida.
genFindPos :: Generator -> IO (Maybe (Int, Int))
genFindPos (Gen n sudo vals rows cols grps) = do
    randRow <- randomArr n
    randCol <- randomArr n
    return $ genSelectCell (Gen n sudo vals rows cols grps) randRow randCol

-- PRE: Un objeto generador y 2 listas que especifican el orden a recorrer las celdas
--      (La primera es el orden a recorrer filas y la segunda el orden a recorrer columnas).
-- POST: La primera celda donde se puede introducir el siguiente valor, o Nothing si no se puede.
genSelectCell :: Generator -> [Int] -> [Int] -> Maybe (Int, Int)
genSelectCell (Gen n sudo (val:_) rows cols grps) randRow randCol = do
    row <- genSelectRow val randRow rows
    col <- genSelectCol val row randCol sudo cols grps n
    return (row, col)

-- PRE: El valor a introducir, una lista especificando el orden a recorrer filas y la matriz de fila/valor.
-- POST: La primera fila encontrada donde no se haya introducido aún el valor.
genSelectRow :: Int -> [Int] -> [[Bool]] -> Maybe Int
genSelectRow _ [] _ = Nothing
genSelectRow x (row:r) rows = if getValMat (row, x-1) rows
    then genSelectRow x r rows else Just row

-- PRE: El valor a introducir, la fila seleccionada para introducir el valor, una lista especificando
--      el orden a recorrer columnas, el tablero, la matriz de columna/valor y la matriz de grupo/valor.
-- POST: La primera columna encontrada donde no se haya introducido aún el valor, de tal forma que
--       la celda determinada no esté ocupada y en el grupo no se haya introducido aún el valor.
genSelectCol :: Int -> Int -> [Int] -> [[Int]] -> [[Bool]] -> [[Bool]] -> Int -> Maybe Int
genSelectCol _ _ [] _ _ _ _ = Nothing
genSelectCol x row (col:r) field cols grps n =
    if 0 /= getValMat (row, col) field || getValMat (col, x-1) cols || getValMat (pos2grp (row, col) n, x-1) grps
        then genSelectCol x row r field cols grps n else Just col

-- PRE: Posición donde introducir el siguiente valor y estado actual del Generator.
-- POST: Nuevo estado del Generator tras añadir el siguiente valor al tablero en la posición dada.
genSetValue :: (Int, Int) -> Generator -> Generator
genSetValue (r,c) (Gen n sudo (x:xr) rows cols grps) = Gen n (setValMat x (r,c) sudo) xr
    (setValMat True (r,x-1) rows) (setValMat True (c,x-1) cols) (setValMat True (pos2grp (r,c) n,x-1) grps)

-- PRE: Número natural positivo n.
-- POST: Genera una lista con los números del 0 al n-1 ordenados aleatoriamente.
randomArr :: Int -> IO [Int]
randomArr n = reorder [0..n-1]

-- PRE: Lista de enteros.
-- POST: Reordena aleatoriamente los elementos de la lista.
reorder :: [Int] -> IO [Int]
reorder [] = return []
reorder l = do { rng <- randomRIO (0, length l - 1); lr <- reorder (remove rng l); return (l !! rng : lr) }

-- PRE: Lista de enteros y número natural n menor que la longitud de la lista.
-- POST: Elimina el elemento en la posición n de la lista.
remove :: Int -> [Int] -> [Int]
remove 0 (_:xr) = xr
remove n (x:xr) = x:remove (n-1) xr
