module Practica04 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

type Literal = Prop
type Clausula = [Literal]

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

--Definicion de los tipos para la practica
type Interpretacion = [( String , Bool ) ]
type Estado = ( Interpretacion , [Clausula])
data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void deriving Show

-- IMPLEMENTACION PARTE 1

--revisa si es la clausula unitaria
esUnit :: Clausula -> Bool
esUnit [_] = True
esUnit _   = False

-- sacamos el valor de la literal
obtenerLiteral :: Clausula -> Literal
obtenerLiteral [x] = x
obtenerLiteral _   = Cons False 

-- Extrae el String de una variable
obtenerString :: Literal -> String
obtenerString (Var p)       = p
obtenerString (Not (Var p)) = p
obtenerString _             = ""

-- hacemos interpretacion la clausula con sus valores
darVal :: Clausula -> Interpretacion
darVal [Var p]       = [(p, True)]
darVal [Not (Var p)] = [(p, False)]
darVal _             = []

--  si una variable ya existe dentro de la interpretación ya no la repite
tieneInter :: String -> Interpretacion -> Bool
tieneInter _ [] = False
tieneInter pBuscada ((pModelo, _):xs) = 
    if pBuscada == pModelo 
    then True 
    else tieneInter pBuscada xs

--Buscamos si la literal ya es true dentro de nuestras interpretaciones
evaluaTrue :: Interpretacion -> Literal -> Bool
evaluaTrue [] _ = False
evaluaTrue ((v, b): xs) (Var p)       = if v == p && b == True then True else evaluaTrue xs (Var p)
evaluaTrue ((v,b): xs) (Not (Var p)) = if  v == p && b == False  then True else evaluaTrue xs (Not (Var p))
evaluaTrue _ _ = False

-- Ejercicio 1: conflict
conflict :: Estado -> Bool
conflict (_, clausulas) = hayClausulaVacia clausulas

-- Busacamos clausulas vacias
hayClausulaVacia :: [Clausula] -> Bool
hayClausulaVacia [] = False
hayClausulaVacia ([]:_) = True
hayClausulaVacia (_:xs) = hayClausulaVacia xs

-- Ejercicio 2: success
success :: Estado -> Bool
success (_, []) = True
success _       = False

-- Ejercicio 3: unit
unit :: Estado -> Estado
unit (modelo, clausulas) = unitAux modelo [] clausulas

-- busca las clausaulas unitarias y toma la primera en encontrar
unitAux :: Interpretacion -> [Clausula] -> [Clausula] -> Estado
unitAux modelo revisadas [] = (modelo, revisadas) -- no hay clausula unit
unitAux modelo revisadas (c:xs) =
    if esUnit c
    then
        let lit = obtenerLiteral c
            nombre = obtenerString lit
        in if tieneInter nombre modelo
            --revisa que no estuviera dentro de la interpretacion
           then unitAux modelo (revisadas ++ [c]) xs
           -- Si no tiene interpretación 
           else (darVal c ++ modelo, revisadas ++ xs)
    else
        -- No es unitaria, se acumulamos y sigue
        unitAux modelo (revisadas ++ [c]) xs

-- Ejercicio 4: elim
elim :: Estado -> Estado
elim (modelo, clausulas) = (modelo, filtrarElim modelo clausulas)

-- Auxiliar para elim: conserva solo las cláusulas que no tienen la litertal verdadera
filtrarElim :: Interpretacion -> [Clausula] -> [Clausula]
filtrarElim _ [] = []
filtrarElim modelo (c:xs) =
    if tieneLiteralVerdadera modelo c
    then filtrarElim modelo xs        -- La eliminamos
    else c : filtrarElim modelo xs    -- La conservamos

-- Auxiliar para revisar si alguna literal de la cláusula es verdadera en el modelo
tieneLiteralVerdadera :: Interpretacion -> Clausula -> Bool
tieneLiteralVerdadera _ [] = False
tieneLiteralVerdadera modelo (l:ls) =
    if evaluaTrue modelo l
    then True
    else tieneLiteralVerdadera modelo ls

--Ejercicio 5
red :: Estado -> Estado
red (modelo, clausulas) = (modelo, mapearReducir modelo clausulas)

mapearReducir :: Interpretacion -> [Clausula] -> [Clausula]
mapearReducir _ [] = []
mapearReducir modelo (c:cs) = reducirClausula modelo c : mapearReducir modelo cs

reducirClausula :: Interpretacion -> Clausula -> Clausula
reducirClausula _ [] = []
reducirClausula modelo (l:ls)
    | esFalsa modelo l = reducirClausula modelo ls
    | otherwise        = l : reducirClausula modelo ls

esFalsa :: Interpretacion -> Literal -> Bool
esFalsa [] _ = False
esFalsa ((v,b):xs) (Var p)
    | v == p = b == False
    | otherwise = esFalsa xs (Var p)
esFalsa ((v,b):xs) (Not (Var p))
    | v == p = b == True
    | otherwise = esFalsa xs (Not (Var p))
esFalsa _ _ = False

--Ejercicio 6
sep :: Literal -> Estado -> (Estado, Estado)
sep lit (modelo, clausulas) =
    let nombre = obtenerString lit
        estadoTrue  = ((nombre, True) : modelo, clausulas)
        estadoFalse = ((nombre, False) : modelo, clausulas)
    in (estadoTrue, estadoFalse)


--IMPLEMENTACION PARTE 2

-- Auxiliares de recursión para heuristicsLiteral (reemplaza nub, length, filter, etc.)
aplanarLits :: [Clausula] -> [Literal]
aplanarLits [] = []
aplanarLits (c:cs) = c ++ aplanarLits cs

contarApariciones :: Literal -> [Literal] -> Int
contarApariciones _ [] = 0
contarApariciones l (x:xs) = if l == x then 1 + contarApariciones l xs else contarApariciones l xs

buscarLiteralMasFrecuente :: [Literal] -> [Literal] -> Literal -> Int -> Literal
buscarLiteralMasFrecuente [] _ mejorLit _ = mejorLit
buscarLiteralMasFrecuente (x:xs) todos mejorLit maxCuentas =
    let cuentasActual = contarApariciones x todos
    in if cuentasActual > maxCuentas
       then buscarLiteralMasFrecuente xs todos x cuentasActual
       else buscarLiteralMasFrecuente xs todos mejorLit maxCuentas

-- 7. heuristicsLiteral: literal más frecuente en las cláusulas
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral clausulas =
    let todasLits = aplanarLits clausulas
    in case todasLits of
         [] -> Cons False 
         (l:_) -> buscarLiteralMasFrecuente todasLits todasLits l 0


-- 8. Conversión a FNC (para dpll2)

-- Elimina implicaciones y bicondicionales
elimImpl :: Prop -> Prop
elimImpl (Impl p q)  = Or (Not (elimImpl p)) (elimImpl q)
elimImpl (Syss p q)  = And (elimImpl (Impl p q)) (elimImpl (Impl q p))
elimImpl (And p q)   = And (elimImpl p) (elimImpl q)
elimImpl (Or  p q)   = Or  (elimImpl p) (elimImpl q)
elimImpl (Not p)     = Not (elimImpl p)
elimImpl p           = p

-- Mueve las negaciones hacia adentro (NNF)
toNNF :: Prop -> Prop
toNNF (Not (Not p))    = toNNF p
toNNF (Not (And p q))  = Or  (toNNF (Not p)) (toNNF (Not q))
toNNF (Not (Or  p q))  = And (toNNF (Not p)) (toNNF (Not q))
toNNF (And p q)        = And (toNNF p) (toNNF q)
toNNF (Or  p q)        = Or  (toNNF p) (toNNF q)
toNNF (Not p)          = Not (toNNF p)
toNNF p                = p

-- Distribuye OR sobre AND para obtener FNC
toCNFProp :: Prop -> Prop
toCNFProp (And p q) = And (toCNFProp p) (toCNFProp q)
toCNFProp (Or  p q) =
  let p' = toCNFProp p
      q' = toCNFProp q
  in distribute p' q'
toCNFProp p = p

distribute :: Prop -> Prop -> Prop
distribute (And p1 p2) q = And (distribute p1 q) (distribute p2 q)
distribute p (And q1 q2) = And (distribute p q1) (distribute p q2)
distribute p q           = Or p q

-- De árbol Prop en FNC a lista de cláusulas
propToClauseList :: Prop -> [Clausula]
propToClauseList (And p q) = propToClauseList p ++ propToClauseList q
propToClauseList p         = [clauseFromOr p]

clauseFromOr :: Prop -> Clausula
clauseFromOr (Or p q) = clauseFromOr p ++ clauseFromOr q
clauseFromOr p        = [p]

-- Pipeline completo: fórmula -> cláusulas
toClauses :: Prop -> [Clausula]
toClauses prop = propToClauseList (toCNFProp (toNNF (elimImpl prop)))

-- 9. dpll: algoritmo DPLL sobre estado (modelo, cláusulas)

-- Aplica unit, elim y red sucesivamente hasta estabilizar el estado
aplicarReglas :: Estado -> Estado
aplicarReglas estado =
    let e1 = unit estado
        e2 = elim e1
        e3 = red e2
    in if e3 == estado
       then estado
       else aplicarReglas e3

-- Genera el ArbolDPLL ejecutando transiciones
crearArbol :: Estado -> ArbolDPLL
crearArbol estado =
    let estadoActual = aplicarReglas estado
    in if conflict estadoActual then Node estadoActual Void
       else if success estadoActual then Node estadoActual Void
       else 
           let lit = heuristicsLiteral (snd estadoActual)
               (eTrue, eFalse) = sep lit estadoActual
           in Branch estadoActual (crearArbol eTrue) (crearArbol eFalse)

-- Recorre el ArbolDPLL buscando el primer modelo exitoso (backtracking)
buscarModelo :: ArbolDPLL -> Interpretacion
buscarModelo Void = []
buscarModelo (Node estado arbol) =
    if success estado then fst estado
    else if conflict estado then []
    else buscarModelo arbol
buscarModelo (Branch estado izq der) =
    let resIzq = buscarModelo izq
    in if null resIzq 
       then buscarModelo der
       else resIzq

-- Ejecución principal
dpll :: [Clausula] -> Interpretacion
dpll clausulas = buscarModelo (crearArbol ([], clausulas))

-- (Opcional, punto extra) Implementar dpll2
dpll2 :: Prop -> Interpretacion
dpll2 prop = dpll (toClauses prop)