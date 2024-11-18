-- ========== Practica 3 ========== --
-- =      Real Araiza Yamile      = --
-- =  Tenorio Reyes Ihebel Luro   = --
-- ================================ --

import Data.List -- Puse esto para usar el operador \\

data Bit = Cero | Uno deriving Eq

instance Show Bit where
  show Cero = "0" -- Apagado
  show Uno = "1" -- Prendido

computerAND :: Bit -> Bit -> Bit
computerAND Cero Cero = Cero
computerAND Uno Cero = Cero
computerAND Cero Uno = Cero
computerAND Uno Uno = Uno

computerOR :: Bit -> Bit -> Bit
computerOR Cero Cero = Cero
computerOR Uno Cero = Uno
computerOR Cero Uno = Uno
computerOR Uno Uno = Uno

computerNOT :: Bit -> Bit
computerNOT Cero = Uno
computerNOT Uno = Cero

computerNAND :: Bit -> Bit -> Bit
computerNAND Cero Cero = Uno
computerNAND Uno Cero = Uno
computerNAND Cero Uno = Uno
computerNAND Uno Uno = Cero

computerNOR :: Bit -> Bit -> Bit
computerNOR Cero Cero = Uno
computerNOR Uno Cero = Cero
computerNOR Cero Uno = Cero
computerNOR Uno Uno = Cero

computerXOR :: Bit -> Bit -> Bit
computerXOR a b = if a == b then Cero else Uno

computerXNOR :: Bit -> Bit -> Bit
computerXNOR a b = if a == b then Uno else Cero

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder a b = ( computerXOR a b , computerAND a b)

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder x y z = (computerXOR (computerXOR x y) z, computerOR (computerAND x y) (computerAND (computerXOR x y) z))

flipFlopRS :: Bit -> Bit -> Bit -> Bit
flipFlopRS q r s = computerOR s (computerAND (computerNOT r) q)

flipFlopD :: Bit -> Bit -> Bit
flipFlopD _ d = d

flipFlopT :: Bit -> Bit -> Bit
flipFlopT q t = if t == Uno then toggle q else q
  where
    toggle Cero = Uno
    toggle Uno = Cero

-- == LOGICA PRIMER ORDEN == --
instance Show Pred where
  show PTrue = "True"                                       -- T
  show PFalse = "False"                                     -- F
  show (Predicado n t) = n ++ "(" ++ show t ++ ")"          -- P|Q|R 
  show (Neg p) = "¬" ++ show p                              -- ¬(P)
  show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")" -- (P ∧ Q)
  show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")" -- (P ∨ Q)
  show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")" -- (P → Q)
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")" -- (P ↔ Q)
  show (PTodo n p) = "∀" ++ n ++ " " ++ show p              -- ∀n (P)
  show (Existe n p) = "∃" ++ n ++ " " ++ show p             -- ∃n (P)

instance Show Term where
  show (Var n) = n
  show (Fun f l) = f ++ "(" ++ show l ++ ")"

data Term = Var Nombre | Fun Nombre [Term] deriving Eq

type Nombre = String

data Pred = PTrue | PFalse | Predicado Nombre [Term] | Neg Pred | Conj Pred Pred | Disy Pred Pred | Impl Pred Pred | Syss Pred Pred | PTodo Nombre Pred | Existe Nombre Pred deriving Eq

varsTerm :: Term -> [Nombre]
varsTerm (Var x) = [x]
varsTerm (Fun x y) = concatMap varsTerm y

-- 1. Variables
variables :: Pred -> [Nombre]
variables PTrue = []
variables PFalse = []
variables (Predicado x y) = concatMap varsTerm y
variables (Neg x) = variables x
variables (Conj x y) = (variables x) ++ (variables y)
variables (Disy x y) = (variables x) ++ (variables y)
variables (Impl x y) = (variables x) ++ (variables y)
variables (Syss x y) = (variables x) ++ (variables y)
variables (PTodo x y) = variables y
variables (Existe x y) = variables y

-- 2. Variables libres
variablesLibres :: Pred -> [Nombre]
variablesLibres p = variablesLibresAux [] p

-- Función auxiliar que mantiene un registro de las variables ligadas
variablesLibresAux :: [Nombre] -> Pred -> [Nombre]
variablesLibresAux bound PTrue = []
variablesLibresAux bound PFalse = []
variablesLibresAux bound (Predicado _ terms) = filter (`notElem` bound) (concatMap varsTerm terms)
variablesLibresAux bound (Neg p) = variablesLibresAux bound p
variablesLibresAux bound (Conj p1 p2) = variablesLibresAux bound p1 ++ variablesLibresAux bound p2
variablesLibresAux bound (Disy p1 p2) = variablesLibresAux bound p1 ++ variablesLibresAux bound p2
variablesLibresAux bound (Impl p1 p2) = variablesLibresAux bound p1 ++ variablesLibresAux bound p2
variablesLibresAux bound (Syss p1 p2) = variablesLibresAux bound p1 ++ variablesLibresAux bound p2
variablesLibresAux bound (PTodo n p) = variablesLibresAux (n : bound) p
variablesLibresAux bound (Existe n p) = variablesLibresAux (n : bound) p

-- 3. Variables Ligadas
variablesLigadas :: Pred -> [Nombre]
variablesLigadas x = variables x \\ variablesLibres x

-- 4. Variables Leyes de Morgan x
equivalengiaLogica :: Pred -> Pred
equivalengiaLogica (Neg (Existe a (Conj x y))) = (PTodo a (Disy (equivalengiaLogica x) (equivalengiaLogica y)))
equivalengiaLogica (Neg (PTodo a (Disy x y))) = (Existe a (Conj (equivalengiaLogica x) (equivalengiaLogica y)))

-- 5. Cuantificadores
cuantificadores :: Pred -> Integer
cuantificadores PTrue = 0
cuantificadores PFalse = 0
cuantificadores (Predicado x y) = 0
cuantificadores (Neg x) = cuantificadores x
cuantificadores (Conj x y) = (cuantificadores x) + (cuantificadores y)
cuantificadores (Disy x y) = (cuantificadores x) + (cuantificadores y)
cuantificadores (Impl x y) = (cuantificadores x) + (cuantificadores y)
cuantificadores (Syss x y) = (cuantificadores x) + (cuantificadores y)
cuantificadores (PTodo x y) = cuantificadores y + 1
cuantificadores (Existe x y) = cuantificadores y + 1

-- Suponiendo que los cuantificadores solo abren una vez cada variable podemos contar el numero de variables ligadas para obtener el numero de cuantificadores
cuantifs :: Pred -> Integer
cuantifs x = cuantifsAux (variablesLigadas x)

cuantifsAux :: [Nombre] -> Integer
cuantifsAux [] = 0
cuantifsAux (_:xs) = 1 + cuantifsAux xs

-- 6. Define una funcion que cuente el numero de conectivos logicos en un predicado.
conectivos :: Pred -> Integer
conectivos PTrue = 0
conectivos PFalse = 0
conectivos (Predicado _ _) = 0
conectivos (Neg p) = 1 + conectivos p
conectivos (Conj x y) = 1 + conectivos x + conectivos y
conectivos (Disy x y) = 1 + conectivos x + conectivos y
conectivos (Impl x y) = 1 + conectivos x + conectivos y
conectivos (Syss x y) = 1 + conectivos x + conectivos y
conectivos (PTodo _ p) = conectivos p
conectivos (Existe _ p) = conectivos p

--                                 __
--                     .--.      .'  `.
--                   .' . :\    /   :  L
--                   F     :\  /   . : |        .-._
--                  /     :  \/        J      .' ___\
--                 J     :   /      : : L    /--'   ``.
--                 F      : J           |  .<'.o.  `-'>
--                /        J             L \_>.   .--w)
--               J        /              \_/|   . `-__|
--               F                        / `    -' /|)
--              |   :                    J   '        |
--             .'   ':                   |    .    :  \
--            /                          J      :     |L
--           F                              |     \   ||
--          F .                             |   :      |
--         F  |                             ; .   :  : F
--        /   |                                     : J
--       J    J             )                ;        F
--       |     L           /      .:'                J
--    .-'F:     L        ./       :: :       .       F
--    `-'F:     .\    `:.J         :::.             J
--      J       ::\    `:|        |::::\            |
--      J        |:`.    J        :`:::\            F
--       L   :':/ \ `-`.  \       : `:::|        .-'
--       |     /   L    >--\         :::|`.    .-'
--       J    J    |    |   L     .  :::: :`, /
--        L   F    J    )   |        >::   : /
--        |  J      L   F   \     .-.:'   . /
--        ): |     J   /     `-   | |   .--'
--        /  |     |: J        L  J J   )
--        L  |     |: |        L   F|   /
--        \: J     \:  L       \  /  L |
--         L |      \  |        F|   | )
--         J F       \ J       J |   |J
--          L|        \ \      | |   | L
--          J L        \ \     F \   F |
--           L\         \ \   J   | J   L
--          /__\_________)_`._)_  |_/   \_____
--                              ""   `"""


