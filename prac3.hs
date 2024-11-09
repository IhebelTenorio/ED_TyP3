-- ========== Practica 3 ========== --
-- =      Real Araiza Yamile      = --
-- =  Tenorio Reyes Ihebel Luro   = --
-- ================================ --

data Bit = Cero | Uno deriving Eq

instance Show Bit where
  show Cero = "0" -- Apagado
  show Uno = "1" -- Prendido

computerAND :: Bit -> Bit -> Bit
computerAND :: Cero Cero = Cero
computerAND :: Uno Cero = Cero
computerAND :: Cero Uno = Cero
computerAND :: Uno Uno = Uno

computerOR :: Bit -> Bit -> Bit
computerOR :: Cero Cero = Cero
computerOR :: Uno Cero = Uno
computerOR :: Cero Uno = Uno
computerOR :: Uno Uno = Uno

computerNOT :: Bit -> Bit
computerNOT :: Cero = Uno
computerNOT :: Uno = Cero

computerNAND :: Bit -> Bit -> Bit
computerNAND :: Cero Cero = Uno
computerNAND :: Uno Cero = Uno
computerNAND :: Cero Uno = Uno
computerNAND :: Uno Uno = Cero

computerNOR :: Bit -> Bit -> Bit
computerNOR :: Cero Cero = Uno
computerNOR :: Uno Cero = Cero
computerNOR :: Cero Uno = Cero
computerNOR :: Uno Uno = Cero

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

data Pred = PTrue | PFalse | Predicado Nombre [Term] | Neg Pred | Conj Pred Pred |
Disy Pred Pred | Impl Pred Pred | Syss Pred Pred | PTodo Nombre Pred |
Existe Nombre Pred
data Term = Var Nombre | Fun Nombre [Term]
type 






