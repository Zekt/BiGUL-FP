module Terms where

type Name typ = String

data Val a where
    N :: Int -> Val Int
    B :: Bool -> Val Bool
    P :: Val a -> Val b -> Val (a, b)
    L :: [Val a] -> Val [a]
    F :: (Val a -> Val b) -> Val (a -> b)

{--
data Val = N Int
         | B Bool
         | P (Val, Val)
         | L ([] Val) Type
         | F (Val -> Val)

Lcon :: [Val] -> Type -> Val
Lcon s t = if typeList s t then L s t
                           else error "List type error"

typeList :: [Val] -> Type -> Bool
typelist x:xs t = if typeof x != t then False
                                   else typeList xs t
typeList [] _ = True
--}

data Type = TBool
          | TNum
          | TPair Type Type
          | TList Type
          | TFun Type Type
          deriving Eq

data Term a where
    Lit :: Val a -> Term a
    Var :: Name a -> Term a
    Pair :: Term a -> Term b -> Term (a, b)
    Cons :: Term a -> Term [a] -> Term [a]
    Lam :: Name -> Type -> Term a -> Term a
    App :: Term a -> Term b -> Term a
    If :: Term a -> Term b -> Term b -> Term b
    Put :: FBiGUL -> Term a -> Term b -> Term FBiGUL
    Get :: FBiGUL -> Term a -> Term FBiGUL

{--
data Term a = Lit (Val a)
            | Var Name
            | Pair Term Term
            | Cons Term Term
            | Lam Name Type Term
            | App Term Term
            | If Term Term Term
            | Put FBiGUL Term Term
            | Get FBiGUL Term
            --}

data FBiGUL = Clause Term Term Term

type Env t = Name t -> Val t

extend :: Name -> Val a -> Env a -> Env a
extend x v env y = if x == y then v
                             else env y

type Cxt = Name -> Type
extendCxt :: Name -> Type -> Cxt -> Cxt
extendCxt x t cxt y = if x == y then t
                                else cxt y

typeof :: Val a -> Type
typeof (N _) = TNum
typeof (B _) = TBool
typeof (P (v1, v2)) = TPair (typeof v1) (typeof v2)
typeof (L ) = TList t
typeof (F f) = undefined
{--
typeinf :: Term -> Cxt -> Type
typeinf (Lit (N _)) cxt = TNum
typeinf (Lit (B _)) cxt = TBool
typeinf (Lit (P (v1, v2))) cxt = TPair (typeof v1) (typeof v2)
typeinf (Lit (L s t)) cxt = t
typeinf (Var n) cxt = cxt n
typeinf (Pair t1 t2) cxt = TPair (typeinf v1 cxt) (typeinf v2 cxt)
typeinf (Cons th tt) cxt = if typeinf th cxt == typeinf tt cxt then 
typeinf (Lam th tt) cxt = if typeinf th cxt == typeinf tt cxt then 
typeinf (App n) cxt = cxt n
typeinf (If n) cxt = cxt n
--}
