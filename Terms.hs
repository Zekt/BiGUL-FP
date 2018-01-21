module Terms where

type Name = String

data Val = N Int
         | B Bool
         | P (Val, Val)
         | L ([] Val)
         | F (Val -> Val)

{--
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

data Term = Lit Val
          | Var Name
          | Pair Term Term
          | Cons Term Term
          | Lam Name Type Term
          | App Term Term
          | If Term Term Term
          | Put FBiGUL Term Term
          | Get FBiGUL Term

data FBiGUL = Clause Term Term Term

type Env = Name -> Val

extend :: Name -> Val -> Env -> Env
extend x v env y = if x == y then v
                             else env y

type Cxt = Name -> Type
extendCxt :: Name -> Type -> Cxt -> Cxt
extendCxt x t cxt y = if x == y then t
                                else cxt y

{--
    typeof :: Val a -> Type
typeof (N _) = TNum
typeof (B _) = TBool
typeof (P (v1, v2)) = TPair (typeof v1) (typeof v2)
typeof (L ) = TList t
typeof (F f) = undefined

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
