--def de indentificadores
type ID = String

--def de datos EAB
data EAB =
      Num Int
    | Var ID
    | Bool Bool
    | Suma EAB EAB
    | Prod EAB EAB
    | Suc EAB
    | Pred EAB
    | Not EAB
    | If EAB EAB EAB
    | IsZero EAB
    | Lt EAB EAB
    | Gt EAB EAB
    | Eq EAB EAB
    | Let ID EAB EAB
    deriving (Eq)

--def ambiente
type Env = [(ID, EAB)]

--instancia de show para EAB
instance Show EAB where

    -- num
    show (Num n) = show n

    --bool
    show (Bool True)  = "true"
    show (Bool False) = "false"

    --var
    show (Var x) = x

    --sum
    show (Suma e1 e2) =
        "(" ++ show e1 ++ "+" ++ show e2 ++ ")"

    --prod
    show (Prod e1 e2) =
        "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

    --suc
    show (Suc e) =
        "suc(" ++ show e ++ ")"

    --pred
    show (Pred e) =
        "pred(" ++ show e ++ ")"

    --negacion
    show (Not e) =
        "not (" ++ show e ++ ")"

    --condicional
    show (If e1 e2 e3) =
        "if " ++ show e1
        ++ " then " ++ show e2
        ++ " else " ++ show e3

    --IsZero
    show (IsZero e) =
        "iszero(" ++ show e ++ ")"

    --menor que
    show (Lt e1 e2) =
        "(" ++ show e1 ++ "<" ++ show e2 ++ ")"

    --mayor que
    show (Gt e1 e2) =
        "(" ++ show e1 ++ ">" ++ show e2 ++ ")"

    --igualdad
    show (Eq e1 e2) =
        "(" ++ show e1 ++ "==" ++ show e2 ++ ")"

    --let
    show (Let x e1 e2) =
        "let " ++ x
        ++ " = " ++ show e1
        ++ " in " ++ show e2


--Define la función evalEnv env e que devuelve el valor resultante de evaluar la expresión EAB dada con el ambiente env.
evalEnv :: Env -> EAB -> Either Int Bool

--num
evalEnv env (Num n) = Left n

--bool
evalEnv env (Bool b) = Right b

--var
evalEnv env (Var x) =
    case lookup x env of
        Just valor -> evalEnv env valor
        Nothing -> error ("Error: Variable no definida: " ++ x)

--sum
evalEnv env (Suma e1 e2) =
    case (evalEnv env e1, evalEnv env e2) of
        (Left n1, Left n2) -> Left (n1 + n2)
        _ -> error "Error: Se esperaban numeros en la suma"

--prod
evalEnv env (Prod e1 e2) =
    case (evalEnv env e1, evalEnv env e2) of
        (Left n1, Left n2) -> Left (n1 * n2)
        _ -> error "Error: Se esperaban numeros en el producto"

--suc
evalEnv env (Suc e) =
    case evalEnv env e of
        Left n -> Left (n + 1)
        Right _ -> error "Error: Suc esperaba un numero"

--pred
evalEnv env (Pred e) =
    case evalEnv env e of
        Left n -> Left (n - 1)
        Right _ -> error "Error: Pred esperaba un numero"

--negacion
evalEnv env (Not e) =
    case evalEnv env e of
        Right b -> Right (not b)
        Left _ -> error "Error: Not esperaba un booleano"

--condicional
evalEnv env (If e1 e2 e3) =
    case evalEnv env e1 of
        Right True  -> evalEnv env e2
        Right False -> evalEnv env e3
        Left _ -> error "Error: La condicion del If debe ser booleana"

--iszero
evalEnv env (IsZero e) =
    case evalEnv env e of
        Left n -> Right (n == 0)
        Right _ -> error "Error: IsZero esperaba un numero"

--menor que
evalEnv env (Lt e1 e2) =
    case (evalEnv env e1, evalEnv env e2) of
        (Left n1, Left n2) -> Right (n1 < n2)
        _ -> error "Error: Lt esperaba dos numeros"

--mayor que
evalEnv env (Gt e1 e2) =
    case (evalEnv env e1, evalEnv env e2) of
        (Left n1, Left n2) -> Right (n1 > n2)
        _ -> error "Error: Gt esperaba dos numeros"

--igual
evalEnv env (Eq e1 e2) =
    case (evalEnv env e1, evalEnv env e2) of
        (Left n1, Left n2) -> Right (n1 == n2)
        (Right b1, Right b2) -> Right (b1 == b2)
        _ -> error "Error: Los tipos de Eq deben coincidir"

--let
evalEnv env (Let x e1 e2) =
    case evalEnv env e1 of
        Left n ->
            evalEnv ((x, Num n) : env) e2

        Right b ->
            evalEnv ((x, Bool b) : env) e2


--verifica si una variable aparece libre en una expresion
esLibre :: ID -> EAB -> Bool

--num
esLibre x (Num n) = False

--bool
esLibre x (Bool b) = False

--var
esLibre x (Var y) =
    x == y

--sum
esLibre x (Suma e1 e2) =
    esLibre x e1 || esLibre x e2

--prod
esLibre x (Prod e1 e2) =
    esLibre x e1 || esLibre x e2

--suc
esLibre x (Suc e) =
    esLibre x e

--pred
esLibre x (Pred e) =
    esLibre x e

--negacion
esLibre x (Not e) =
    esLibre x e

--condicional
esLibre x (If e1 e2 e3) =
    esLibre x e1 ||
    esLibre x e2 ||
    esLibre x e3

--iszero
esLibre x (IsZero e) =
    esLibre x e

--menor que
esLibre x (Lt e1 e2) =
    esLibre x e1 || esLibre x e2

--mayor que
esLibre x (Gt e1 e2) =
    esLibre x e1 || esLibre x e2

--igual
esLibre x (Eq e1 e2) =
    esLibre x e1 || esLibre x e2

--let
esLibre x (Let y e1 e2) =
    esLibre x e1 ||
    if x == y
        then False
        else esLibre x e2


--sustituye las apariciones libres de una variable por una expresion
sust :: ID -> EAB -> EAB -> EAB

--num
sust x e1 (Num n) = Num n

--bool
sust x e1 (Bool b) = Bool b

--var
sust x e1 (Var y)
    | x == y    = e1
    | otherwise = Var y

--sum
sust x e1 (Suma e2 e3) =
    Suma (sust x e1 e2)
         (sust x e1 e3)

--prod
sust x e1 (Prod e2 e3) =
    Prod (sust x e1 e2)
         (sust x e1 e3)

--suc
sust x e1 (Suc e) =
    Suc (sust x e1 e)

--pred
sust x e1 (Pred e) =
    Pred (sust x e1 e)

--negacion
sust x e1 (Not e) =
    Not (sust x e1 e)

--condicional
sust x e1 (If e2 e3 e4) =
    If (sust x e1 e2)
       (sust x e1 e3)
       (sust x e1 e4)

--iszero
sust x e1 (IsZero e) =
    IsZero (sust x e1 e)

--menor que
sust x e1 (Lt e2 e3) =
    Lt (sust x e1 e2)
       (sust x e1 e3)

--mayor que
sust x e1 (Gt e2 e3) =
    Gt (sust x e1 e2)
       (sust x e1 e3)

--igual
sust x e1 (Eq e2 e3) =
    Eq (sust x e1 e2)
       (sust x e1 e3)

--let
sust x e1 (Let y e2 e3)
    | x == y = Let y (sust x e1 e2) e3
    | esLibre y e1 =
        error "Error: Captura de Variable libre"
    | otherwise =
        Let y
            (sust x e1 e2)
            (sust x e1 e3)


--evalua un solo paso de una expresion EAB
evalStep :: EAB -> EAB

--num
evalStep (Num n) = Num n

--bool
evalStep (Bool b) = Bool b

--suma de dos numeros
evalStep (Suma (Num n1) (Num n2)) =
    Num (n1 + n2)

--evalua primero el lado izquierdo
evalStep (Suma e1 e2) =
    Suma (evalStep e1) e2
    