### Ejercicio 1 — Instancia de `Show`

En este ejercicio se implementamos una instancia de la clase `Show`
para el tipo de datos `EAB`.

El tipo `EAB` representa expresiones aritméticas y booleanas mediante
diferentes constructores, entre ellos:

- `Num`
- `Var`
- `Bool`
- `Suma`
- `Prod`
- `Suc`
- `Pred`
- `Not`
- `If`
- `IsZero`
- `Lt`
- `Gt`
- `Eq`
- `Let`

La implementación de `Show` permite obtener una representación textual
de las expresiones `EAB`.

Implementar:

```haskell
instance Show EAB where