-- | Домашка 2. Параметрический полиморфизм: уровень 3.
module Level3 where

import GHC.Exts (RuntimeRep, TYPE)
import MetaUtils (todo)


-- 3.1. Тип error
--
-- Как на самом деле выглядит тип функции error? Дайте error' такую же сигнатуру
-- (без HasCallStack) и реализуйте её через error. В комментарии объясните, почему такая
-- сигнатура не нарушает запрет на связыватели, полиморфные по представлению, и зачем
-- error вообще полиморфна по представлению.

error' :: forall (r :: RuntimeRep). forall (a :: TYPE r). String -> a
error' = error

-- Тип в GHC:
--  error :: forall (r :: RuntimeRep). forall (a :: TYPE r). HasCallStack => [Char] -> a
--  data Levity = Lifted | Unlifted
--  data RuntimeRep = BoxedRep Levity | IntRep | DoubleRep | TupleRep [RuntimeRep] | ...
--  TYPE :: RuntimeRep -> Type
--  type Type = TYPE ('BoxedRep 'Lifted)
--
-- Почему не нарушает запрет:
--  RuntimeRep -- как именно в памяти представлены значения типа. При стирании
--  типов остаётся String::Type, т.е. нам известно представление этих значений,
--  а как будет представлено значение типа `a` не важно, так как error всегда
--  кидает исключение (raise#, никакой переменной в памяти не будет).
--  Тоже самое с undefined
--
-- Зачем полиморфна:
--  a :: Type -- не достаточно для всех сценариев, так как не все типы имеют кайнд
--  Type. Пример, который не будет работать при a::Type (так как Int# :: TYPE IntRep):
--  f :: Int# -> Int#
--  f _ = error "err" -- ошибка, так как кайнды Int# :: TYPE IntRep и a :: Type несовпадают
