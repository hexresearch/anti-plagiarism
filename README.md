Техническое задание
===================

Интерфейс функции
-----------------

Необходимо реализовать функцию генерации плагиата с интерфейсом
```haskell
plagiarism :: Options -> [Text] -> (Text, [Int])
```
которая составляет текст из частей корпуса текстов, переданного в качестве второго аргумента.
Также функция возвращает список индексов текстов, из которых был взят плагиат.

Сам процесс генерации полностью случаен, но его правила задаются первым аргументом функции, который включает в себя следующие параметры:

1. Функция, разбивающая текст на параграфы, с интерфейсом `Text -> [Text]`. Объединяются параграфы символом переноса строки.
1. Функция, разбивающая текст внутри одного параграфа на лексические единицы (это могут быть предложения, слова, предложения без знаков препинания, более сложные конструкции), имеющая интерфейс `Text -> [Text]`. Объединяются лексические части пробелом.
1. Алгебраический тип данных, значение которого определяет правила, производимые над текстом. Возможные варианты:
    - разбиение на параграфы и порядок лексических элементов в них не меняются (то есть, допускается только отбрасывание лексических элементов или целых параграфов);
    - допускается перемешивание лексических элементов внутри одного параграфа;
    - допускается объединение параграфов и перемешивание лексических элементов (например, вклинивание предложения из одного параграфа внутрь другого).

&laquo;Не плагиат&raquo; генерируется в варианте &laquo;допускается объединение параграфов и перемешивание лексических элементов&raquo;, когда лексические элементы &mdash; слова.

Тестирование
------------

Написать тесты для генератора плагиата, выводящие в качестве оценки коэффициент Жаккара:
```haskell
kJaccard :: (Ord a, Foldable f) => f a -> f a -> Double
kJaccard a' b' = ilen / ulen
  where ilen = fromIntegral $ S.size (a `S.intersection` b)
        ulen = fromIntegral $ S.size (a `S.union` b)
        a = S.fromList (foldMap return a')
        b = S.fromList (foldMap return b')
```
Корпус текстов берётся из [Яндекс.Рефераты](https://yandex.ru/referats/).
