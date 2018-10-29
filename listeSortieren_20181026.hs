
minimum' :: Ord a => [a] -> a
minimum' [] = error "Leere Liste"
minimum' [a] = a
minimum' x = miniHelp x (head x)
  where miniHelp [] a = a
        miniHelp [x] a = min x a
        miniHelp (x:xs) a = min3 x (miniHelp xs a) a
        min3 a b c = min a (min b c)



-- Liste bis zu erstem Auftreten von wert zurückgeben (wert wird nicht zurückgegeben)
listeBis liste wert = listeBisHelp [] liste wert
  where listeBisHelp ausgabe [] wert = error "Wert nicht gefunden"
        listeBisHelp ausgabe liste wert = if (head liste) == wert
          then ausgabe
          else listeBisHelp (ausgabe ++ [head liste]) (tail liste) wert


-- Liste ab dem erstem Auftreten von wert zurückgeben (wert wird nicht zurückgegeben)
listeAb [x] wert = if x == wert
  then []
  else error "Wert nicht in Liste"
listeAb liste wert = if head liste == wert
  then tail liste
  else listeAb (tail liste) wert

removeFromList :: Eq t => [t] -> t -> [t]
removeFromList liste wert = (listeBis liste wert) ++ (listeAb liste wert)
 
-- Liste sortieren
sort :: Ord a => [a] -> [a]
sort liste = sortHelp [] liste
  where sortHelp :: Ord a => [a] -> [a] -> [a]
        sortHelp sorted [] = sorted
        sortHelp sorted unsorted = sortHelp (addMax (maximum unsorted) sorted) (remMax unsorted)
        addMax wert liste = [wert] ++ liste
        remMax  liste = removeFromList liste (maximum liste)
        

