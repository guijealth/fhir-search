# Idea de los módulos
Con el cambio de estructura del proyecto, propongo la idea de los módulos para facilitar todo. Un módulo está compuesto por una capa o varias capas anidadas. El formato de estas capas es el siguiente:
```clj
;; Layer structure
{:name "name"
 :type "type"
 :compartment {}
 :join (or :fhir.search.join/and :fhir.search.join/and)
 :chain (or true nil)
 :composite (or true nil)
 :modifier :fhir.search.modifier/<modifier>
 :prefix :fhir.search.modifier/<prefix>
 :value "value"
 :params []
}
```
Esta estructura se anidará dentro de ella misma asociándose al *keyword* **:params**. Excepto **:compartment**, que solo está presente en la capa más superficial de la anidación *(main layer)*, todos los demás elementos del mapa se repiten. No hay que preocuparse por los elementos nulos, pues serán borrados luego con `(clean`.

## Anidación secuencial
Se deben construir las diferentes capas que conformen el módulo principal y luego almacenarlos en una colección para aplicar anidación secuencial donde el sucesor esté dentro del antecesor y así sucesivamente. Creo que es la mejor forma. Para esto he creado esta *fn* que se encargará de actualizar el keyword especificado con un valor dado.
```clj
(defn update-specific-val [coll key origin-val new-val]
  (postwalk (fn [x]
              (if (and (map? x)
                       (contains? x key)
                       (= origin-val (key x)))
                (assoc x key new-val)
                x))
            coll))
```
Si le damos en todas las capas de anidación el valor de *nil* a **:params**, se podrían anidar secuencialmente todas las capas de la forma anteriormente descrita.

## Procedimiento para crear el map tree
Se crea la capa principal, que es la más superficial a niveles de anidación, y en ella se anidan los módulos referentes a los parámetros, agrupando a estos en una colección. La capa principal es diferente dependiendo de los parámetros que contiene.

### Pasos
1. Se crea la *path-layer* que será la capa más superficial.
2. Se crean la *param-layer* y la *value-layer*. Esta última se anida dentro de la primera.
3. Se repite el procedimiento dependiendo de la cantidad de parámetros que halla.
4. se anidan todos los módulos en la *path-layer* que es la capa principal.

